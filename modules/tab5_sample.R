library(shiny)
library(DT)
library(dplyr)

# TODO: explanation
# TODO: actual sampling

source("modules/utils.R")
source("modules/stratification/new.R")
# source("modules/stratification/strata_sizes.R")
# source("modules/stratification/strat_sample.R")

# Define the UI for the stratified sampling module (in German, using snake_case)
sample_ui <- function(id) {
  ns <- NS(id) # Namespace for the module
  titlePanel("Stichprobenziehung")
  sidebarLayout(
    sidebarPanel(
      # Sample size input
      numericInput(ns("sample_size"),
                   "Gesamtstichprobengröße",
                   value = 100, # Placeholder
                   min = 1,
                   max = 9999), # Placeholder

      # Toggle between sampling strategies
      radioButtons(
        ns("sampling_type"),
        "Stichprobenstrategie",
        choices = c("Garantierte Gesamtstichprobengröße" = "total_size",
                    "Garantierte Mindestgröße pro Kategorie" = "category_size"),
        selected = "total_size"
      ),
      hr(),
      # Dynamic UI for probability selection
      tabsetPanel(id = ns("select_prob")),
      hr(),
      # Button to trigger the sampling
      actionButton(ns("sample_button"), "Stichprobe generieren")
    ),

    mainPanel(
      h4("Zusammenfassung der Stichprobe"),
      dataTableOutput(ns("sample_summary"))
    )
  )
}

# Define the server logic for the stratified sampling module
sample_server <- function(id, strat_layers, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the sampled dataset
    sampled_data <- reactiveVal(NULL)

    # Reactive value to store current probabilities for sliders
    slider_probs <- reactiveValues()

    observeEvent(strat_layers(), {
      req(length(strat_layers()) > 0)

      # Update default and limit of sample size input
      layer1 <- strat_layers()[[names(strat_layers())[1]]]
      data_size <- length(layer1$col)
      updateNumericInput(
        session,
        inputId = "sample_size",
        max = data_size,
        value = min(c(100, data_size))
      )
    })

    # Render probability selection UI dynamically
    observeEvent(strat_layers(), {
      sample_size <- input$sample_size
      req(strat_layers(), sample_size > 0)

      for (layer in strat_layers()) {
        # Create a new tab for each layer
        new_tab <- tabPanel(
          title = layer$name,
          value = layer$id,
          br(),
          # choice of selection probability
          radioButtons(ns(paste0(layer$id, "sel_kind")),
            label = "Auswahl gemessen an der ...",
            choices = c(
              "Grundgesamtheit (Diese Kategorie soll ...% der Grundgesamtheit ausmachen)" = "population", # nolint
              "Kategorie (...% dieser Kategorie sollen Teil der Stichprobe sein)" = "ingroup" # nolint
            )
          ),
          hr(),
          # button to reset sliders to proportional
          actionButton(
            ns(paste0(layer$id, "_proportional")),
            "Proportional zum Datensatz"
          ),
          br(), br(),
          # a probability slider for each category
          unname(lapply(names(layer$cat_counts), function(cat) {
            sliderInput(
              ns(paste0(layer$id, "_", cat, "_prob")),
              label = paste("Wahrscheinlichkeit für", cat),
              min = 0,
              max = 1,
              value = layer$cat_counts[[cat]] / length(layer$col),
              step = 0.01
            )
          })),
          uiOutput(ns(paste0(layer$id, "_error")))
        )

        # remove tab if it is already there
        # (to avoid doubles after updating the data)
        removeTab(
          session = session,
          inputId = "select_prob",
          target = layer$id
        )
        # Add the new tab to the UI
        insertTab(
          session = session,
          inputId = "select_prob",
          tab = new_tab,
          select = TRUE
        )
      }
    })

    observe({
      for (layer in strat_layers()) {
        # selection mode: either "population" or "ingroup"
        sel_kind <- input[[paste0(layer$id, "sel_kind")]]

        # Update sliders when "Proportional zur Grundgesamtheit" is toggled
        observeEvent(input[[paste0(layer$id, "_proportional")]], {
          req(sel_kind)
          # Update sliders with proportional values
          for (cat in names(layer$cat_counts)) {
            # calculate default value
            if (sel_kind == "population") {
              # calculate proportion of this category to hole dataset
              value <- layer$cat_counts[[cat]] / length(layer$col)
            } else if (sel_kind == "ingroup") {
              # calculate proportion of the sample to the dataset size
              value <- input$sample_size / length(layer$col)
            }

            updateSliderInput(
              session,
              paste0(layer$id, "_", cat, "_prob"),
              value = value
            )
          }
        })

        output[[paste0(layer$id, "_error")]] <- renderUI({
          total <- 0
          for (cat in names(layer$cat_counts)) {
            total <- total + input[[paste0(layer$id, "_", cat, "_prob")]]
          }
          # if more than 100% are selected in population mode
          if (total > 1 && sel_kind == "population") {
            ui <- fluidRow(column(12,
              hr(),
              span(
                HTML("Die Wahrscheinlichkeiten der einzelnen Kategorien
                      sollten addiert nicht größer als 1 sein. Das kann zu
                      Abweichungen bei der Stichprobenziehung führen."),
                style = "color:red"
              )
            ))
          } else {
            ui <- br()
          }
          ui
        })
      }
    })

    # Perform sampling when the button is clicked
    observeEvent(input$sample_button, {
      req(strat_layers())

      # collect selected proportions
      str_tmp <- lapply(strat_layers(), function(layer) {
        sel_kind <- input[[paste0(layer$id, "sel_kind")]]
        layer$ratios <- lapply(names(layer$cat_counts), function(cat) {
          r <- input[[paste0(layer$id, "_", cat, "_prob")]]
          if (sel_kind == "ingroup") {
            # convert to proportional to population
            r <- (r * layer$cat_counts[[cat]]) / length(layer$col)
          }
          r
        })
        layer
      })
      strat_layers(str_tmp)

      # define strata sizes
      strata <- strata_sizes(strat_layers(), 3, input$sample_size)

      print(strata)

      # sample data
      #sampled_data(strat_sample(data, strata))
    })

    # Render the summary table
    output$sample_summary <- renderDataTable({
      req(sampled_data())
      datatable(
        sampled_data(),
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
    })

    return(list(sample = sampled_data))
  })
}
