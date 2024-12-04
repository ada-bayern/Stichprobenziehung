library(shiny)
library(DT)
library(dplyr)

# TODO: explanation
# TODO: init with proportional values
# TODO: actual sampling

source("modules/utils.R")

# Define the UI for the stratified sampling module (in German, using snake_case)
sample_ui <- function(id) {
  ns <- NS(id) # Namespace for the module
  titlePanel("Stichprobenziehung")
  sidebarLayout(
    sidebarPanel(
      # Sample size input
      numericInput(ns("sample_size"),
                   "Gesamtstichprobengröße",
                   value = 100,
                   min = 1,
                   max = 9999),

      # Toggle between sampling strategies
      radioButtons(
        ns("sampling_type"),
        "Stichprobenstrategie",
        choices = c("Garantierte Gesamtstichprobengröße" = "total_size",
                    "Garantierte Mindestgröße pro Kategorie" = "category_size"),
        selected = "total_size"
      ),

      # Dynamic UI for probability selection
      tabsetPanel(ns("select_prob")),

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
    sampled_data <- reactiveVal()

    # Reactive value to store current probabilities for sliders
    slider_probs <- reactiveValues()

    # Render probability selection UI dynamically
    observeEvent(input$sample_size, {
      req(strat_layers())
      sample_size <- input$sample_size

      # Clear any existing tabs
      removeUI(selector = paste0("#", ns("select_prob"), " .tab-pane"),
               multiple = TRUE)

      for (layer in strat_layers()) {
        # get counts for each category
        cat_counts <- table(layer$col)

        # Create a new tab for each layer
        new_tab <- tabPanel(
          title = layer$name,
          value = layer$id,
          # actionButton(ns(paste0(layer$id, "_select_all")), 
          #              "Alle auswählen"),
          # actionButton(ns(paste0(layer$id, "_proportional")),
          #              "Proportional zur Grundgesamtheit"),
          unname(lapply(layer$uniq_cats, function(cat) {
            sliderInput(
              ns(paste0(layer$id, "_", cat, "_prob")),
              label = paste("Wahrscheinlichkeit für", cat),
              min = 0,
              max = min(sample_size, cat_counts[[cat]]),
              value = min(sample_size, cat_counts[[cat]]),
              step = 1
            )
          }))
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

    # Update sliders when "Alle auswählen" is toggled
    observeEvent(strat_layers(), {
      lapply(strat_layers(), function(layer) {
        observeEvent(input[[paste0(layer$id, "_select_all")]], {
          for (cat in layer$uniq_cats) {
            updateSliderInput(
              session,
              paste0(layer$id, "_", cat, "_prob"),
              value = 1
            )
          }
        })
      })
    })

    # Update sliders when "Proportional zur Grundgesamtheit" is toggled
    observeEvent(strat_layers(), {
      lapply(strat_layers(), function(layer) {
        observeEvent(input[[paste0(layer$id, "_proportional")]], {
          # Calculate category proportions
          cat_counts <- table(layer$col)
          total_count <- length(layer$col)
          cat_probs <- lapply(layer$uniq_cats,
                              function(cat) cat_counts[[cat]] / total_count)

          print(cat_probs)
          # Update sliders with proportional values
          for (cat in layer$uniq_cats) {
            updateSliderInput(
              session,
              paste0(layer$id, "_", cat, "_prob"),
              value = cat_probs[[cat]]
            )
          }
        })
      })
    })

    # Perform sampling when the button is clicked
    observeEvent(input$sample_button, {
      req(presets(), strat_layers())

      data <- presets()
      layers <- strat_layers()

      # Collect probabilities set by the user
      probabilities <- lapply(layers, function(layer) {
        sapply(layer$uniq_cats, function(cat) {
          input[[paste0(layer$id, "_", cat, "_prob")]]
        })
      })

      # TODO: Implement sampling logic based on probabilities
      sampled_data(data)
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
  })
}
