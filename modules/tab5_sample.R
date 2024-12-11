library(shiny)
library(DT)

# TODO: explanation
# TODO: LP sampling
# TODO: summary of single categories?

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
      fluidRow(
        column(6, numericInput(ns("sample_size"),
                               "Stichprobengröße",
                               value = 100,
                               min = 1,
                               max = 99999,
                               width = "80px")),
        column(6, align = "center",
               br(),
               textOutput(ns("realized_sample_size")))
      ),
      uiOutput(ns("data_size")),
      hr(),
      # Dynamic UI for probability selection
      tabsetPanel(id = ns("select_prob")),
      hr(),
      # Button to trigger the sampling
      actionButton(ns("sample_button"), "Stichprobe generieren")
    ),

    mainPanel(
      h4("Zusammenfassung der Stichprobe"),
      # Toggle between sampling strategies
      # TODO: find problem
    #   radioButtons(
    #     ns("sampling_type"),
    #     "Stichprobenstrategie",
    #     choices = c("Garantierte Gesamtstichprobengröße" = "sample_size",
    #                 "Garantierte Mindestgröße pro Kategorie" = "category_size"),
    #     selected = "sample_size"
    #   ),
      dataTableOutput(ns("sample_summary"))
    )
  )
}

# Define the server logic for the stratified sampling module
sample_server <- function(id, strat_layers, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    tab_ids <- reactiveVal(list())

    # Reactive value to store the sampled dataset
    strata <- reactiveVal(NULL)
    display_strata <- reactiveVal(NULL)
    ratios <- reactiveVal(NULL)
    sample_size <- reactiveVal(NULL)
    realized_sample_size <- reactiveVal(NULL)

    observeEvent(input$sample_size, {
      sample_size(input$sample_size)
    })

    # render UIs that depend an data size
    observeEvent(strat_layers(), {
      req(length(strat_layers()) > 0)

      # Update default and limit of sample size input
      layer1 <- strat_layers()[[names(strat_layers())[1]]]
      data_size <- length(layer1$col)

      output$data_size <- renderUI({
        HTML(paste("Grundgesamtheit:", data_size, "Instanzen"))
      })

      updateNumericInput(
        session,
        inputId = "sample_size",
        max = data_size,
        value = min(c(100, data_size))
      )
    })

    # Render probability selection UI dynamically
    observeEvent(strat_layers(), {
      req(strat_layers(), sample_size() > 0)

      # Remove all tabs (all tabs that were kept are inserted again)
      # needed for the case, that a layer was removed in tab4
      for (tid in tab_ids()) {
        removeTab(
          session = session,
          inputId = "select_prob",
          target = tid
        )
      }
      tab_ids(list())
      for (layer in strat_layers()) {
        # Create a new tab for each layer
        new_tab <- tabPanel(
          title = layer$name,
          value = layer$id,
          br(),
          # choice of selection probability
          radioButtons(ns(paste0(layer$id, "sel_kind")),
            label = "Wahrscheinlichkeitsart",
            choices = c(
              "Stichprobenanteil: (Diese Kategorie soll X% der Grundgesamtheit ausmachen)" = "population", # nolint
              "Auswahlwahrscheinlichkeit: (X% dieser Kategorie sollen Teil der Stichprobe sein)" = "category" # nolint
            )
          ),
          hr(),
          # button to reset sliders to proportional
          actionButton(
            ns(paste0(layer$id, "_proportional")), 
            label = "Proportional zum Datensatz"
          ),
          br(), br(),
          # a probability slider for each category
          unname(lapply(names(layer$cat_counts), function(cat) {
            sliderInput(
              ns(paste0(layer$id, "_", cat, "_prob")),
              label = paste0("Wahrscheinlichkeit für ", cat, " (",
                             layer$cat_counts[[cat]], " Instanzen)"),
              min = 0,
              max = 1,
              value = round(layer$cat_counts[[cat]] / length(layer$col), 2),
              step = 0.01
            )
          })),
          uiOutput(ns(paste0(layer$id, "_error")))
        )

        # Add the new tab to the UI
        insertTab(
          session = session,
          inputId = "select_prob",
          tab = new_tab,
          select = TRUE
        )
        tab_ids(c(tab_ids(), layer$id))
      }
    })

    # implement slider logic
    observe({
      for (layer in strat_layers()) {
        # selection mode: either "population" or "category"
        sel_kind <- input[[paste0(layer$id, "sel_kind")]]
        req(sel_kind)

        # Update sliders when "Proportional zur Grundgesamtheit" is toggled
        observeEvent(input[[paste0(layer$id, "_proportional")]], {
          for (cat in names(layer$cat_counts)) {
            prop_value <- ifelse(
              sel_kind == "population",
              # proportion of this category to hole dataset
              layer$cat_counts[[cat]] / length(layer$col),
              # proportion of the sample to the dataset size
              sample_size() / length(layer$col)
            )

            updateSliderInput(
              session,
              paste0(layer$id, "_", cat, "_prob"),
              value = round(prop_value, digits = 2)
            )
          }
        })

        # Throw warning
        output[[paste0(layer$id, "_error")]] <- renderUI({
          # get condition params
          total <- 0
          req_sample_size <- 0
          for (cat in names(layer$cat_counts)) {
            cat_prop <- input[[paste0(layer$id, "_", cat, "_prob")]]
            total <- total + cat_prop
            sample_cat_num <- layer$cat_counts[[cat]] * cat_prop
            req_sample_size <- req_sample_size + sample_cat_num
          }

          # if more than 100% are selected in population mode
          if (sel_kind == "population" && total != 1) {
            ui <- fluidRow(column(12,
              hr(),
              span(HTML(
                "Die Stichprobenanteile der einzelnen Kategorien sollten
                addiert 1 ergeben. Ist das nicht so, werden sie
                standardisiert, sodass das Verhältnis zwischen den Kategorien
                bewahrt bleibt. Die absoluten Stichprobenanteile können dann
                allerdings abweichen."
              ), style = "color:red")
            ))

          # if selection probabilites of the categories are too high for the
          # sample size
          } else if (sel_kind == "category" &&
                       req_sample_size > sample_size()) {
            ui <- fluidRow(column(12,
              hr(),
              span(HTML(paste(
                "Die Auswahlwahrscheinlichkeiten sind so hoch, dass die
                gewählte Stichprobegröße nicht mehr gewährleistet werden
                kann: Die bei diesen Wahrscheinlichkeiten benötigte
                Stichprobengöße von mindestens", req_sample_size, "ist
                größer als die angegebene Stichprobengröße von",
                sample_size(), ". Die Stichprobe wird dadurch größer,
                als angegeben."
              )), style = "color:red")
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
      req(length(strat_layers()) > 0)

      # collect selected proportions
      ratios <- lapply(strat_layers(), function(layer) {
        sel_kind <- input[[paste0(layer$id, "sel_kind")]]
        # store chosen ratios for each layer as a named list like cat_counts
        layer_ratios <- lapply(names(layer$cat_counts), function(cat) {
          r <- input[[paste0(layer$id, "_", cat, "_prob")]]
          if (sel_kind == "category") {
            # convert to proportional to population
            r <- (r * layer$cat_counts[[cat]]) / length(layer$col)
          }
          r
        })
        names(layer_ratios) <- names(layer$cat_counts)

        # if prop was chosen as population percentage and don't add up to 1:
        # -> standardize so ratios always add up to 1
        sum_ratios <- sum(unlist(layer_ratios))
        if (sum_ratios != 1 && sel_kind == "population") {
          layer_ratios <- lapply(layer_ratios, function(e) e / sum_ratios)
        }
        layer_ratios
      })

      # get all categorized columns and chosen ratios from strat_layers()
      data <- data.frame(features(strat_layers(), "col"))
      cat_counts <- features(strat_layers(), "cat_counts")

      # ensure consistent naming
      names <- features(strat_layers(), "name")
      colnames(data) <- names
      names(cat_counts) <- names
      names(ratios) <- names

      # store ratios for later use
      ratios(ratios)

      # define strata sizes
      strata(strata_sizes(data, ratios, cat_counts, 3, sample_size()))
    })

    # Render the summary table
    observeEvent(strata(), {
      req(strata())
      strt <- strata()

      if (TRUE) { # TODO: input$sampling_type == "sample_size") {
        cols <- c("Stratum", "Size.Population", "Ratio.Population",
                  "Size.MD", "Selection.Probability.MD")
      } else {
        cols <- c("Stratum", "Size.Population", "Ratio.Population",
                  "Size.LP", "Selection.Probability.LP")
      }

      strt <- strt[cols]
      colnames(strt) <- c("Stratum", "Größe in Grundgesamtheit",
                          "Anteil Grundgesamtheit", "Größe Stichprobe",
                          "Auswahlwahrscheinlichkeit")
      rss <- sum(strt$`Größe Stichprobe`)
      realized_sample_size(rss)
      display_strata(strt)
    })

    # rendering the calculated strata sizes
    output$sample_summary <- renderDT({
      req(display_strata())

      options <- ifelse(
        !is.null(display_strata()) && nrow(display_strata()) > 10,
        list(dom = "ltpr", lengthMenu = c(10, 15, 20), pageLength = 10),
        list(dom = "ltr")
      )

      datatable(display_strata(),
                class = "cell-border stripe",
                editable = TRUE,
                rownames = FALSE,
                options = options)
    })

    output$realized_sample_size <- renderText({
      paste("Realisierte Stichprobengröße:", realized_sample_size())
    })


    # persisting table edits
    observeEvent(input$strata_cell_edit, {
      row <- input$strata_cell_edit$row
      col <- input$strata_cell_edit$col
      str <- display_strata()
      str[row, col] <- input$strata_cell_edit$value
      display_strata(str)
    })

    return(list(strata = display_strata, ratios = ratios,
                sample_size = realized_sample_size))
  })
}
