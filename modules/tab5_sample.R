#' Module: Stratified Sampling
#'
#' This module implements a stratified sampling approach in a Shiny
#' application. Users can define sampling strategies, allocate probabilities for
#' different strata, and calculate sample sizes with respect to stratification
#' criteria.
#'
#' Libraries and Imports:
#' - Utilizes libraries such as shiny and DT for dynamic UI elements and
#'   interactive table displays.
#' - Imports utility functions from external R scripts (`utils.R`,
#'   `stratification/strata_sizes.R`).
#'
#' UI Structure:
#' - Sidebar Panel: Allows input of sample size and manages dynamic UI elements
#'   for probability allocation.
#' - Main Panel: Displays a summary of the sampled data and provides options
#'   for selecting sampling strategies.
#'
#' Server Functionality:
#' - Handles interactive inputs for defining strata probabilities.
#' - Calculates strata sizes based on user-defined probabilities.
#' - Generates and displays a summary of sampled data.
#'
#' Important Variables/Functions:
#' - `sample_ui`: Constructs the user interface for the sampling module.
#' - `sample_server`: Manages logic related to stratified sampling, including
#'   updating UI components and processing sampling output.
#' - `strat_layers`: Represents stratification layer information, which affects
#'   sampling calculations.
#' - `ratios`, `sample_size`, `display_strata`: Reactive values storing
#'   sampling parameters and results.

# Load necessary libraries
library(shiny)
library(DT)

# TODO: LP sampling method without md (see strata_sizes.R)
# TODO: Add summary of single categories?
# TODO: modularize

# Import utility functions
source("modules/helpers/utils.R")

source("modules/helpers/tab5_1_strata_sizes.R")

# Define the UI for the stratified sampling module (in German)
sample_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module

  fluidPage(
    titlePanel("Stichprobenziehung"),
    sidebarLayout(
      sidebarPanel(
        # Sample size input
        numericInput(ns("sample_size"),
                     "Stichprobengröße",
                     value = 100,
                     min = 1,
                     max = 99999,
                     width = "80px"),
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
        # TODO: LP
        # radioButtons(
        # ns("sampling_type"),
        # "Stichprobenstrategie",
        # choices = c("Garantierte Gesamtstichprobengröße" = "sample_size",
        # "Garantierte Mindestgröße pro Kategorie" = "category_size"),
        # selected = "sample_size"
        # ),
        dataTableOutput(ns("sample_summary"))
      )
    )
  )
}

# Define the server logic for the stratified sampling module
sample_server <- function(id, strat_layers, data_size, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tab_ids <- reactiveVal(list())

    # Reactive values for sampling output
    strata <- reactiveVal(NULL)
    display_strata <- reactiveVal(NULL)
    ratios <- reactiveVal(NULL)
    sample_size <- reactiveVal(NULL)
    realized_sample_size <- reactiveVal(NULL)

    # Update sample size when input changes
    observeEvent(input$sample_size, {
      sample_size(input$sample_size)
    })

    # Render UI elements that depend on data size
    observeEvent(data_size(), {
      output$data_size <- renderUI({
        HTML(paste("Grundgesamtheit:", data_size(), "Instanzen"))
      })

      updateNumericInput(
        session,
        inputId = "sample_size",
        max = data_size(),
        value = min(c(100, data_size()))
      )
    })

    # Render probability selection UI dynamically
    observeEvent(strat_layers(), {
      req(strat_layers(), sample_size() > 0)

      # Remove all existing tabs
      for (tid in tab_ids()) {
        removeTab(
          session = session,
          inputId = "select_prob",
          target = tid
        )
      }
      tab_ids(list())

      for (layer in strat_layers()) {
        # Create a new tab for each stratification layer
        new_tab <- tabPanel(
          title = layer$name,
          value = layer$id,
          br(),
          # Choice for selection probability
          radioButtons(ns(paste0(layer$id, "sel_kind")),
            label = "Wahrscheinlichkeitsart",
            choices = c(
              "Stichprobenanteil: (Diese Kategorie soll X% der Stichprobe ausmachen)" = "population", # nolint
              "Auswahlwahrscheinlichkeit: (X% dieser Kategorie sollen Teil der Stichprobe sein)" = "category" # nolint
            )
          ),
          hr(),
          # Button to reset sliders to proportional values
          actionButton(
            ns(paste0(layer$id, "_proportional")),
            label = "Proportional zum Datensatz"
          ),
          br(), br(),
          # Probability slider for each category
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

    # Implement slider logic
    observe({
      for (layer in strat_layers()) {
        # Selection mode: either "population" or "category"
        sel_kind <- input[[paste0(layer$id, "sel_kind")]]
        req(sel_kind)

        # Update sliders to proportional values
        observeEvent(input[[paste0(layer$id, "_proportional")]], {
          for (cat in names(layer$cat_counts)) {
            prop_value <- ifelse(
              sel_kind == "population",
              # Proportion of this category to entire dataset
              layer$cat_counts[[cat]] / length(layer$col),
              # Proportion of the sample to the dataset size
              sample_size() / length(layer$col)
            )
            updateSliderInput(
              session,
              paste0(layer$id, "_", cat, "_prob"),
              value = round(prop_value, digits = 2)
            )
          }
        })

        # Render error messages for each layer
        output[[paste0(layer$id, "_error")]] <- renderUI({
          total <- 0
          req_sample_size <- 0

          for (cat in names(layer$cat_counts)) {
            cat_prop <- input[[paste0(layer$id, "_", cat, "_prob")]]
            total <- total + cat_prop
            sample_cat_num <- layer$cat_counts[[cat]] * cat_prop
            req_sample_size <- req_sample_size + sample_cat_num
          }

          # Display warning message if conditions are not met
          if (sel_kind == "population" && total != 1) {
            ui <- fluidRow(column(12,
              hr(),
              span(HTML(
                "Die Stichprobenanteile der einzelnen Kategorien sollten
                addiert 1 ergeben. Ist das nicht so, werden sie standardisiert,
                sodass das Verhältnis zwischen den Kategorien bewahrt bleibt.
                Die absoluten Stichprobenanteile können dann allerdings
                abweichen."
              ), style = "color:red")
            ))
          } else if (sel_kind == "category" &&
                       req_sample_size > sample_size()) {
            ui <- fluidRow(column(12,
              hr(),
              span(HTML(paste(
                "Die Auswahlwahrscheinlichkeiten sind zu hoch: Die bei diesen
                Wahrscheinlichkeiten benötigte Stichprobengöße von mindestens",
                req_sample_size, "ist größer als die angegebene Stichprobengröße
                von", sample_size(), ". Die Sichprobengröße bleibt unverändert,
                jede gezogene Instanz wird Teil dieser Kategorie sein,
                allerdings kann die Auswahlwahrscheinlichkeit nicht eingehalten
                werden."
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
      req(strat_layers())

      # Collect selected proportions
      ratios <- lapply(strat_layers(), function(layer) {
        sel_kind <- input[[paste0(layer$id, "sel_kind")]]

        # Store chosen ratios as a named list like cat_counts
        layer_ratios <- lapply(names(layer$cat_counts), function(cat) {
          r <- input[[paste0(layer$id, "_", cat, "_prob")]]

          if (sel_kind == "category") {
            # Convert to proportion of population
            r <- (r * layer$cat_counts[[cat]]) / length(layer$col)
            r <- min(c(r, 1))
          }
          r
        })

        # Name the list of ratios
        names(layer_ratios) <- names(layer$cat_counts)

        # Standardize ratios to sum up to 1 in population mode
        sum_ratios <- sum(unlist(layer_ratios))
        if (sum_ratios != 1 && sel_kind == "population") {
          layer_ratios <- lapply(layer_ratios, function(e) e / sum_ratios)
        }
        layer_ratios
      })

      # Get categorized columns and chosen ratios from strat_layers()
      data <- data.frame(features(strat_layers(), "col"))
      cat_counts <- features(strat_layers(), "cat_counts")

      # Ensure consistent naming conventions
      names <- features(strat_layers(), "name")
      colnames(data) <- names
      names(cat_counts) <- names
      names(ratios) <- names

      # Store ratios for later use
      ratios(ratios)

      if (length(ratios) == 0) {
        data <- data.frame(Gesamtheit = rep("Gesamtheit", 100))
        ratios <- list(Gesamtheit = list(Gesamtheit = 1))
        cat_counts <- list(Gesamtheit = list(Gesamtheit = 100))
      }

      # Define strata sizes using imported function
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

    # Render the calculated strata sizes in a data table
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

    # Persist table edits
    observeEvent(input$strata_cell_edit, {
      row <- input$strata_cell_edit$row
      col <- input$strata_cell_edit$col
      str <- display_strata()
      str[row, col] <- input$strata_cell_edit$value
      display_strata(str)
    })

    # Return information for use in the application, such as strata summary
    return(list(strata = display_strata, ratios = ratios,
                sample_size = realized_sample_size))
  })
}