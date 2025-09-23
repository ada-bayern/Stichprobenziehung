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
#' - `dataset`: Represents the categorized dataset, which is used for sampling
#' - `cat_counts`, `ratios`, `sample_size`, `display_strata`: Reactive values
#'   storing sampling parameters and results.

# Load necessary libraries
library(shiny)
library(DT)
library(rintrojs)
library(R.utils)

# TODO: Add summary of single categories?

# Import utility functions
source("modules/helpers/utils.R")
source("modules/helpers/manual.R")
source("modules/helpers/tab5_1_strata_sizes.R")

# Define the UI for the stratified sampling module (in German)
sample_ui <- function(id) {
  ns <- NS(id)  # Namespace for the module

  fluidPage(
    introjsUI(),
    actionButton(ns("info"), "Info",
                 icon = icon("question-circle")),

    titlePanel("Stichprobenziehung"),
    sidebarLayout(
      sidebarPanel(
        div(id = ns("sidebar1"),
          # Sample size input
          numericInput(ns("sample_size"),
                       "Stichprobengröße",
                       value = 100,
                       min = 1,
                       max = 99999,
                       width = "80px"),
          uiOutput(ns("data_size"))
        ), hr(),

        div(id = ns("sidebar2"),
          # Dynamic UI for probability selection
          tabsetPanel(id = ns("select_prob"))
        ), hr(),

        div(id = ns("sidebar3"),
          # Button to trigger the sampling
          actionButton(ns("sample_button"), "Stichprobe generieren")
        )
      ),
      mainPanel(
        h4("Zusammenfassung der Stichprobe"),

        div(id = ns("main1"),
          numericInput(ns("strat_min_size"),
                       "Mindestgröße der Schichten (Strata)",
                       value = 1,
                       min = 1,
                       max = 100)
        ), hr(),
        div(id = ns("main2"),
          # Toggle between sampling strategies
          radioButtons(
            ns("sampling_type"),
            "Stichprobenstrategie",
            choices = c(
              "Naives Verfahren (Keine Garantie; empfohlen bei vielen Strata (über 20))" = "naive",
              "Garantierte Gesamtstichprobengröße" = "sample_size",
              "Garantierte Mindestgröße pro Kategorie" = "category_size"
            ),
            selected = "naive"
          )
        ), hr(),

        div(id = ns("main3"),
          # Display the size of the sample
          uiOutput(ns("realized_sample_size"))
        ), hr(),

        # Data table to show the summary of the sampled data
        div(id = "main4",
          DTOutput(ns("sample_summary")),
          style = "overflow-x: auto;"
        ),
      )
    )
  )
}

# Define the server logic for the stratified sampling module
sample_server <- function(id, dataset, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Steps for Guided Tour
    steps <- reactive(data.frame(
      element = c(NA, paste0("#", ns("sidebar1")), paste0("#", ns("sidebar2")),
                  paste0("#", ns("sidebar3")), paste0("#", ns("main1")),
                  paste0("#", ns("main2")), paste0("#", ns("main2")),
                  paste0("#", ns("main3")), paste0("#", ns("main4")),
                  paste0("#", ns("sidebar2")), paste0("#", ns("sidebar2")),
                  paste0("#", ns("sidebar2")), NA, NA),
      intro = MANUAL$sample
    ))

    # Info button for Guided Tour
    observeEvent(input$info, introjs(session, options = c(
      list("steps" = steps()),
      INTRO_OPTIONS
    )))

    tab_ids <- reactiveVal(list())

    # Reactive values for sampling output
    strata <- reactiveVal(NULL)
    display_strata <- reactiveVal(NULL)
    ratios <- reactiveVal(NULL)
    population_ratios <- reactiveVal(NULL)
    sample_size <- reactiveVal(NULL)
    realized_sample_size <- reactiveVal(NULL)
    cat_counts <- reactiveVal(NULL)
    data_size <- reactiveVal(NULL)
    sampling_type <- reactiveVal(NULL)
    proportional <- reactiveVal(TRUE)

    observeEvent(dataset(), {
      cc <- list()
      for (name in colnames(dataset())) {
        cc[[name]] <- table(dataset()[[name]])
      }
      cat_counts(cc)
      data_size(nrow(dataset()))
    })

    # Update sample size when input changes
    observeEvent(input$sample_size, {
      sample_size(input$sample_size)
    })

    observeEvent(input$sampling_type, {
      sampling_type(input$sampling_type)
    })

    # Render UI elements that depend on data size
    observeEvent(data_size(), {
      output$data_size <- renderUI({
        HTML(paste("Grundgesamtheit:", data_size(), "Instanzen"))
      })

      # Get sample_size from presets if applcable, else 100
      val <- presets()$sample_size
      if (is.null(val)) {
        val <- 100
      }
      updateNumericInput(
        session,
        inputId = "sample_size",
        max = data_size(),
        value = min(c(val, data_size()))
      )
    })

    # Render probability selection UI dynamically
    observeEvent(cat_counts(), {
      req(cat_counts(), sample_size() > 0)
      # Remove all existing tabs
      for (tid in tab_ids()) {
        removeTab(
          session = session,
          inputId = "select_prob",
          target = tid
        )
      }
      tab_ids(list())

      for (name in names(cat_counts())) {
        # skip hidden "Gesamtheit" column
        if (name == "Gesamtheit") next
        # sublist of cat_counts
        ccn <- cat_counts()[[name]]

        # Create a new tab for each stratification layer
        new_tab <- tabPanel(
          title = name,
          value = name,
          br(),
          # Choice for selection probability
          radioButtons(ns(paste0(name, "_sel_kind")),
            label = "Wahrscheinlichkeitsart",
            choices = c(
              "Stichprobenanteil: (Diese Kategorie soll X% der Stichprobe ausmachen)" = "population", # nolint
              "Auswahlwahrscheinlichkeit: (X% dieser Kategorie sollen Teil der Stichprobe sein)" = "category" # nolint
            )
          ),
          hr(),
          # Button to reset sliders to proportional values
          actionButton(
            ns(paste0(name, "_proportional")),
            label = "Proportional zum Datensatz"
          ),
          br(), br(),
          # Probability slider for each category
          unname(lapply(names(ccn), function(cat) {
            # Get default value from presets (or use proportional if not appl.)
            val <- presets()$ratios[[name]][[cat]]
            if (is.null(val)) {
              val <- ccn[[cat]] / data_size()
            }

            sliderInput(
              ns(paste0(name, "_", cat, "_prob")),
              label = paste0("Wahrscheinlichkeit für ", cat, " (",
                             ccn[[cat]], " Instanzen)"),
              min = 0,
              max = 1,
              value = val,
              step = 0.001
            )
          })),
          uiOutput(ns(paste0(name, "_error")))
        )

        # Add the new tab to the UI
        insertTab(
          session = session,
          inputId = "select_prob",
          tab = new_tab,
          select = TRUE
        )
        tab_ids(c(tab_ids(), name))
      }
    })

    # Implement slider logic
    observe({
      req(cat_counts())

      for (name in names(cat_counts())) {
        # skip hidden "Gesamtheit" column
        if (name == "Gesamtheit") next
        # sublist of cat_counts
        ccn <- cat_counts()[[name]]

        # Selection mode: either "population" or "category"
        sel_kind <- input[[paste0(name, "_sel_kind")]]
        req(sel_kind)

        # Update sliders to proportional values
        observeEvent(input[[paste0(name, "_proportional")]], {
          sel_kind <- input[[paste0(name, "_sel_kind")]] # must be done because of the observeEvent # nolint
          for (cat in names(ccn)) {
            # Marker
            prop_value <- if (sel_kind == "population") {
              # Proportion of this category to entire dataset
              ccn[[cat]] / data_size()
            } else {
              # Proportion of the sample to the dataset size
              sample_size() / data_size()
            }
            updateSliderInput(
              session,
              paste0(name, "_", cat, "_prob"),
              value = prop_value
            )
          }
          # Set marker that sliders are proportional
          proportional(TRUE)
        })

        # If any slider is changed, set proportional marker to FALSE
        for (cat in names(ccn)) {
          observeEvent(input[[paste0(name, "_", cat, "_prob")]], {
            proportional(FALSE)
          })
        }

        # Render error messages for each layer
        output[[paste0(name, "_error")]] <- renderUI({
          total <- 0
          req_sample_size <- 0

          for (cat in names(ccn)) {
            cat_prop <- input[[paste0(name, "_", cat, "_prob")]]
            total <- total + cat_prop
            sample_cat_num <- ccn[[cat]] * cat_prop
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
                von", sample_size(), ". Die Sichprobengröße bleibt unverändert 
                und der Anteil dieser Kategorie wird maximiert,
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
      req(cat_counts(), dataset(), sampling_type(), sample_size() > 0)

      population_ratios <- lapply(names(cat_counts()), function(name) {
        ccn <- cat_counts()[[name]]
        lapply(names(ccn), function(cat) {
          ccn[[cat]] / data_size()
        })
      })

      # Collect selected proportions
      ratios <- lapply(names(cat_counts()), function(name) {
        sel_kind <- input[[(paste0(name, "_sel_kind"))]]
        ccn <- cat_counts()[[name]]

        # Store chosen ratios as a named list like cat_counts
        ratios_n <- lapply(names(ccn), function(cat) {
          ratio_input <- input[[paste0(name, "_", cat, "_prob")]]
          # Gesamtheit is a hidden column that has the same category
          # for every instance -> necessary for random sampling
          if (name == "Gesamtheit") {
            r <- 1
          } else if (sel_kind == "category") {
            # Convert to proportion of population
            r <- (ratio_input * ccn[[cat]]) / sample_size()
            r <- min(c(r, 1))
            print(paste("RatioInput for", name, "category", cat, "is", ratio_input))
            print(paste("Ratio for", name, "category", cat, "is", r))
          } else {
            r <- ratio_input
          }
          r
        })

        # Standardize ratios to sum up to 1 in population mode
        sum_ratios <- sum(unlist(ratios_n))
        if (sum_ratios != 1 && sel_kind == "population") {
          ratios_n <- lapply(ratios_n, function(e) e / sum_ratios)
        }
        names(ratios_n) <- names(ccn)
        ratios_n
      })
      names(ratios) <- names(cat_counts())
      names(population_ratios) <- names(cat_counts())

      # Store ratios for later use
      ratios(ratios)
      population_ratios(population_ratios)

      # Add a timeout to prevent long-running operations
      s_sizes <- withProgress(message = "Berechnung läuft...", value = 0, {
        #withTimeout({
          strata_sizes(sampling_type(), dataset(), ratios(),
                       cat_counts(), input$strat_min_size,
                       sample_size())
        #}, timeout = 20, onTimeout = "warning")
      })

      # Define strata sizes using imported function
      strata(s_sizes)
    })

    # Render the summary table
    observeEvent({
      strata()
      #sampling_type()
    }, {
      req(strata(), sampling_type())
      strt <- strata()
      cols <- c("Stratum", "Size.Population", "Ratio.Population", "Size.Sample",
                "Selection.Probability")

      if (sum(strt["Size.Sample"]) == 0) {
        showModal(modalDialog(
          title = "Fehler bei der Stichprobenziehung",
          "Das Verfahren hat das Zeitlimit überschritten. Bitte wählen Sie 
          ein anderes Verfahren für eine Stichprobenziehung mit diesen
          Parametern aus.",
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      }

      # if (sampling_type() == "sample_size") {
      #   cols <- c(cols, "Size.MD", "Selection.Probability.MD")
      # } else if (sampling_type() == "category_size") {
      #   cols <- c(cols, "Size.LP", "Selection.Probability.LP")
      # } else {
      #   cols <- c(cols, "Size.Naive", "Selection.Probability.Naive")
      # }
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
      options <- DT_OPTIONS
      if(!is.null(display_strata()) && nrow(display_strata()) > 5) {
        options$dom <- "ltpr"
        options$lengthMenu <- c(10, 15, 20)
        options$pageLength <- 10
      } else {
        options$dom <- "ltr"
      }
      datatable(display_strata(),
                class = "cell-border stripe",
                editable = TRUE,
                rownames = FALSE,
                options = options)
    })

    output$realized_sample_size <- renderUI({
      req(realized_sample_size())
      fluidRow(column(12,
        tags$b("Tatsächlich realisierte Stichprobengröße:"),
        br(),
        span(realized_sample_size(), style = "font-weight: bold;")
      ))
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
    return(list(strata = display_strata,
                ratios = ratios,
                population_ratios = population_ratios,
                sample_size = realized_sample_size,
                sampling_type = sampling_type,
                proportional = proportional,
                data_size = data_size))
  })
}