#' Module: CSV Data Upload Interface
#'
#' This module creates a user interface (UI) and corresponding server logic to
#' facilitate uploading and handling CSV files in a Shiny application. It
#' enables users to upload data files, configure input options, and preview
#' data before proceeding with analysis or processing.
#'
#' Libraries:
#' - Utilizes DT for rendering data tables, shiny for creating the app
#'   structure, and shinyWidgets for enhanced UI components.
#'
#' UI Structure:
#' - `start_ui`: Composes the UI for file upload, column selection, and data
#'               preview.
#'   - Sidebar: Contains controls for file upload, separator selection, header
#'              indication, and column selection using a picker widget.
#'   - Main Panel: Provides a table preview of the uploaded data.
#'
#' Server Functionality:
#' - `start_server`: Implements the server-side logic for managing data upload
#'                   and interaction.
#'   - Reactive Values: Utilizes reactive values for maintaining state and data.
#'   - File Management: Handles file reading based on UI specifications (header,
#'                      separator).
#'   - Data Preview: Allows user to preview a few rows of the uploaded CSV file.
#'   - Column Selection: Offers a mechanism for choosing specific columns from
#'                       the CSV.
#'
#' Functions and Variables:
#' - `csv_data`: Holds the uploaded CSV data and updates upon file changes.
#' - `done`: A reactive flag indicating whether the data upload process is
#'           complete.
#' - Event Observers: Reactively manage user interaction, such as file
#'                    selection, and update the UI components accordingly.
#' - `pickerInput`: Widget for selecting multiple columns from the dataset.

# Load necessary libraries
library(DT)
library(shiny)
library(shinyWidgets)
library(readr)
library(rintrojs)

source("modules/helpers/utils.R")
source("modules/helpers/manual.R")

# UI Function for CSV upload screen
start_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    introjsUI(),
    actionButton(ns("info"), "Info",
                 icon = icon("question-circle")),

    titlePanel("Daten-Upload"),
    # Layout for sidebar and main panel
    sidebarLayout(
      # Sidebar for actions and options
      sidebarPanel(
        div(id = ns("csv_upload"),
          # Input for file upload
          fileInput(ns("csv_file"),
                    "Datensatz (CSV) auswählen",
                    accept = c("text/csv",
                              "text/comma-separated-values,text/plain",
                              ".csv"),
                    buttonLabel = "Suchen",
                    placeholder = "Keine Datei ausgewählt")
        ), hr(),

        div(id = ns("csv_options"),
          # Option to indicate if the first row contains headers
          checkboxInput(ns("csv_header"),
                        "Kopfzeile",
                        TRUE),
          # Options for specifying the separator
          radioButtons(ns("csv_sep"),
                       "Separator",
                       choices = c(Komma = ",",
                                   Semikolon = ";",
                                   Tab = "\t"),
                       selected = ","),
          # Options for specifying the decimal point
          radioButtons(ns("csv_decimal"),
                       "Dezimaltrennzeichen",
                       choices = c(Komma = ",",
                                   Punkt = "."),
                       selected = ".")
        ),
        hr(),

        # PickerInput for selecting columns
        # pickerInput(
        #   inputId = ns("col_selector"),
        #   label = "Spaltenauswahl:",
        #   choices = NULL, # Placeholder, will be filled by server
        #   selected = NULL, # Placeholder for default selection
        #   multiple = TRUE,
        #   options = list(`actions-box` = TRUE,
        #                  `deselect-all-text` = "Alle abwählen",
        #                  `select-all-text` = "Alle auswählen",
        #                  `none-selected-text` = "Keine ausgewählt")
        # ),

        div(id = ns("col_selector"),
          # Dynamic UI for column (type) selection
          uiOutput(ns("col_type_ui")),

          # Button to finalize the upload
          actionButton(ns("csv_upload_btn"), "Auswahl hochladen"),
        ), hr(),

        div(id = ns("rds"),
          # Optional: File input for RDS file upload
          fileInput(ns("rds_file"),
            "Optional: Stichprobeneinstellungen (RDS) auswählen",
            accept = c(".rds", ".RDS"),
            buttonLabel = "Suchen",
            placeholder = "Keine Datei ausgewählt"
          ),
          uiOutput(ns("rds_response"))
        )
      ),
      # Main panel for data preview
      mainPanel(
        div(id = ns("preview"),
          h3("Datenvorschau"),
          div(dataTableOutput(ns("csv_preview")), style = "overflow-x: auto;")
        )
      )
    )
  )
}

# Server function for handling CSV upload and column selection
start_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    steps <- reactive(data.frame(
      element = c(NA, NA, NA, NA, NA, NA,
                  paste0("#", ns("csv_upload")), paste0("#", ns("csv_options")),
                  paste0("#", ns("col_selector")), paste0("#", ns("rds")),
                  paste0("#", ns("preview"))),
      intro = MANUAL$start
    ))

    observeEvent(input$info, introjs(session, options = c(
      list("steps" = steps()),
      INTRO_OPTIONS
    )))

    # Reactive values to manage the uploaded data state
    done <- reactiveVal(FALSE)
    csv_data <- reactiveVal(NULL)
    rds_data <- reactiveVal(NULL)

    # Handle CSV file upload event
    observeEvent(input$csv_file, {
      req(input$csv_file) # Ensure a file is chosen

      # Read the CSV file with specified input options
      data <- read.csv2(
        input$csv_file$datapath,
        header = input$csv_header,
        sep = input$csv_sep,
        dec = input$csv_decimal,
        # Guess the encoding of the file (UTF-8/ISO-8859-1) to avoid errors
        fileEncoding = guess_encoding(input$csv_file$datapath)$encoding[1],
        nrows = 5
      ) # Read a few lines for preview

      # Store the preview data
      csv_data(data)
      done(FALSE)
    })

    # Finalize upload and handle full data processing
    observeEvent(input$csv_upload_btn, {
      req(input$csv_file) # Check that the file is selected
      data <- read.csv2(
        input$csv_file$datapath,
        header = input$csv_header,
        sep = input$csv_sep,
        dec = input$csv_decimal,
        fileEncoding = guess_encoding(input$csv_file$datapath)$encoding[1]
      )

      # Convert columns based on user selection (or remove them if none)
      for (col in colnames(data)) {
        data[[col]] <- switch(input[[paste0("col_type_", col)]],
          numeric = as.numeric(data[[col]]),
          categorical = as.character(data[[col]]),
          none = NULL
        )
      }

      # Store the selected column data
      csv_data(data)
      # csv_data(data[input$col_selector])
      done(TRUE)
    })

    # Update picker input with column names from the CSV
    # Apply presets if RDS is already loaded
    # observeEvent(csv_data(), {
    #   req(csv_data())
    #   if (is.null(rds_data())) {
    #     selected <- colnames(csv_data())
    #   } else {
    #     selected <- intersect(colnames(csv_data()), rds_data()$cols)
    #   }
    #   updatePickerInput(
    #     session,
    #     "col_selector",
    #     choices = colnames(csv_data()),
    #     selected = selected
    #   )
    # })

    # Generate UI for column type selection
    output$col_type_ui <- renderUI({
      req(csv_data())
      cols <- colnames(csv_data())
      l <- lapply(cols, function(col) {
        # Define choices depending of column properties
        choices <- c("Kategorisch" =  "categorical",
                     "Spalte Entfernen" = "remove")
        if (is.numeric(csv_data()[[col]])) {
          choices <- c("Numerisch" = "numeric", choices)
        }

        fluidRow(column(12,
          tags$i(col),
          radioButtons(
            inputId = session$ns(paste0("col_type_", col)),
            label = "",
            inline = TRUE,
            choices = choices,
            selected = choices[1]
          ),
          hr()
        ))
      })
      list(tags$b("Spaltenauswahl"),
           div(l, style = "height: 40vh; overflow-x: auto;"))
    })

    # Apply presets from RDS
    observeEvent(rds_data(), {
      req(rds_data(), csv_data())
      updatePickerInput(
        session,
        "col_selector",
        selected = intersect(colnames(csv_data()), rds_data()$cols)
      )
    })

    # Render data table for the CSV data preview
    output$csv_preview <- renderDataTable({
      req(csv_data())
      datatable(csv_data(),
                class = "cell-border stripe",
                options = DT_OPTIONS)
    })

    observeEvent(input$rds_file, {
      rds <- readRDS(input$rds_file$datapath)
      valid <- TRUE

      if (is.list(rds)) {
        ui <- span(HTML(paste0(
          "Stichprobeneinstellungen erfolgreich geladen"
        )), style = "color:green")
        settings <- c("cols", "filters", "strat_layers", "ratios", "strata",
                      "sample_size")
        for (s in settings) {
          if (!(s %in% names(rds))) {
            ui <- span(HTML(paste0(
              "Die Stichprobeneinstellungen in der RDS-Datei sind nicht valide
              und können daher nicht verwendet werden. Die Einstellung '", s,
              "' fehlt."
            )), style = "color:red")
            valid <- FALSE
            break
          }
        }
      } else {
        ui <- span(HTML(paste0(
          "Die Stichprobeneinstellungen in der RDS-Datei konnten nicht gelesen
          werden."
        )), style = "color:red")
      }
      output$rds_response <- renderUI(ui)
      if (valid) {
        rds_data(rds)
      }
    })

    # Return outputs for any additional required context
    return(list(data = csv_data, presets = rds_data, done = done))
  })
}