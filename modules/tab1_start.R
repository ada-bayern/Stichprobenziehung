library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)

# TODO: Kartenvorschau

start_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Daten-Upload"),

    # CSV-Upload
    sidebarLayout(
      # Sidebar for actions and options
      sidebarPanel(
        # Input for file upload
        fileInput(ns("csv_file"),
                  "CSV-Datei auswählen",
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        hr(),
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
        # Button to trigger upload
        actionButton(ns("csv_upload_btn"), "Hochladen"),
        hr(color = "black"),
        pickerInput(
          inputId = ns("col_selector"),
          label = "Spaltenauswahl:",
          choices = NULL,  # Placeholder, will be updated in the server
          selected = NULL, # Placeholder, will be updated in the server
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        actionButton(ns("select_btn"), "Auswahl anwenden")
      ),

      # Preview
      mainPanel(
        h3("Datenvorschau"),
        div(DTOutput(ns("csv_preview")),
            style = "overflow-x: auto;")
      )
    )
  )
}

start_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store uploaded data
    csv_data <- reactiveVal(NULL)
    final_data <- reactiveVal(NULL)
    done <- reactiveVal(FALSE)

    # Event to handle CSV file upload when the button is clicked
    observeEvent(input$csv_upload_btn, {
      req(input$csv_file)                       # Ensure a CSV file is selected
      data <- read.csv(input$csv_file$datapath, # Read the uploaded CSV file
                       header = input$csv_header,  # Use header option from UI
                       sep = input$csv_sep)        # Use selected separator
      csv_data(data)                      # Store the data in the reactive value
      final_data(data)            # Use another reactive value for selected data
      done(TRUE)
    })

    observe({
      req(csv_data())
      # Choose columns
      updatePickerInput(
        session,
        "col_selector",
        choices = colnames(csv_data()),
        selected = colnames(csv_data())
      )
    })

    observeEvent(input$select_btn, {
      cols <- input$col_selector
      final_data(select(csv_data(), cols))
    })

    # Render the preview table for the uploaded CSV data
    output$csv_preview <- renderDT({
      req(final_data())

      datatable(final_data(),
                class = "cell-border stripe",
                options = list(pageLength = 5))
    })

    return(list(data = final_data,
                done = done))
                # old_sample = old_sample,
                # ident_primary = ident_primary,
                # ident_secondary = ident_secondary
  })
}