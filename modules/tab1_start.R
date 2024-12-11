library(DT)
library(shiny)
library(shinyWidgets)

# TODO: messages in german?

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
        hr(color = "black"),
        pickerInput(
          inputId = ns("col_selector"),
          label = "Spaltenauswahl:",
          choices = NULL,  # Placeholder, will be updated in the server
          selected = NULL, # Placeholder, will be updated in the server
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        actionButton(ns("csv_upload_btn"), "Auswahl hochladen")
      ),

      # Preview
      mainPanel(
        h3("Datenvorschau"),
        div(dataTableOutput(ns("csv_preview")),
            style = "overflow-x: auto;")
      )
    )
  )
}

start_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive values to store uploaded data
    done <- reactiveVal(FALSE)
    csv_data <- reactiveVal(NULL)

    observeEvent(input$csv_file, {
      req(input$csv_file)                       # Ensure a CSV file is selected
      data <- read.csv(input$csv_file$datapath, # Read the uploaded CSV file
                       header = input$csv_header,  # Use header option from UI
                       sep = input$csv_sep,        # Use selected separator
                       nrows = 5)               # Only preview
      csv_data(data)                      # Store the data in the reactive value
      done(FALSE)
    })

    # Event to handle CSV file upload when the button is clicked
    observeEvent(input$csv_upload_btn, {
      req(input$csv_file)                       # Ensure a CSV file is selected
      data <- read.csv(input$csv_file$datapath, # Read the uploaded CSV file
                       header = input$csv_header,  # Use header option from UI
                       sep = input$csv_sep)        # Use selected separator
      csv_data(data[input$col_selector])        # Store selected data
      done(TRUE)
    })

    observeEvent(csv_data(), {
      req(csv_data())
      # Choose columns
      updatePickerInput(
        session,
        "col_selector",
        choices = colnames(csv_data()),
        selected = colnames(csv_data())
      )
    })

    # Render the preview table for the uploaded CSV data
    output$csv_preview <- renderDataTable({
      req(csv_data())

      datatable(csv_data(),
                class = "cell-border stripe",
                options = list(pageLength = 5))
    })

    return(list(data = csv_data, done = done))
                # old_sample = old_sample,
                # ident_primary = ident_primary,
                # ident_secondary = ident_secondary
  })
}