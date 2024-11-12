library(DT)
library(shiny)

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
        actionButton(ns("csv_upload_btn"), "Hochladen")
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

    # Event to handle CSV file upload when the button is clicked
    observeEvent(input$csv_upload_btn, {
      req(input$csv_file)                       # Ensure a CSV file is selected
      data <- read.csv(input$csv_file$datapath, # Read the uploaded CSV file
                       header = input$csv_header,  # Use header option from UI
                       sep = input$csv_sep)        # Use selected separator
      csv_data(data)                     # Store the data in the reactive value
    })
    # Render the preview table for the uploaded CSV data
    output$csv_preview <- renderDT({
      req(csv_data())
      datatable((csv_data()),
                class = "cell-border stripe",
                options = list(pageLength = 5))
    })


    return(list(csv_data = csv_data))
                # old_sample = old_sample,
                # ident_primary = ident_primary,
                # ident_secondary = ident_secondary
  })
}