library(DT)

start_ui <- function(id) {
    ns <- NS(id)
    fluidPage(
        titlePanel('Daten-Upload'),
            
        # CSV-Upload
        sidebarLayout(
            # Sidebar for actions and options
            sidebarPanel(
                fileInput(ns('csv_file'), 'CSV-Datei auswählen',        # Input for file upload
                    accept=c('text/csv',
                            'text/comma-separated-values,text/plain',
                            '.csv')),
                tags$hr(),                                  # Horizontal line for separation
                checkboxInput(ns('csv_header'), 'Kopfzeile', TRUE), # Option to indicate if the first row contains headers
                radioButtons(ns('csv_sep'), 'Separator',            # Options for specifying the separator
                                choices = c(Komma = ',',
                                            Semikolon = ';',
                                            Tab = '\t'),
                                selected = ','),            # Default selected separator
                actionButton(ns('csv_upload_btn'), 'Hochladen')     # Button to trigger upload
            ),

            # Preview
            mainPanel(
                h3('Datenvorschau'),                         # Title for the data preview section
                tableOutput(ns('csv_preview'))                # Output area for the data preview table
            )
        ),

        # RDS-Upload
        sidebarLayout(
            # Sidebar for actions and options
            sidebarPanel(
                fileInput(ns('rds_file'), 'Kartendatei (RDS) auswählen',     # Input for RDS file upload
                        accept = '.rds'),
                actionButton(ns('rds_upload_btn'), 'Hochladen')  # Button to trigger RDS upload
            ),

            # Preview
            mainPanel(
                h3('Kartenvorschau'),  # Title for the RDS data preview section
                verbatimTextOutput(ns('rds_preview'))  # Output area for the RDS data preview
            )
        )
    )
}

start_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        # Reactive values to store uploaded data
        csv_data <- reactiveVal(NULL)  
        rds_data <- reactiveVal(NULL)
    
        # Event to handle CSV file upload when the button is clicked
        observeEvent(input$csv_upload_btn, {
            req(input$csv_file)                          # Ensure a CSV file is selected
            data <- read.csv(input$csv_file$datapath,   # Read the uploaded CSV file
                            header = input$csv_header,  # Use header option from UI
                            sep = input$csv_sep)        # Use selected separator
            csv_data(data)                              # Store the uploaded CSV data in the reactive value
        })
        # Render the preview table for the uploaded CSV data
        output$csv_preview <- renderTable({
            req(csv_data())                             # Ensure CSV data is available
            head(csv_data(), 5)                        # Show the first 10 rows of the uploaded CSV data
        })
        
        # Event to handle RDS file upload when the button is clicked
        observeEvent(input$rds_upload_btn, {
            req(input$rds_file)                         # Ensure an RDS file is selected
            data <- readRDS(input$rds_file$datapath)    # Read the uploaded RDS file
            rds_data(data)                              # Store the uploaded RDS data in the reactive value
        })
        # Render the preview for the uploaded RDS data
        output$rds_preview <- renderPrint({
            req(rds_data())                             # Ensure RDS data is available
            rds_data()                                  # Display the content of the uploaded RDS data
        })
    })
}