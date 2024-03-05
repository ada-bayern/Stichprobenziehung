
# Define UI
tab2ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fileInput(ns("file"), "Daten", buttonLabel = "Hochladen"),
    textInput(ns("delim"), "Delimiter (leer lassen um zu raten)", ""),
    numericInput(ns("skip"), "Spalten zu überspringen", 0, min = 0),
    numericInput(ns("rows"), "Spalten anzeigen", 10, min = 1)
    
  )
}

tab2server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store the uploaded dataframe
    uploaded_data <- reactiveVal(NULL)
    
    # Upload file
    observeEvent(input$file, {
      uploaded_data(read.csv(input$file$datapath))
    })
    
    # Output file information
    output$file_info <- renderPrint({
      req(input$file)
      paste("Uploaded file:", input$file$name)
    })
    
    # Return uploaded data
    return(uploaded_data)
  })
}