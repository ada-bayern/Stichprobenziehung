
# Define UI
tab2ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fileInput(ns("file"), "Daten", buttonLabel = "Hochladen")
    
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