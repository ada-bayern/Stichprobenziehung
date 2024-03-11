
# Define UI
tab2ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fileInput(ns("file"), "Daten", buttonLabel = "Hochladen"),
    br(),
    actionButton(ns("show_text"), "Daten einsehen"),
    tableOutput(ns("head")),
    
    plotOutput(ns("plot1"))
    
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
    
    observe({
      req(input$show_text)
      output$head <- renderTable({
        head(uploaded_data())
      })
      output$plot1 <- renderPlot({
        plot(uploaded_data()[,5])
      })
    })
    
    # Return uploaded data
    return(uploaded_data)
  })
}