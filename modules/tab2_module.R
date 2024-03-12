
# Define UI
tab2ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fileInput(ns("file"), "Daten", buttonLabel = "Hochladen"),
    br(),
    actionButton(ns("show_text"), "Daten einsehen"),
    tableOutput(ns("head")),
    tableOutput(ns("head.akten")),
    verbatimTextOutput(ns("summary1")),
    
    plotOutput(ns("plot1"))
    
  )
}

tab2server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value to store the uploaded dataframe
    uploaded_data <- reactiveVal(NULL)
    
    my_akten <- reactiveVal(NULL)
    
    # Upload file
    observeEvent(input$file, {
      uploaded_data(read.csv(input$file$datapath))
      
      akten <- group_by(uploaded_data(), 
                        `Gericht`,
                        `Aktenzeichen`,
                        `Streitwert.in.EURO`,
                        `Gesamtstreitgegenstand`,
                        `Erledigungsgrund`,
                        `Dauer.des.Verfahrens.in.Tagen`,
                        `Archivstatus`,
                        `Anbietungsgrund..manuell.erfasst.`,
                        `Anbietungsgrund`)
      akten <- summarise(akten, `Anzahl Beteiligte` = n()) 
      akten <- as.data.frame(akten)
      akten <- mutate(akten,Index = row_number())
      
      my_akten(akten)
                        
    })
    
    observe({
      #show head
      req(input$show_text)
      output$head <- renderTable({
        head(uploaded_data())
      })
      
      req(input$show_text)
      output$head.akten <- renderTable({
        head(my_akten())
      })
      
      output$summary1 <- renderPrint({
        summary(my_akten())
      })
      
      #plot 
      output$plot1 <- renderPlot({
        ggplot(my_akten(), aes(Gericht)) + 
          geom_bar() + 
          scale_x_discrete(guide = guide_axis(angle = 90))
      })
      
    })
    
    # Return uploaded data
    return(uploaded_data)
  })
}