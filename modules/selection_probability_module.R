# Define the module
selection_probability_ui <- function(id, title) {
  ns <- NS(id)
  tabPanel(title,
           selectInput(inputId = ns("sp_kind"),
                       "Art der Auwahlwahrscheinlichkeit",
                       choices = c("Proportional", "Als Anteil an Stichprobe",
                                   "Alle auswählen"),
                       selected = "Als Anteil an Stichprobe"),
           uiOutput(ns("sp_inputs")))
}


# Define the module server logic
selection_probability_server <- function(id, vals) {
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    selection_params <- reactiveValues(kind = NULL, vec = list())
    values <- reactiveVal(na.omit(vals))
    
    output$sp_inputs <- renderUI({
      if(input$sp_kind == "Als Anteil an Stichprobe"){
        # Create a numeric input for each unique value
        inputs <- lapply(values(), function(value) {
          # TODO: could offer to input a percentage value
          numericInput(ns(paste0("ratio_", value)), 
                       paste0("Anteil für ", value, ":"),
                       value = 1 / length(values()),
                       min = 0,
                       max = 1, 
                       step = 0.01)
        })
        do.call(tagList, inputs)
      }
      
      else if(input$sp_kind == "Alle auswählen"){
        # Create a numeric input for each unique value
        checkboxGroupInput(ns("value_select"), "Alle Elemente dieses Stratum auswählen", 
                           choices = values()
        )
      }
      
      else if(input$sp_kind == "Proportional"){
        ## TODO: Placeholder for actually calculating and displaying selection
        ## probabilities
        HTML("Auswahlwahrscheinlichkeiten proportionell zum Vorkommen in Grundgesamtheit")
      }
    })
    
    
    # Calculating the selection params which define strata sizes by way of defining
    # category sizes. Differs based on what kind of interface user has selected
    observe({
      req(input$sp_kind)
      if(input$sp_kind == "Als Anteil an Stichprobe"){
        ratios <- lapply(values(), function(value) {
          req(input[[paste0("ratio_", value)]])
          input[[paste0("ratio_", value)]]
        })
        ratios <- unlist(ratios)
        names(ratios) <- values()
        selection_params$kind <- "sample"
      }
      if(input$sp_kind == "Alle auswählen"){
        req(input$value_select)
        ratios <- ifelse(values() %in% input$value_select, 1, NA)
        names(ratios) <- values()
        selection_params$kind <- "population"
      }
      if(input$sp_kind == "Proportional") {
        selection_params$kind <- "proportional"
        ratios <- rep(NA, length(values()))
        names(ratios) <- values()
      }
      
      selection_params$vec <- ratios
    })
    
    
    # Return the ratios as a reactive value
    return(selection_params)
  
  })
}