# Define the module
selection_probability_ui <- function(id, title) {
  ns <- NS(id)
  tabPanel(title(),
           selectInput(inputId = ns("sp_kind"),
                       "Art der Auwahlwahrscheinlichkeit",
                       choices = c("Proportionell", "Als Anteil an Stichprobe",
                                   "Alle auswählen"),
                       selected = "Proportionell"),
           uiOutput(ns("sp_inputs")))
}


# Define the module server logic
selection_probability_server <- function(id, values) {
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns
    selection_params <- reactiveValues()
    
    observeEvent(input$sp_kind, {
      selection_params[["kind"]] <- input$sp_kind
    })
    
    
    # putting out a different ui to input selection probabilities
    # depending on what kind is chosen
    observeEvent(input$sp_kind, {
      if(input$sp_kind == "Als Anteil an Stichprobe"){
        # Create a numeric input for each unique value
        output$sp_inputs <- renderUI({
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
        })
        ratios <- lapply(values(), function(value) {
          input[[ns(paste0("ratio_", value))]]
        })
        
        setNames(ratios, values())
        selection_params[["vec"]] <- ratios
      }
      
      if(input$sp_kind == "Alle auswählen"){
        # Create a numeric input for each unique value
        output$sp_inputs <- renderUI({
          checkboxGroupInput(ns("value_select"), "Alle Elemente dieses Stratum
                             auswählen", 
                             choices = values()
                             )
        })
        bools <- as.integer(values() %in% input$value_select)
        selection_params[["vec"]] = bools
      }
      
      if(input$sp_kind == "Proportionell"){
        output$sp_inputs <- renderUI({
          ## TODO: Placeholder for actually calculating and displaying selection
          ## probabilities
          HTML("Auswahlwahrscheinlichkeiten proportionell zu Vorkommen in
                     Grundgesamtheit")
        })
        
      }
    })
    
    
    # Return the ratios as a reactive value
    return(selection_params)
  
  })
}