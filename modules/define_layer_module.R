# Define the module UI
define_layer_ui <- function(id, col_options) {
  ns <- NS(id)
  
  tagList(
    h4(textOutput(ns("column_name"))),
    selectInput(ns("column_select"), "Spalte auswählen:", choices = col_options),
    # This could also be recognized automatically
    radioButtons(ns("data_type"), "Datentyp auswählen:",
                 choices = c("Numerisch", "Kategorisch"),
                 selected = "Numerisch"),
    numericInput(ns("num_categories"), "Anzahl der Kategorien", value = 2, min = 2, step = 1),
    uiOutput(ns("category_ranges"))
    
  )
}

# Define the module server logic
define_layer_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the selected column
    selected_column <- reactiveVal(NULL)
    
    # Update the selected column
    observeEvent(input$column_select, {
      selected_column(input$column_select)
    })
    
    # Update the column name display
    output$column_name <- renderText({
      req(input$column_select)
      input$column_select
    })
    
    # Rendering the ui for defining categories
    output$category_ranges <- renderUI({
      
      if (input$data_type == "Categorical") {
        return()
      }
      
      min_value <- min(selected_column(), na.rm = TRUE)
      max_value <- max(selected_column(), na.rm = TRUE)
      
      lapply(1:input$num_categories, function(i) {
        fluidRow(
          column(6, numericInput(ns(paste0("category_", i, "_min")), paste0("Kategorie ", i, " von"), min_value)),
          column(6, numericInput(ns(paste0("category_", i, "_max")), paste0("Kategorie ", i, " bis"), max_value))
        )
      })
    })
    # Return name of selected column.
    # TODO: return all collected information which define categories
    #return(reactive({ input$column_select }))
    return(selected_column)
  })
  
}
  
  
  
