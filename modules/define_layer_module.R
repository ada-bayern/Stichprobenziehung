library(sortable)

# Define the module UI
define_layer_ui <- function(id, col_options) {
  ns <- NS(id)
  
  fluidPage(
    tags$head(
      # Note the wrapping of the string in HTML()
      tags$style(HTML("
      .buckets-right {
        margin-top: -25px;
        margin-left: -20px;
        margin-bottom: -10px;
        margin-right: -20px;
        /* Adjust as needed */
      }
      .bucket-left {
        margin-top: -20px;
        margin-left: -20px;
        margin-right: -10px;
      }
      
    "))
    ),
    tagList(
      h4(textOutput(ns("column_name"))),
      selectInput(ns("column_select"), "Spalte auswählen:", choices = col_options),
      # This could also be recognized automatically
      radioButtons(ns("data_type"), "Datentyp auswählen:",
                   choices = c("Numerisch", "Kategorisch"),
                   selected = "Numerisch"),
      numericInput(ns("num_categories"), "Anzahl der Kategorien", value = 2, min = 2, step = 1),
      uiOutput(ns("def_categories_ui"))
      
    )
  )
  
  
}

# Define the module server logic
define_layer_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the selected column
    selected_column <- reactiveVal(NULL)
    
    # reactive value to store number of categories
    num_categories <- reactiveVal(NULL)
    
    # stores what kind the data in selected column is (categorical or numeric)
    #TODO: there might be some validation of whether this is correct
    data_type <- reactiveVal(NULL)
    
    # definitions of categories
    categories_def_c <- reactiveValues()
    categories_def_n <- reactiveValues()
    
    # Update selected column, number of categories, and data type
    observeEvent(input$column_select, {
      selected_column(input$column_select)
    })
    
    observeEvent(input$num_categories, {
      num_categories(input$num_categories)
    })
    
    #also resetting category definitions when different data type is selected
    observeEvent(input$data_type, {
      if(input$data_type == "Kategorisch"){
        data_type("categorical")
      } else {
        data_type("numerical")
      }
    })
    
    #update category definitions
    observe({
      req(data_type())
      if (data_type() == "categorical"){
        for (n in 1:num_categories()){
          categories_def_c$n <- input[[paste0("list_cat_", n)]]
        }
      } else {
        for (n in 1:num_categories()){
          categories_def_n$n <- c(input[[paste0("category_", n, "_min")]], 
                                  input[[paste0("category_", n, "_max")]])
          }
      }
    })
  
    
    # Update the column name display
    output$column_name <- renderText({
      req(input$column_select)
      input$column_select
    })
    
    # Rendering the ui for defining categories
    output$def_categories_ui <- renderUI({
      
      if (data_type() == "categorical") {
        vals <- unique(dataset()[[selected_column()]])
        
        fluidRow(
          column(6,
            bucket_list(
              header = NULL,
              group_name = "bucket_list_group",
              class = c("default-sortable", "bucket-left"),
              add_rank_list(
                text = "Werte in Spalte",
                labels = vals,
                input_id = ns("list_orignal_values")
              )
            )
          ),
          column(6,
            lapply(1:num_categories(), function(n){
              tagList(
                textInput(ns(paste0("name_cat_", n)),
                          label = NULL,
                          placeholder = paste("Kategorie", n)),
                bucket_list(
                  header = NULL,
                  group_name = "bucket_list_group",
                  orientation = "vertical",
                  class = c("default-sortable", "buckets-right"),
                  add_rank_list(
                    text = NULL,
                    labels = NULL,
                    input_id = ns(paste0("list_cat_", n))
                  ),
                )
              )
            })
          )
        )
      }
      
      else{
        min_value <- min(selected_column(), na.rm = TRUE)
        max_value <- max(selected_column(), na.rm = TRUE)
        
        lapply(1:num_categories(), function(i) {
          fluidRow(
            column(6, numericInput(ns(paste0("category_", i, "_min")), paste0("Kategorie ", i, " von"), min_value)),
            column(6, numericInput(ns(paste0("category_", i, "_max")), paste0("Kategorie ", i, " bis"), max_value))
          )
        })
      }
      
      
    })
    # Return name of selected column.
    # TODO: return all collected information which define categories
    return(selected_column)
  })
  
}
  
  
  
