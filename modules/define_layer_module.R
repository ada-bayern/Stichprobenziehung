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
      uiOutput(ns("def_categories_ui")),
      actionButton(ns("apply_button"), label = "Anwenden")
      
    )
  )
  
  
}

# Define the module server logic
define_layer_server <- function(id, dataset) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Reactive value to store the selected column
    selected_column <- reactiveValues(name = NULL, data_type = NULL, 
                                      categories = list())
    
    # reactive value to store number of categories
    num_categories <- reactiveVal(NULL)
    
    # Update selected column, number of categories, and data type
    observeEvent(input$column_select, {
      selected_column$name <- input$column_select
    })
    
    observeEvent(input$num_categories, {
      num_categories(input$num_categories)
    })
    
    #also resetting category definitions when different data type is selected
    observeEvent(input$data_type, {
      if(input$data_type == "Kategorisch"){
        selected_column$data_type <- "categorical"
      } else {
        selected_column$data_type <- "numerical"
      }
    })
  
    
    # Update the column name display
    output$column_name <- renderText({
      req(input$column_select)
      input$column_select
    })
    
    # Rendering the ui for defining categories
    output$def_categories_ui <- renderUI({
      
      if (selected_column$data_type == "categorical") {
        vals <- unique(dataset()[[selected_column$name]])
        
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
        min_value <- min(num_categories(), na.rm = TRUE)
        max_value <- max(num_categories(), na.rm = TRUE)
        
        lapply(1:num_categories(), function(i) {
          fluidRow(
            column(6, numericInput(ns(paste0("min_cat_", i)), paste0("Kategorie ", i, " von"), value = 0)),
            column(6, numericInput(ns(paste0("max_cat_", i)), paste0("Kategorie ", i, " bis"), value = 0))
          )
        })
      }
    })
    
    # Update category definitions
    observeEvent(input$apply_button, {
      req(selected_column$data_type)
      if(selected_column$data_type == "categorical"){
        categories <- lapply(1:num_categories(), function(i){
          req(input[[paste0("list_cat_", i)]])
          input[[paste0("list_cat_", i)]]
        })
        names <- lapply(1:num_categories(), function(i){
          name <- input[[paste0("name_cat_", i)]]
          ifelse(!is.null(name), name, paste("Kategorie", i))
        })
      }else{
        categories <- lapply(1:num_categories(), function(i){
          req(input[[paste0("min_cat_", i)]], input[[paste0("max_cat_", i)]])
          c(input[[paste0("min_cat_", i)]], input[[paste0("max_cat_", i)]])
        })
        names <- lapply(1:num_categories(), function(i){
          paste0(input[[paste0("min_cat_", i)]], input[[paste0("max_cat_", i)]])
        })
      }
      names(categories) <- names
      selected_column$categories <- categories
    })
    
    
    # Return name of selected column.
    # TODO: return all collected information which define categories
    return(selected_column)
  })
  
}
  
  
  
