source("modules/define_layer_module_old.R")
source("modules/selection_probability_module.R")
library(gtsummary)
source(file.path("../aktenstichprobe/R", "select_groups.R"))

install.packages()
# Define UI
tab2_1ui <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Alte Stichprobe einsehen"),
    fluidRow(
      tags$b("Diese Akten wurden als Grundgesamheit ausgewählt."),
      br(),
      br(),
      sidebarPanel(
        #actionButton(ns("filter_dropdown_1"), "Filtern", icon = icon("filter")),
        #conditionalPanel(
          #condition = "input.filter_dropdown % 2 != 0",
          #ns = ns,
          uiOutput(ns("column_dropdown_1")),
          #textOutput(ns("text6")),
          checkboxGroupInput(ns("value_dropdown_1"), label = NULL, choices = c())
          #uiOutput(ns("value_dropdown_1"))
       # )
      ),
      mainPanel(
        textOutput(ns("pop_count_1")),
        tableOutput(ns("filtered_table_1")),
      )
    ), 
    fluidRow(
      br(),
      tags$b("Hier sieht man die Gruppierung für die Schichten und die resultierende Kreuztabelle."),
      br(),
      br(),
      mainPanel(
        uiOutput(ns("strat_layer_buttons_ui_1")),
        #actionButton(ns("add_strat_layer_button_1"), "Schicht hinzufügen", 
        #             icon = icon("plus")),
        
        tabsetPanel(id = ns("strata_rename_input_ui_1"))
      ),
      sidebarPanel(
        tags$b("Wählen Sie die Spalten aus, die Sie einsehen möchten."),
        uiOutput(ns("crosstab_columns_1")),
        tableOutput(ns("crosstable_1"))
      )
    )
  )
    
  
}

tab2_1server <- function(id, data, old) {
  moduleServer(id, function(input, output, session) {
    
    
    filtered_data <- reactiveVal(NULL)
    observeEvent(data(), {
      filtered_data(data())
    })
    
    dataset <- reactiveVal(NULL)
    observeEvent(data(), {
      dataset(data())
    })
    
    ########## tab 3
    ################################################################################    
    #when the old sample is loaded, existing values are included 
    observeEvent(old(), {
      ns <- session$ns
      
      # this controls number of value choices in dropdown menu. If this is not done,
      #the app can crash or struggle with rendering when a column with many unique
      # values (like an id column) is selected for filtering
      max_choices = 100
      
      # Saving the data uploaded in another tab and passes to this module.
      
      #Save the old sample list in this tab
      # old_sample <- reactiveVal(NULL)
      # observeEvent(old(), {
      #   old_sample(old())
      # })
      
      old_sample <- old()
      
      # output$text6 <- renderText({
      #   paste(value_choices())
      #   #paste("Bitte klicken Sie eine der beliebigen Objekte an, um die Auswahl der letzten Stichprobe anzuzeigen.")
      # })
      
      # Defining the column and the column values by which to filter
      # selected_column: string of column selected for filtering
      # selected_values: character vector of values selected from that column
      # value_choices: the possible values of the selected column plus "Alle auswählen"
      selected_column <- reactiveVal(NULL)
      selected_values <- reactiveVal(NULL)
      value_choices <- reactiveVal(NULL)
      
      selected_column(old_sample[["selected_column"]])
      selected_values(old_sample[["selected_values"]])
      value_choices(old_sample[["value_choices"]])
      
      # renders select input based on column names in dataset
      output$column_dropdown_1 <- renderUI({
        selectInput(ns("column_dropdown_1"), label = "Diese Spalte wurde zum filtern ausgewählt",
                    choices = selected_column())
      })
      
      observeEvent(value_choices(), {
        #selected_values()
      updateCheckboxGroupInput(inputId = "value_dropdown_1",
                               choices = value_choices(),
                               selected = selected_values())
      })
      
      # filters the population
      filtered_data <- reactive({
        req(selected_values())
        subset(dataset(), dataset()[[selected_column()]] %in% selected_values())
      })
      
      # displays number of rows in filtered data
      output$pop_count_1 <- renderText({
        paste("Zeilen:", nrow(filtered_data()))
      })
      
      # Showing the filtered table
      output$filtered_table_1 <- renderTable({
        head(filtered_data(), 30)
      })
      
      # return filtered data
      #return(filtered_data)
    }, ignoreNULL = TRUE)
    
    ###########################################################################
    ######### end tab 3
    
    ###########################################################################
    ######### tab 4
    
    
    # All the relevant information for stratification
    # ids: character vector of layer ids (layer_1, layer_2, etc.)
    # columns: named list with the names of columns selected for stratification.
    #   The names correspond to ids 
    # data_types: Named list indiating for each column whether the user has selected
    #   categorical or numerical values ("categorical" or "numerical")
    # categories: Named list of named lists of lists. The top-layer names correspond to layer_ids
    #   Each top layer list element contains a named list in which the names define
    #   a category name and each element is a list defining the category. 
    # data: The computed data with only the selected columns and the applied 
    #   categorizations
    # sel_kind: Created here, but values are only inserted in tab 5. Named list
    #   indicating for each column how the user defines the selection probabilities
    #   or strata sizes in UI. ("proportional", "sample" or "population"). Names
    #   correspond to layer_ids
    # sel_params: Also computed in tab 5. The relevant parameters for specifying
    #   strata sizes using the R package.
    
    # strat_layers <- reactiveValues(ids = c(), columns = list(), data_types = list(), 
    #                                categories = list(), data = NULL,
    #                                sel_kind = list(), sel_params = list())
    
    
    # Adding a new stratification layer
    
    observeEvent(old(), {
      # Create new UI for defining layer and adding it to list of tabs
      ns <- session$ns
      
      old1 <- old()
      
      old_sample <- reactiveValues(ids = old1$ids, columns = old1$columns, data_types = old1$data_types, 
                                     categories = old1$categories,
                                     sel_kind = old1$sel_kind, sel_params = old1$sel_params)
      
      
      #layer_id <- paste0("layer_", length(old_sample$ids) + 1)
      
      #column values which haven't been selected yet so no columns
      # is selected for two stratification layers
      #sc_vec <- as.character(old_sample$columns)
      #unselected_cols <- setdiff(colnames(filtered_data()), sc_vec)
      
      # Creates a new tab for the tabsetpanel. This tab containts the UI
      # to define a stratification layer. The defined parameters are defined
      # and saved. Inserts tab into tabsetpanel
      # TODO: More parameters besides the column name
      lapply(old_sample$ids, function(layer_1){
        
        new_def_layer_ui_1 <- tabPanel(old_sample$columns[layer_1],
                                       define_layer_ui_1(ns(paste0("def_", layer_1)), 
                                                       old_sample, layer_1))
        # TODO: also take the information defining the categories
        
        layer_define_output <- define_layer_server_1(paste0("def_", layer_1), filtered_data, old_sample, layer_1)
        insertTab(inputId = "strata_rename_input_ui_1", new_def_layer_ui_1)
        
      })
      
      
      
      # observe({
      #   old_sample$columns[[layer_id]] <- layer_define_output$name
      #   old_sample$data_types[[layer_id]] <- layer_define_output$data_type
      #   old_sample$categories[[layer_id]] <- layer_define_output$categories
      # })
    
      #Shows the buttons on top
      # Outputting strat layer buttons UI
      # output$strat_layer_buttons_ui_1 <- renderUI({
      #   ns <- session$ns
      #   lapply(old_sample$ids, function(button_id) {
      #     column_name <- old_sample$columns[[old_sample$ids]]
      #     actionButton(inputId = ns(paste0("button_", button_id)), label = column_name)
      #   })
      # })
      
      # Event handler for strat layer button click. Switches to the corresponding 
      # tab in the hidden tabset
      # observeEvent(old_sample$ids, {
      #   ns <- session$ns
      #   lapply(old_sample$ids, function(layer_id){
      #     button_id <- paste0("button_", layer_id)
      #     observeEvent(input[[button_id]], {
      #       updateTabsetPanel(inputId = "strata_rename_input_ui_1", selected = ns(paste0("panel_def_", layer_id)))
      #   })
      # })
      # })
      
#working###############################################################################        
      # Applies the categorization to the data and saves a data frame of only the 
      # selected columns with the created categories
      #observe({
        cols <- lapply(old_sample$ids, function(id){
          #req(old_sample$data_types[[id]], unlist(old_sample$categories[[id]]))
          if(old_sample$data_types[[id]] == "categorical"){
            column_name <- old_sample$columns[[id]]
            categories <- old_sample$categories[[id]]
            data_cat <- select_groups(filtered_data(), column_name, categories,
                                      "new_column")
            data_cat[["new_column"]]
          }else{
            column_name <- old_sample$columns[[id]]
            categories <- old_sample$categories[[id]]
            breaks <- c(lapply(categories, function(category) category[[1]]), Inf)
            col_categorized <- cut(filtered_data()[[column_name]],
                                   breaks=breaks, right = FALSE, include.lowest = TRUE)
            col_categorized
          }
        })

         names(cols) <- old_sample$columns
         old_sample$data <- data.frame(cols)
      #})
      
      
      # Select columns for crosstabs
      output$crosstab_columns_1 <- renderUI({
        #req(input$strata_rename_input_ui_1)
        ns <- session$ns
        choices = colnames(old_sample$data)
        fluidRow(column(6, selectInput(ns("ct_column_one"), label = "1. Spalte auswählen", choices = choices)),
                 column(6, selectInput(ns("ct_column_two"), label = "2. Spalte auswählen", choices = choices))
        )
      })
      
      # Creating crosstable of chosen row. 
      # TODO: actually base this on all rows for which there is a strat layer
      output$crosstable_1 <- renderTable({
        #req(input$ct_column_one, input$ct_column_two, old_sample$data)
        data <- old_sample$data
        col_one <- data[[input$ct_column_one]]
        col_two <- data[[input$ct_column_two]]
        
        ct <- table(col_one, col_two)
        as.data.frame.matrix(ct)
      }, rownames = T, striped = TRUE, bordered = TRUE
      )
      
    })
    
    ############################################################################
    
    ###########################################################################
    ######### end tab 4
    
    #h##########################################################################
    ######### tab 5 
    # 
    # 
    # # table of computed strata sizes (computed based on inputs)
    # strata <- reactiveVal(NULL)
    # 
    # # sample size (computed based on user input)
    # sample_size <- reactiveVal(NULL)
    # 
    # observeEvent(input$sample_size, {
    #   sample_size(input$sample_size)
    #   
    # })
    # 
    # 
    # # renders the tabs for the tabset panel in which the selection probabilities
    # # are defined
    # output$define_selection_probs_ui <- renderUI({
    #   ns <- session$ns
    #   tabs <-  lapply(strat_layers$ids, function(layer_id) {
    #     col_name <- strat_layers$columns[[layer_id]]
    #     tabPanel(title = col_name, value = layer_id,
    #              selection_probability_ui(ns(paste0("sp_", layer_id)), col_name))
    #   })
    #   do.call(tabsetPanel, tabs)
    # })
    # 
    # 
    # # computes the unique values of each column in the categorized data set
    # observe({
    #   for(id in strat_layers$ids){
    #     req(strat_layers$columns[[id]] %in% colnames(strat_layers$data))
    #     strat_layers$unique_vals[[id]] <- unique(strat_layers$data[[strat_layers$columns[[id]]]])
    #   }
    # })
    # 
    # 
    # # creating moduleservers for the selection probability logic and saving results
    # observe({
    #   ns <- session$ns
    #   for (layer_id in strat_layers$ids){
    #     col_name <- strat_layers$columns[[layer_id]]
    #     req(strat_layers$unique_vals[[layer_id]])
    #     ret <- selection_probability_server(paste0("sp_", layer_id), strat_layers$unique_vals[[layer_id]])
    #     observe({
    #       strat_layers$sel_kind[[layer_id]] = ret$kind
    #       strat_layers$sel_params[[layer_id]] = ret$vec
    #     })
    #     
    #   }
    # })
    # 
    # 
    # # Gathering the inputs for stratification size computation via package and 
    # # putting inputs into named list for function call with do.call()
    # observe({
    #   req(strat_layers$data, sample_size(), unlist(strat_layers$sel_kind))
    #   args <- list(
    #     x = strat_layers$data,
    #     sample_size = sample_size(),
    #     strat_names = unlist(colnames(strat_layers$data)),
    #     ratio_types = unlist(strat_layers$sel_kind)
    #   )
    #   ratios <- lapply(strat_layers$ids, function(id){
    #     strat_layers$sel_params[[id]]
    #   })
    #   
    #   names(ratios) <- strat_layers$columns
    #   args <- c(args, ratios)
    #   str <- do.call(strata_sizes, args)
    #   strata(str)
    # })
    # 
    # 
    # # rendering the calculated strata sizes
    # output$strata <- renderTable({
    #   strata()
    # }
    # )
    ###########################################################################
    ######### end tab 5
    
    #return(dataset)
  })
}