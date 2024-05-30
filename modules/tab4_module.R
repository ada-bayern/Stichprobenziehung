
source("modules/define_layer_module.R")
source("modules/selection_probability_module.R")
library(gtsummary)
source(file.path("../aktenstichprobe/R", "select_groups.R"))

install.packages()
# Define UI
tab4ui <- function(id){
  ns <- NS(id)
  # Defining the layout elements. Action button to add new stratification layers
  # adds button for each layer. These buttons are tied to dynamically generated
  # layouts for defining stratification layers
  # In main panel, a crosstable of strata can be displayed.
  fluidPage(
    titlePanel("Stichprobenziehung definieren"),
    sidebarLayout(
      mainPanel(
        uiOutput(ns("strat_layer_buttons_ui")),
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen", 
                     icon = icon("plus")),
        tabsetPanel(id = ns("strata_rename_input_ui"), type="hidden")
      ),
      sidebarPanel(
        uiOutput(ns("crosstab_columns")),
        tableOutput(ns("crosstable"))
      )
    )
  )
}


# Define server logic
tab4server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(data(), {
      dataset(data())
    })
    
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
    # cols_categorized: list of the columns with categorizations applied
    # sel_kind: Created here, but values are only inserted in tab 5. Named list
    #   indicating for each column how the user defines the selection probabilities
    #   or strata sizes in UI. ("proportional", "sample" or "population"). Names
    #   correspond to layer_ids
    # sel_params: Also computed in tab 5. The relevant parameters for specifying
    #   strata sizes using the R package.
    strat_layers <- reactiveValues(ids = c(), columns = list(), data_types = list(), 
                                   categories = list(), data = NULL,
                                   cols_categorized = list(),
                                   sel_kind = list(), sel_params = list())
    
    
    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {
      
      # Create new UI for defining layer and adding it to list of tabs
      ns <- session$ns
      
      layer_id <- paste0("layer_", length(strat_layers$ids) + 1)
      strat_layers$ids <- c(strat_layers$ids, layer_id)
      
      #column values which haven't been selected yet so no columns
      # is selected for two stratification layers
      sc_vec <- strat_layers$columns
      unselected_cols <- setdiff(colnames(dataset()), sc_vec)
      
      # Creates a new tab for the tabsetpanel. This tab containts the UI
      # to define a stratification layer. The defined parameters are defined
      # and saved. Inserts tab into tabsetpanel
      # TODO: More parameters besides the column name
      new_def_layer_ui <- tabPanel(ns(paste0("panel_def_", layer_id)),
                                   define_layer_ui(ns(paste0("def_", layer_id)), 
                                                   unselected_cols))
      # TODO: also take the information defining the categories
      layer_define_output <- define_layer_server(paste0("def_", layer_id), dataset)
      insertTab(inputId = "strata_rename_input_ui", new_def_layer_ui)
      observe({
        strat_layers$columns[[layer_id]] <- layer_define_output$name
        strat_layers$data_types[[layer_id]] <- layer_define_output$data_type
        strat_layers$categories[[layer_id]] <- layer_define_output$categories
      })
      
    })
    
    # Outputting strat layer buttons UI
    output$strat_layer_buttons_ui <- renderUI({
      ns <- session$ns
      lapply(strat_layers$ids, function(button_id) {
        column_name <- strat_layers$columns[[button_id]]
        actionButton(inputId = ns(paste0("button_", button_id)), label = column_name)
      })
    })
    
    # Event handler for strat layer button click. Switches to the corresponding 
    # tab in the hidden tabset
    observeEvent(strat_layers$ids, {
      ns <- session$ns
      lapply(strat_layers$ids, function(layer_id){
        button_id <- paste0("button_", layer_id)
        observeEvent(input[[button_id]], {
          updateTabsetPanel(inputId = "strata_rename_input_ui", selected = ns(paste0("panel_def_", layer_id)))
        })
      })
    })
    
    
    categorize_column <- function(id) {
      column_name <- strat_layers$columns[[id]]
      data_type <- strat_layers$data_types[[id]]
      req(unlist(strat_layers$categories[[id]]))
      categories <- strat_layers$categories[[id]]
      
      if (data_type == "categorical") {
        strat_layers$cols_categorized[[id]] <- select_groups(
          dataset()[[column_name]], categories, NA)
      } else {
        breaks <- c(0, categories)
        strat_layers$cols_categorized[[id]] <- cut(
          dataset()[[column_name]],
          breaks = breaks, right = FALSE, include.lowest = TRUE
        )
      }
    }
    
    observe({
      lapply(strat_layers$ids, categorize_column)
    })
    
    # saves a data frame of only the selected columns with the created categories
    observeEvent(strat_layers$cols_categorized, {
      cols <- strat_layers$cols_categorized
      names(cols) <- strat_layers$columns
      strat_layers$data <- data.frame(cols)
    })
    
    # Select columns for crosstabs
    output$crosstab_columns <- renderUI({
      req(input$strata_rename_input_ui)
      ns <- session$ns
      choices = colnames(strat_layers$data)
      fluidRow(column(6, selectInput(ns("ct_column_one"), label = "1. Spalte auswählen", choices = choices)),
               column(6, selectInput(ns("ct_column_two"), label = "2. Spalte auswählen", choices = choices))
      )
    })
    
    # Creating crosstable of chosen row. 
    # TODO: actually base this on all rows for which there is a strat layer
    output$crosstable <- renderTable({
      req(input$ct_column_one, input$ct_column_two, strat_layers$data)
      data <- strat_layers$data
      col_one <- data[[input$ct_column_one]]
      col_two <- data[[input$ct_column_two]]
      
      ct <- table(col_one, col_two)
      as.data.frame.matrix(ct)
    }, rownames = T, striped = TRUE, bordered = TRUE
    )
    
    return(strat_layers)
  })
  
}