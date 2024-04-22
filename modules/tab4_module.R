
source("modules/define_layer_module.R")
source("modules/selection_probability_module.R")
library(gtsummary)
# Define UI
tab4ui <- function(id){
  ns <- NS(id)
  # Defining the layout elements. Action button to add new stratification layers
  # adds button for each layer. These buttons are tied to dynamically generated
  # layouts for defining stratification layers
  # In main panel, a crosstable of strata can be displayed.
  fluidPage(
    titlePanel("stichprobenziehung definieren"),
    sidebarLayout(
      sidebarPanel(
        uiOutput(ns("strat_layer_buttons_ui")),
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen", 
                     icon = icon("plus")),
        tabsetPanel(id = ns("strata_rename_input_ui"), type="hidden"),
      ),
      mainPanel(
        uiOutput(ns("crosstab_columns")),
        tableOutput(ns("crosstable")),
        fluidRow(
          # Text input for sample size
          column(2, numericInput("sample_size", "Stichprobengröße", value = 100, min = 1, max = 99999, width = "80px")),
          # Tabset for defining sampling probabilities
          uiOutput(ns("define_selection_probs_ui"))
        )
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
    
    # names of stratification layers
    strat_layers <- reactiveValues(ids = c(), columns = list(), 
                                   categories = list(), sel_kind = list(), 
                                   sel_params = list())
    
    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {
      # Create new UI for defining layer and adding it to list of tabs
      ns <- session$ns
      
      layer_id <- paste0("layer_", length(strat_layers$ids) + 1)
      strat_layers$ids <- c(strat_layers$ids, layer_id)
      
      #column values which haven't been selected yet so no columns
      # is selected for two stratification layers
      sc_vec <- lapply(strat_layers$columns, function(column){
        column()
      })
      unselected_cols <- setdiff(colnames(dataset()), sc_vec)
      
      # Creates a new tab for the tabsetpanel. This tab containts the UI
      # to define a stratification layer. The defined parameters are defined
      # and saved. Inserts tab into tabsetpanel
      # TODO: More parameters besides the column name
      new_def_layer_ui <- tabPanel(ns(paste0("panel_def_", layer_id)),
                                   define_layer_ui(ns(paste0("def_", layer_id)), 
                                                   unselected_cols))
      # TODO: also take the information defining the categories
      strat_layers$columns[[layer_id]] <- define_layer_server(paste0("def_", layer_id), dataset)
      insertTab(inputId = "strata_rename_input_ui", new_def_layer_ui)
    })
    
    
    # renders the tabs for the tabset panel in which the selection probabilities
    # are defined
    output$define_selection_probs_ui <- renderUI({
      ns <- session$ns
      tabs <-  lapply(strat_layers$ids, function(layer_id) {
          col_name <- strat_layers$columns[[layer_id]]
          tabPanel(title = col_name(), value = layer_id,
                   selection_probability_ui(ns(paste0("sp_", layer_id)), col_name))
        })
      do.call(tabsetPanel, tabs)
    })
    
    
    # creating moduleservers for the selection probability logic and saving results
    observeEvent(strat_layers$ids, {
      ns <- session$ns
      for (layer_id in strat_layers$ids){
        unique_values <- reactiveVal(unique(dataset()$Anzahl.Termine))
        ret <- selection_probability_server(paste0("sp_", layer_id), unique_values)
        strat_layers$sel_kind[[layer_id]] = ret$kind
        strat_layers$vec[[layer_id]] = ret$vec
      }
    })
    
    
    # Outputting strat layer buttons UI
    output$strat_layer_buttons_ui <- renderUI({
      ns <- session$ns
      lapply(strat_layers$ids, function(button_id) {
        column_name <- strat_layers$columns[[button_id]]
        actionButton(inputId = ns(paste0("button_", button_id)), label = column_name())
      })
    })
    
    
    # Event handler for strat layer button click. Switches to the corresponding 
    # tab in the hidden tabset
    observeEvent(strat_layers$ids, {
      ns <- session$ns
      lapply(strat_layers$ids, function(layer_id){
        button_id <- paste0("button_", layer_id)
        observeEvent(input[[button_id]], {
          #column_name <- strat_layers$columns[[layer_id]]#()
          updateTabsetPanel(inputId = "strata_rename_input_ui", selected = ns(paste0("panel_def_", layer_id)))
        })
      })
    })
    
    # Select columns for crosstabs
    output$crosstab_columns <- renderUI({
      req(input$strata_rename_input_ui)
      ns <- session$ns
      choices = c("Anzahl.Termine", "Dauer.des.Verfahrens.in.Tagen")
      fluidRow(column(6, selectInput(ns("ct_column_one"), label = "1. Spalte auswählen", choices = choices)),
               column(6, selectInput(ns("ct_column_two"), label = "2. Spalte auswählen", choices = choices))
      )
    })
    
    
    # Creating crosstable of chosen row. 
    # TODO: actually base this on all rows for which there is a strat layer
    output$crosstable <- renderTable({
      req(input$ct_column_one, input$ct_column_two, dataset())
      data <- dataset()
      col_one <- data[[input$ct_column_one]]
      col_two <- data[[input$ct_column_two]]
      
      ct <- table(col_one, col_two)
      as.data.frame.matrix(ct)
    }, rownames = T, striped = TRUE, bordered = TRUE
    )
    
  })
  
}