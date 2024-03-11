
source("modules/define_layer_module.R")
# Define UI
tab3ui <- function(id){
  ns <- NS(id)
  # Defining the layout elements. The options for filtering are only shown when
  # the "Filter" button is pressed and disappear when it is pressed again
  fluidPage(
    titlePanel("Grundgesamtheit Auswählen"),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("filter_dropdown"), "Filtern", icon = icon("filter")),
        uiOutput(ns("strat_layer_buttons_ui")),
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen", 
                     icon = icon("plus")),
        tabsetPanel(id = ns("strata_rename_input_ui"), type="hidden"),
        conditionalPanel(
          condition = "input.filter_dropdown % 2 != 0",
          ns = ns,
          uiOutput(ns("column_dropdown")),
          uiOutput(ns("value_dropdown")),
        )
      ),
      mainPanel(
        tableOutput(ns("filtered_table"))
      )
    )
  )
}

# Define server logic
tab3server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(data(), {
      dataset(data())
    })
    
    # Defining the column and the column values by which to filter
    selected_column <- reactiveVal(NULL)
    selected_values <- reactiveVal(NULL)
    
    # Storing the buttons for stratification layers, names of layers, and
    # number of layers
    #strat_layer_buttons <- reactiveVal(NULL)
    strat_layer_ids <- reactiveValues(ids = list())
    #strat_layer_counter <- reactiveVal(0)
    
    # tabPanels of the ui for defining a layer
    #def_layer_ui_tabs <- reactiveVal(NULL)
    
    # list of module outputs
    module_outputs <- reactiveValues()
                 
    # When the filter button is clicked, show a dropwdown menu with which the 
    # user can select by which column to filter.
    observeEvent(input$filter_dropdown, {
      if (input$filter_dropdown %% 2 != 0) {
        output$column_dropdown <- renderUI({
          ns <- session$ns
          selectInput(ns("column_select"), "Spalte auswählen", 
                      choices = colnames(dataset()),
                      selected = selected_column())
        })
        
        # When a column is selected, display a checkbox list where values from
        # the selected column can be filtered by.
        observe({
          req(input$column_select)
          output$value_dropdown <- renderUI({
            ns <- session$ns
            checkboxGroupInput(ns("value_select"), "Werte auswählen", 
                               choices = unique(dataset()[[input$column_select]]),
                               selected = selected_values())
          })
        })
      # Don't display the filtering options when the filtering button has been 
      # pressed even amount of times
      } else {
        output$column_dropdown <- NULL
        output$value_dropdown <- NULL
      }
    })
    
    # Displays the filtered data
    filtered_data <- reactive({
      req(input$value_select)
      subset(dataset(), dataset()[[input$column_select]] %in% input$value_select)
    })
    
    # Saves the columns and values selecting in filtering so they are displayed 
    # (through the default value option) when the filtering options are 
    # selected again
    observe({
      if (!is.null(input$column_select)) {
        selected_column(input$column_select)
      }
      if (!is.null(input$value_select)) {
        selected_values(input$value_select)
      }
    })
    
    # Showing the filtered table
    output$filtered_table <- renderTable({
      filtered_data()
    })
    
    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {
      # Create new UI for defining layer and adding it to list of tabs
      ns <- session$ns
      
      layer_id <- paste0("layer_", length(strat_layer_ids$ids) + 1)
      strat_layer_ids$ids[[layer_id]] <- layer_id
      # Creates a new tab for the tabsetpanel. This tab containts the UI
      # to define a stratification layer. The defined parameters are defined
      # and saved. Inserts tab into tabsetpanel
      # TODO: More parameters besides the column name
      new_def_layer_ui <- tabPanel(ns(paste0("panel_def_", layer_id)),
                                   define_layer_ui(ns(paste0("def_", layer_id)), 
                                                   colnames(dataset())))
      module_outputs[[layer_id]] = define_layer_server(paste0("def_", layer_id))
      insertTab(inputId = "strata_rename_input_ui", new_def_layer_ui)
      
    })
    
    # Outputting strat layer buttons UI
    output$strat_layer_buttons_ui <- renderUI({
      ns <- session$ns
      lapply(strat_layer_ids$ids, function(button_id) {
        column_name <- module_outputs[[button_id]]()
        print(column_name)
        actionButton(inputId = ns(paste0("button_", button_id)), label = column_name)
      })
    })
    
    # Event handler for strat layer button click. Switches to the corresponding 
    # tab in the hidden tabset
    observeEvent(strat_layer_ids$ids, {
      ns <- session$ns
      lapply(strat_layer_ids$ids, function(layer_id){
        button_id <- paste0("button_", layer_id)
        observeEvent(input[[button_id]], {
          column_name <- module_outputs[[layer_id]]()
          updateTabsetPanel(inputId = "strata_rename_input_ui", selected = ns(paste0("panel_def_", layer_id)))
          })
        })
      })
    })
     
}