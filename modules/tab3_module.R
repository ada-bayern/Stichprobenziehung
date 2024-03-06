
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
        br(),
        uiOutput(ns("strat_layer_buttons_ui")),
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen", 
                     icon = icon("plus")),
        uiOutput(ns("strata_rename_input_ui")),
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
    
    # Storing the buttons for stratification layersn, names of layers, and
    # number of layers
    strat_layer_buttons <- reactiveVal(NULL)
    strat_layer_names <- reactiveVal(NULL)
    strat_layer_counter <- reactiveVal(0)
                 
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
      counter <- strat_layer_counter() + 1
      strat_layer_counter(counter)
      
      # Creating a new button for new strata
      ns <- session$ns
      new_layer_button <- actionButton(ns(paste0("strat_layer_button_", counter)), 
                                       paste("Schicht", counter))
      
      # Add the new strat layer button to the list of layer buttons
      strat_buttons_list <- c(isolate(strat_layer_buttons()), list(new_layer_button))
      strat_layer_buttons(strat_buttons_list)
      
      output$strat_layer_buttons_ui <- renderUI({
        req(strat_layer_buttons())
        strat_layer_buttons()
      })
    })
    
    # Outputting strat layer buttons UI
    
    
    # Event handler for strat layer button click
    observe({
      # Checking for a strata button being pushed
      layer_names_list <- lapply(seq_along(strat_layer_buttons()), function(i) {
        ns <- session$ns
        layer_button_id <- paste0("strat_layer_button_", i)
        input_id <- paste0("layer_name_input_", i)
        req(input[[layer_button_id]])
        if (input[[layer_button_id]] > 0) {
          # Allows for chosing a row base a stratification layer on
          output$strata_rename_input_ui <- renderUI({
            ns <- session$ns
            selectInput(inputId = ns(input_id), "Schicht auswählen", 
                        choices = colnames(dataset()))
          })
          observe({
            updateActionButton(session, layer_button_id, label=input[[input_id]])
          })
        } else {
          NULL
        }
      })
      
      strat_layer_names(unlist(strat_layer_names))
    })
    
  })
  
}