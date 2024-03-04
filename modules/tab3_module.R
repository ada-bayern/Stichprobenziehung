
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
    
    # Defining the column and the column values by which to filter
    selected_column <- reactiveVal(NULL)
    selected_values <- reactiveVal(NULL)
    
    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(data(), {
      dataset(data())
    })
                 
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
  })
  
}
