
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
          checkboxGroupInput(ns("value_dropdown"), label = NULL, choices = c())
        )
      ),
      mainPanel(
        textOutput(ns("pop_count")),
        tableOutput(ns("filtered_table")),
      )
    )
  )
}

# Define server logic
tab3server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # this controls number of value choices in dropdown menu. If this is not done,
    #the app can crash or struggle with rendering when a column with many unique
    # values (like an id column) is selected for filtering
    max_choices = 100
    
    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(data(), {
      dataset(data())
    })
    
    # Defining the column and the column values by which to filter
    selected_column <- reactiveVal(NULL)
    selected_values <- reactiveVal(NULL)
    value_choices <- reactiveVal(NULL)
  
    # renders select input based on column names in dataset
    output$column_dropdown <- renderUI({
      selectInput(ns("column_dropdown"), label = "Spalte zum Filtern auswählen",
                  choices = colnames(data()))
    })
                 
    # Updates selected column
    observeEvent(input$column_dropdown, {
      selected_column(input$column_dropdown)
    })
    
    # updates the values to filter by when column is selected
    observeEvent(selected_column(),{
      choices = c("Alle auswählen", unique(data()[[selected_column()]]))
      choices = head(choices[!is.na(choices)], max_choices)
      value_choices(choices)
      selected_values(choices)
    })
    
    # displays the changed value options
    observeEvent(value_choices(), {
      updateCheckboxGroupInput(inputId = "value_dropdown",
                               choices = value_choices())
    })
    
    # Selecting all elements when user selects "select all"
    # Also unselects "select all" when user unselects at least one element
    observeEvent(input$value_dropdown, {
      if(!"Alle auswählen" %in% selected_values()){
        if("Alle auswählen" %in% input$value_dropdown){
          selected_values(c("Alle auswählen", value_choices()))
        } else {
          selected_values(input$value_dropdown)
        }
      } else {
        if(any(!value_choices() %in% input$value_dropdown)){
          selected_values(input$value_dropdown[input$value_dropdown != "Alle auswählen"])
        } else {
          selected_values(input$value_dropdown)
        }
      }
    })
    
    # renders selected values as they change 
    observeEvent(selected_values(), {
      updateCheckboxGroupInput(inputId = "value_dropdown", 
                               selected = selected_values())
    })
    
    # filters the population
    filtered_data <- reactive({
      req(selected_values())
      subset(dataset(), dataset()[[selected_column()]] %in% selected_values())
    })
    
    # displays number of rows in filtered data
    output$pop_count <- renderText({
      paste("Zeilen:", nrow(filtered_data()))
    })
    
    # Showing the filtered table
    output$filtered_table <- renderTable({
      head(filtered_data(), 100)
    })
    
    # return filtered data
    return(filtered_data)
    
  })   
}