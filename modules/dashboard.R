library(shiny)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(plotly)

# TODO: german
# TODO: no verbatim
# TODO: map
# TODO: replace dots in col names
# TODO: explanation -> error

# UI function for dashboard module
dashboard_ui <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    # Sidebar Panel
    column(
      width = 4,
      box(
        title = "Options",
        width = 12,
        selectInput(
          inputId = ns("column_selector"),
          label = "Choose a column to visualize:",
          choices = NULL,  # Placeholder, choices will be updated in the server
          selected = NULL
        ),
        switchInput(
          inputId = ns("log_scale"),
          label = "Log Scale",
          value = FALSE,
          onLabel = "Log",
          offLabel = "Linear"
        )
      ),
      hr(),
      box(
        title = "Summary of Selected Column",
        width = 12,
        verbatimTextOutput(ns("column_summary"))  # Summary output
      )
    ),
    
    # Main Panel for Plots
    column(
      width = 8,
      box(
        title = "Distribution Plot",
        width = 12,
        plotlyOutput(ns("dist_plot"))  # Use Plotly for the plot
      )
    )
  )
}

# Server function for dashboard module
dashboard_server <- function(id, csv_data, map_data) {
  moduleServer(id, function(input, output, session) {
    
    # Observe and update the dropdown choices based on column names in csv_data
    observe({
      updateSelectInput(
        session,
        "column_selector",
        choices = names(csv_data())
      )
    })
    
    # Render the column summary based on the selected column
    output$column_summary <- renderPrint({
      selected_column <- input$column_selector
      
      if (is.null(selected_column) || selected_column == "") return(NULL)
      
      # Calculate and display summary based on data type
      column_data <- csv_data()[[selected_column]]
      if (is.numeric(column_data)) {
        summary(column_data)
      } else if (is.factor(column_data) || is.character(column_data)) {
        table(column_data)
      } else {
        "Selected column type not supported for summary"
      }
    })
    
    # Render the distribution plot based on selected column and data type
    output$dist_plot <- renderPlotly({
      selected_column <- input$column_selector
      
      if (is.null(selected_column) || selected_column == "") return(NULL)
      
      # Get the selected column as a symbol
      column_sym <- sym(selected_column)
      column_data <- csv_data()[[selected_column]]
      
      # Check data type and render appropriate plot
      if (is.numeric(column_data)) {
        # Histogram for numeric data
        p <- plot_ly(csv_data(), x = ~get(selected_column), type = "histogram") %>%
          layout(title = paste("Distribution of", selected_column),
                 xaxis = list(title = selected_column),
                 yaxis = list(title = "Frequency"))
        
      } else if (is.factor(column_data) || is.character(column_data)) {
        # Check unique values count
        unique_values_count <- length(unique(column_data))
        
        if (unique_values_count > 100) {
          return(list(
            text = "Error: More than 100 unique values. Plotting not possible.",
            type = "scatter",
            mode = "text"
          ))
        }
        
        # Bar plot for categorical data
        p <- plot_ly(x = ~column_data, type = "histogram") %>%
          layout(title = paste("Distribution of", selected_column),
                 xaxis = list(title = selected_column),
                 yaxis = list(title = "Count"))
        
      } else {
        # Show a message if the data type is not supported
        return(list(
          text = "Error: Selected column type not supported for plotting.",
          type = "scatter",
          mode = "text"
        ))
      }
      
      # Apply log scale to y-axis if switch is enabled
      if (input$log_scale) {
        p <- p %>% layout(yaxis = list(type = "log"))
      }
      
      p
    })
  })
}
