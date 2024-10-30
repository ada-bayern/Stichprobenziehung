library(shiny)
library(DT)
library(ggplot2)
library(shinyWidgets)
library(plotly)
library(dplyr)

# TODO: map
# TODO: Kommune auswählen
# TODO: replace dots in col names

# UI function for dashboard module
dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Datenübersicht"),

    # Sidebar Panel
    sidebarLayout(
      # Sidebar for actions and options
      sidebarPanel(
        box(
          title = "Optionen",
          width = 12,
          selectInput(
            inputId = ns("column_selector"),
            label = "Spaltenauswahl:",
            choices = NULL,  # Placeholder, will be updated in the server
            selected = NULL
          ),
          switchInput(
            inputId = ns("log_scale"),
            label = "Log-Skala",
            value = FALSE,
            onLabel = "An",
            offLabel = "Aus"
          )
        ),
        hr(),
        box(
          title = "Merkmalsübersicht",
          width = 12,
          DTOutput(ns("column_summary"))  # Summary output
        )
      ),

      # Main Panel for Plots
      mainPanel(
        box(
          title = "Merkmalsverteilung",
          width = 12,
          plotlyOutput(ns("dist_plot"))  # Use Plotly for the plot
        ),
        hr(),
        box(
          title = "Kommunale Merkmalsverteilung",
          width = 12,
          plotlyOutput(ns("map_plot"))  # Use Plotly for the plot
        )
      )
    )
  )
}

# Returns a summary of a data column of arbitrary type as a DT (datatable)
summarise_col <- function(data, selected_column) {
  column_data <- data[[selected_column]]
  if (is.numeric(column_data)) {
    # Summary for numeric data
    summary_df <- as.data.frame(t(summary(column_data))) %>%
      select("Var2", "Freq")
    colnames(summary_df) <- c("Statistik", "Wert")

  } else if (is.factor(column_data) || is.character(column_data)) {
    # Frequency table for categorical data
    summary_df <- as.data.frame(table(column_data))
    colnames(summary_df) <- c("Klasse", "Anzahl")

  } else {
    # Error message
    summary_df <- data.frame(Message = "Error: Der Datentyp des ausgewählten Merkmals ist unbekannt.") # nolint
  }

  return(datatable(summary_df, options = list(dom = "t", paging = FALSE)))
}

# Returns a plotly object displaying a distribution of a selected data column of arbitrary type
plot_col_dist <- function(data, selected_column) {
  # Get the selected column
  column_data <- data[[selected_column]]

  # Check data type and render appropriate plot
  if (is.numeric(column_data)) {
    # Histogram for numeric data
    return(plot_ly(data, x = ~get(selected_column), type = "histogram") %>%
             layout(title = paste("Verteilung von", selected_column),
                    xaxis = list(title = selected_column),
                    yaxis = list(title = "Häufung")))

  } else if (is.factor(column_data) || is.character(column_data)) {
    # Check unique values count
    unique_values_count <- length(unique(column_data))

    if (unique_values_count > 100) {
      return(plot_ly() %>%
              layout(title = "Error: Das Ausgewählte Merkmal hat zu viele Ausprägungen um geplottet zu werden.", # nolint
                     title_font_color = "red"))
    }

    # Bar plot for categorical data
    return(plot_ly(x = ~column_data, type = "histogram") %>%
             layout(title = paste("Verteilung von", selected_column),
                    xaxis = list(title = selected_column),
                    yaxis = list(title = "Anzahl")))

  } else {
    # Show a message if the data type is not supported
    return(plot_ly() %>%
             layout(title = "Error: Der Datentyp des ausgewählten Merkmals ist unbekannt.",
                    title_font_color = "red"))
  }
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
    output$column_summary <- renderDT({
      selected_column <- input$column_selector

      if (is.null(selected_column) || selected_column == "") return(NULL)

      # Calculate and display summary based on data type
      summarise_col(csv_data(), selected_column)
    })

    # Render the distribution plot based on selected column and data type
    output$dist_plot <- renderPlotly({
      selected_column <- input$column_selector

      if (is.null(selected_column) || selected_column == "") return(NULL)

      # get distribution plot (numeric/categorical)
      p <- plot_col_dist(csv_data(), selected_column)

      # Apply log scale to y-axis if switch is enabled
      if (input$log_scale) {
        p <- p %>% layout(yaxis = list(type = "log"))
      }
      p
    })
  })
}
