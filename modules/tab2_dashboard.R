library(shiny)
library(shinyjs)
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(stringr)

source("modules/utils.R")



# UI function for dashboard module
dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Datenübersicht"),

    # Sidebar Panel
    sidebarLayout(
      # Sidebar for actions and options
      sidebarPanel(
        selectInput(
          inputId = ns("column_selector"),
          label = "Hauptmerkmal:",
          choices = NULL,  # Placeholder, will be updated in the server
          selected = NULL
        ),
        selectInput(
          inputId = ns("column_selector2"),
          label = "Gruppierungsmerkmal:",
          choices = NULL,  # Placeholder, will be updated in the server
          selected = NULL
        ),
        pickerInput(
          inputId = ns("filter_selector"),
          label = "Filter:",
          choices = NULL,  # Placeholder, will be updated in the server
          selected = NULL, # Placeholder, will be updated in the server
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        ),
        switchInput(
          inputId = ns("log_scale"),
          label = "Log-Skala",
          value = FALSE,
          onLabel = "An",
          offLabel = "Aus"
        ),
        hr(),
        DTOutput(ns("column_summary"))  # Summary output
      ),

      # Main Panel for Plots
      mainPanel(
        box(
          title = "Merkmalsverteilung (Merkmal 1)",
          width = 12,
          plotlyOutput(ns("dist_plot"),
                       height = "90vh")
        ),
        hr(),
        box(
          title = "Bivariate Merkmalsverteilung",
          width = 12,
          plotlyOutput(ns("bivariate_plot"),
                       height = "90vh")
        )
      )
    )
  )
}


# Server function for dashboard module
dashboard_server <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {

    # Observe and update the dropdown choices based on column names in csv_data
    observe({
      updateSelectInput(
        session,
        "column_selector",
        choices = names(csv_data())
      )
    })

    observe({
      updateSelectInput(
        session,
        "column_selector2",
        choices = names(csv_data())
      )
    })

    # Observe and update the dropdown choices based on column names in map_data
    observeEvent(input$column_selector2, {
      filter_col <- input$column_selector2
      choices_data <- csv_data()[[filter_col]]

      # Check if the column is numeric
      if (is.numeric(choices_data)) {
        # Define the bin width or number of bins you want
        # max <- max(choices_data, na.rm = TRUE)
        # min <- min(choices_data, na.rm = TRUE)
        # bins <- seq(min, max, by = (max - min) / 20)

        # # Create binned data
        # choices_list <- cut(choices_data,
        #                     breaks = bins,
        #                     include.lowest = TRUE,
        #                     labels = FALSE)
        # choices_list <- unique(choices_list)  # Get unique bins
        # choices_list <- sort(choices_list)

        # # Convert bins to a more readable format
        # choices_list <- sapply(choices_list, function(x) {
        #   range_start <- bins[x]
        #   range_end <- bins[x + 1]
        #   paste(range_start, "-", range_end)
        # })
        choices_list <- c(0)
      } else {
        choices_list <- unique(choices_data)
      }

      # Choose court to filter data by
      updatePickerInput(
        session,
        "filter_selector",
        choices = choices_list,
        selected = choices_list
      )
    })

    # Render the column summary based on the selected column
    output$column_summary <- renderDT({
      main_col <- input$column_selector
      group_col <- input$column_selector2
      filter_vals <- input$filter_selector
      data <- filter_data(csv_data(), group_col, filter_vals)

      # Calculate and display summary based on data type
      summarise_col(data, main_col)
    })

    # Render the distribution plot based on selected column and data type
    output$dist_plot <- renderPlotly({
      main_col <- input$column_selector
      group_col <- input$column_selector2
      filter_vals <- input$filter_selector
      data <- filter_data(csv_data(), group_col, filter_vals)

      # get distribution plot (numeric/categorical)
      fig <- plot_univariate(data, main_col)

      # Apply log scale to y-axis if switch is enabled
      if (input$log_scale) {
        fig <- fig %>% layout(yaxis = list(type = "log"))
      }
      fig
    })

    # Render the bivariate behavior of two selected columns based on
    # their data types
    output$bivariate_plot <- renderPlotly({
      main_col <- input$column_selector
      group_col <- input$column_selector2
      filter_vals <- input$filter_selector
      data <- filter_data(csv_data(), group_col, filter_vals)

      # get distribution plot (numeric/categorical)
      plot_bivariate(data, main_col, group_col)
    })
  })
}
