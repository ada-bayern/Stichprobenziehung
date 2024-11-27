library(shiny)
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(stringr)

source("modules/utils.R")


# UI function for selection module
selection_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Datenübersicht"),

    # Sidebar Panel
    sidebarLayout(
      # Sidebar for actions and options
      sidebarPanel(
        selectInput(
          inputId = ns("column_selector"),
          label = "Merkmal:",
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
        actionButton(ns("filter_btn"), "Filter unwiderruflich anwenden"),
        hr(),
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
      )
    )
  )
}


# Server function for dashboard module
selection_server <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {

    # Observe and update the dropdown choices based on column names in csv_data
    observe({
      updateSelectInput(
        session,
        "column_selector",
        choices = names(csv_data())
      )
    })

    # Observe and update the dropdown choices based on column names in map_data
    observeEvent(input$column_selector, {
      filter_col <- input$column_selector
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
        choices_list <- NULL
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

    observeEvent(input$filter_btn, {
      p <- prepare_data(input, csv_data())
      if (!is.null(p)) {
        csv_data(p$data)
      }
    })

    # Render the column summary based on the selected column
    output$column_summary <- renderDT({
      col <- input$column_selector
      filter_vals <- input$filter_selector
      data <- filter_data(csv_data(), col, filter_vals)

      # Calculate and display summary based on data type
      summarise_col(data, col)
    })

    # Render the distribution plot based on selected column and data type
    output$dist_plot <- renderPlotly({
      col <- input$column_selector
      filter_vals <- input$filter_selector
      data <- filter_data(csv_data(), col, filter_vals)

      # get distribution plot (numeric/categorical)
      fig <- plot_univariate(data, col)

      # Apply log scale to y-axis if switch is enabled
      if (input$log_scale) {
        fig <- fig %>% layout(yaxis = list(type = "log"))
      }
      fig
    })

    return(list(data = csv_data))
  })
}
