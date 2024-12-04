library(shiny)
library(DT)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(stringr)

#' ### Imports
#' * plot_univariate
#' * filter_data
#' * summarise_col
#' * MAX_VAL
#' * ERROR_MESSAGE
source("modules/utils.R")


# UI function for selection module
filter_ui <- function(id) {
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
        fluidRow(column(1), column(11,
          uiOutput(ns("filter_selector")),
        )),
        hr(),
        actionButton(ns("filter_btn"), "Filter  anwenden"),
        actionButton(ns("reset_btn"), "Alle Filter zurücksetzen"),
        hr(),
        dataTableOutput(ns("column_summary"))  # Summary output
      ),

      # Main Panel for Plots
      mainPanel(
        box(
          title = "Merkmalsverteilung (Haputmerkmal)",
          width = 12,
          plotlyOutput(ns("dist_plot"),
                       height = "80vh"),
          switchInput(
            inputId = ns("log_scale"),
            label = "Log-Skala",
            value = FALSE,
            onLabel = "An",
            offLabel = "Aus"
          ),
        )
      )
    )
  )
}


# Server function for dashboard module
filter_server <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {

    # reactive values to store currently selected features and data
    filtered_data <- reactiveVal(NULL)
    selected_col <- reactiveVal(NULL)

    # reactive value to store whether a column is numeric or categorical
    num <- reactiveVal(NULL)

    # reactive value representing whether a continue button was pressed
    continue <- reactiveVal(FALSE)

    # output of this layer
    output_data <- reactiveVal(NULL)
    observeEvent(csv_data(), {
      output_data(csv_data())
    })

    # Observe and update the dropdown choices based on column names in csv_data
    observeEvent(csv_data(), {
      updateSelectInput(
        session,
        "column_selector",
        choices = names(csv_data())
      )
    })

    # handle continue button presses
    observeEvent(input$continue, {
      continue(TRUE)
    })
    # handle unpress condition for continue buttons
    observeEvent(input$column_selector, {
      continue(FALSE)
    })

    # render the filter selector depending on the group column
    output$filter_selector <- renderUI({
      req(output_data(), input$column_selector)

      col_name <- input$column_selector
      col <- output_data()[[col_name]]
      col_uniq <- unique(col)
      ns <- session$ns

      if (is.numeric(col)) {
        # store current col in reactive value
        selected_col(col_name)
        # set flag for numeric column
        num(TRUE)

        min <- min(col)
        max <- max(col)
        ui <- fluidRow(column(12,
          numericInput(ns("filter_selector_min"),
            "Minimum:",
            value = min,
            min = min,
            max = max
          ),
          numericInput(ns("filter_selector_max"),
            "Maximum:",
            value = max,
            min = min,
            max = max
          )
        ))
      } else if (length(col_uniq) > MAX_VAL && !continue()) {
        selected_col(NULL)
        # render warning
        ui <- fluidRow(column(12,
          HTML(ERROR_MESSAGE),
          br(), br(),
          actionButton(ns("continue"), "Fortfahren")
        ))
      } else {
        selected_col(col_name)
        num(FALSE)
        ui <- pickerInput(ns("filter_selector_cat"),
          label = "Filter:",
          choices = col_uniq,
          selected = col_uniq,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      }
      ui
    })

    # apply filter from the numeric filter selector
    observeEvent(input$filter_selector_min | input$filter_selector_max, {
      req(output_data(), selected_col(), input$filter_selector_min,
          input$filter_selector_max)

      filter_vals <- list(min = input$filter_selector_min,
                          max = input$filter_selector_max)
      filtered_data(filter_data(
        output_data(),
        group_col = selected_col(),
        filter_vals = filter_vals,
        numeric = TRUE
      ))
    })

    # apply filter from the categorical filter selector
    observeEvent(input$filter_selector_cat, {
      req(output_data(), selected_col(), input$filter_selector_cat)

      filtered_data(filter_data(
        output_data(),
        group_col = selected_col(),
        filter_vals = input$filter_selector_cat,
        numeric = FALSE
      ))
    })

    # apply filter and store it in reactive value
    observeEvent(input$filter_btn, {
      output_data(filtered_data())
    })

    # apply reset
    observeEvent(input$reset_btn, {
      output_data(csv_data())
    })

    # Render the column summary based on the selected column
    output$column_summary <- renderDataTable({
      req(filtered_data(), selected_col())

      # Calculate and display summary based on data type
      summarise_col(filtered_data(), selected_col())
    })

    # Render the distribution plot based on selected column and data type
    output$dist_plot <- renderPlotly({
      req(filtered_data(), selected_col())

      # get distribution plot (numeric/categorical)
      fig <- plot_univariate(filtered_data(), selected_col())

      # Apply log scale to y-axis if switch is enabled
      if (input$log_scale) {
        fig <- fig %>% layout(yaxis = list(type = "log"))
      }
      fig
    })

    return(list(data = output_data))
  })
}
