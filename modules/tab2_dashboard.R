library(shiny)
library(DT)
library(shinyWidgets)
library(plotly)

#' ### Imports
#' * plot_univariate
#' * plot_bivariate
#' * filter_data
#' * summarise_col
#' * MAX_VAL
#' * ERROR_MESSAGE
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
        uiOutput(ns("error_col")),
        selectInput(
          inputId = ns("column_selector2"),
          label = "Gruppierungsmerkmal:",
          choices = NULL,  # Placeholder, will be updated in the server
          selected = NULL
        ),
        fluidRow(column(1), column(11,
          uiOutput(ns("filter_selector")),
        )),
        hr(),
        DTOutput(ns("column_summary"))  # Summary output
      ),

      # Main Panel for Plots
      mainPanel(
        box(
          title = "Merkmalsverteilung (Hauptmerkmal)",
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
        ),
        hr(),
        box(
          title = "Bivariate Merkmalsverteilung",
          width = 12,
          plotlyOutput(ns("bivariate_plot"),
                       height = "80vh")
        )
      )
    )
  )
}


# Server function for dashboard module
dashboard_server <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # reactive values to store currently selected features and data
    filtered_data <- reactiveVal(NULL)
    main_col <- reactiveVal(NULL)
    group_col <- reactiveVal(NULL)

    # reactive value to store whether a column is numeric or categorical
    num <- reactiveVal(NULL)

    # reactive values representing whether a continue button was pressed
    continue_col <- reactiveVal(FALSE)
    continue_col2 <- reactiveVal(FALSE)

    # Observe and update the dropdown choices based on column names in csv_data
    observeEvent(csv_data(), {
      updateSelectInput(
        session,
        "column_selector",
        choices = names(csv_data())
      )
      updateSelectInput(
        session,
        "column_selector2",
        choices = names(csv_data())
      )
    })

    # handle continue button presses
    observeEvent(input$continue_col, {
      continue_col(TRUE)
    })
    observeEvent(input$continue_col2, {
      continue_col2(TRUE)
    })
    # handle unpress condition for continue buttons
    observeEvent(input$column_selector, {
      continue_col(FALSE)
    })
    observeEvent(input$column_selector2, {
      continue_col2(FALSE)
    })

    # handle columns with more than MAX_VAL categorical values
    output$error_col <- renderUI({
      req(input$column_selector)
      col <- input$column_selector
      vals <- unique(csv_data()[[col]])

      # if column contains too many categorical values and continue button was
      # not pressed yet
      if (length(vals) > MAX_VAL && !is.numeric(vals) && !continue_col()) {
        # return warning
        ui <- fluidRow(column(12,
          HTML(ERROR_MESSAGE),
          br(), br(),
          actionButton(ns("continue_col"), "Fortfahren"),
          hr()
        ))
        main_col(NULL)
      } else {
        # store current col in reactive value
        main_col(col)
        # return no error
        ui <- hr()
      }
      ui
    })

    # render the filter selector depending on the group column
    output$filter_selector <- renderUI({
      req(csv_data(), input$column_selector2)

      col_name <- input$column_selector2
      col <- csv_data()[[col_name]]
      col_uniq <- unique(col)

      if (is.numeric(col)) {
        # store current col in reactive value
        group_col(col_name)
        # set flag for numeric column
        num(TRUE)

        min <- min(col)
        max <- max(col)
        # render Min-Max-Input
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
      } else if (length(col_uniq) > MAX_VAL && !continue_col2()) {
        group_col(NULL)
        # render warning
        ui <- fluidRow(column(12,
          HTML(ERROR_MESSAGE),
          br(), br(),
          actionButton(ns("continue_col2"), "Fortfahren")
        ))
      } else {
        group_col(col_name)
        num(FALSE)
        # render categorical input
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
      req(csv_data(), group_col(), input$filter_selector_min,
          input$filter_selector_max)

      filter_vals <- list(min = input$filter_selector_min,
                          max = input$filter_selector_max)
      filtered_data(filter_data(
        data = csv_data(),
        group_col = group_col(),
        filter_vals = filter_vals,
        numeric = TRUE
      ))
    })

    # apply filter from the categorical filter selector
    observeEvent(input$filter_selector_cat, {
      req(csv_data(), group_col(), input$filter_selector_cat)

      filtered_data(filter_data(
        data = csv_data(),
        group_col = input$column_selector2,
        filter_vals = input$filter_selector_cat,
        numeric = FALSE
      ))
    })

    # Render the column summary based on the selected column
    output$column_summary <- renderDT({
      req(filtered_data(), main_col())

      # Calculate and display summary based on data type
      summarise_col(filtered_data(), main_col())
    })

    # Render the distribution plot based on selected column and data type
    output$dist_plot <- renderPlotly({
      req(filtered_data(), main_col())

      # get distribution plot (numeric/categorical)
      fig <- plot_univariate(filtered_data(), main_col())

      # Apply log scale to y-axis if switch is enabled
      if (input$log_scale) {
        fig <- fig %>% layout(yaxis = list(type = "log"))
      }
      fig
    })

    # Render the bivariate behavior of two selected columns based on
    # their data types
    output$bivariate_plot <- renderPlotly({
      req(filtered_data(), main_col(), group_col())

      # get distribution plot (numeric/categorical)
      plot_bivariate(filtered_data(), main_col(), group_col())
    })
  })
}
