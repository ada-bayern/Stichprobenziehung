#' Module: Dashboard for Data Overview and Visualization
#'
#' This module provides a Shiny dashboard interface for data visualization and
#' exploration, utilizing interactive charting with Plotly for both univariate
#' and bivariate data distributions. It allows users to select variables for
#' visualization, apply filters, and view summary statistics of the data.
#'
#' Libraries:
#' - Utilizes Shiny for creating the app interface, DT for rendering data
#'   tables, plotly for compiling interactive plots, and shinyWidgets for
#'   enhanced UI components.
#'
#' Imports:
#' - From the modules/utils.R file, imports are plot_univariate, plot bivariate,
#'   get_filter_data, summarise_col, MAX_VAL, and ERROR_MESSAGE.
#'
#' UI Structure:
#' - `dashboard_ui`: Constructs a user interface with a sidebar and main panel
#'                   for plotting and data summarization.
#'   - Sidebar: Allows selecting main features and grouping features, applying
#'              filters, and displays a summary table.
#'   - Main Panel: Showcases distribution plots for both univariate and
#'                 bivariate data.
#'
#' Server Functionality:
#' - `dashboard_server`: Implements server-side logic to manage user
#'   interaction, dynamically updating UI components, applying data filters, and
#'   rendering plots.
#'   - Utilizes reactive values and observers to manage state and user inputs.
#'   - Automatically updates the UI based on changes in the reactive data.
#'   - Handles log scale plotting, as well as visualization for both categorical
#'     and numeric data.

# Load necessary libraries
library(shiny)
library(DT)
library(shinyWidgets)
library(plotly)
library(rintrojs)

# Source the necessary utility functions
source("modules/helpers/utils.R")
source("modules/helpers/manual.R")

# UI function for the dashboard module
dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    introjsUI(),
    actionButton(ns("info"), "Info",
                 icon = icon("question-circle")),

    titlePanel("Datenübersicht"),
    # Sidebar Layout
    sidebarLayout(
      # Sidebar for actions and options
      sidebarPanel(
        div(id = ns("cs1"),
          selectInput(
            inputId = ns("column_selector"),
            label = "Hauptmerkmal:",
            choices = NULL, # Placeholder, filled by server
            selected = NULL
          ),
          uiOutput(ns("error_col"))
        ),

        div(id = ns("cs2"),
          selectInput(
            inputId = ns("column_selector2"),
            label = "Gruppierungsmerkmal:",
            choices = NULL, # Placeholder, filled by server
            selected = NULL
          )
        ),
        fluidRow(id = ns("fs"),
          column(1),
          introBox(
            column(11, uiOutput(ns("filter_selector"))),
            data.step = 3,
            data.intro = "Hier können Sie Filter auf das Gruppierungsmerkmal
            anwenden. Je nach Typ des Gruppierungsmerkmals (kategorisch oder
            numerisch) werden unterschiedliche Filteroptionen angezeigt."
          )
        ),
        hr(),
        div(id = ns("col_sum"),
          tags$b("Kennwerte (Hauptmerkmal):"),
          div(DTOutput(ns("column_summary")),
              style = "max-height: 100vh; overflow-y: auto;")
        )
      ),
      # Main Panel for Plots
      mainPanel(
        box(id = ns("plot1"),
          title = "Merkmalsverteilung (Hauptmerkmal)",
          width = 12,
          plotlyOutput(ns("dist_plot"), height = "80vh"),
          switchInput(
            inputId = ns("log_scale"),
            label = "Log-Skala",
            value = FALSE,
            onLabel = "An",
            offLabel = "Aus"
          )
        ),

        hr(),
        box(id = ns("plot2"),
          title = "Bivariate Merkmalsverteilung",
          width = 12,
          plotlyOutput(ns("bivariate_plot"), height = "80vh")
        )

      )
    )
  )
}

# Server function for the dashboard module
dashboard_server <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Steps for Guided Tour
    steps <- reactive(data.frame(
      element = c(NA, paste0("#", ns("cs1")), paste0("#", ns("cs2")),
                  paste0("#", ns("fs")), paste0("#", ns("col_sum")),
                  paste0("#", ns("plot1")), paste0("#", ns("plot2"))),
      intro = MANUAL$dashboard
    ))

    # Info button for Guided Tour
    observeEvent(input$info, introjs(session, options = c(
      list("steps" = steps()),
      INTRO_OPTIONS
    )))

    # Reactive values to store currently selected features and filtered data
    filtered_data <- reactiveVal(NULL)
    main_col <- reactiveVal(NULL)
    group_col <- reactiveVal(NULL)
    num <- reactiveVal(NULL) # Indicator if the column is numeric
    continue_col <- reactiveVal(FALSE) # Flag for continuation
    continue_col2 <- reactiveVal(FALSE) # Flag for continuation

    # Observe CSV data changes to update column dropdowns
    observeEvent(csv_data(), {
      updateSelectInput(
        session, "column_selector",
        choices = names(csv_data())
      )
      updateSelectInput(
        session, "column_selector2",
        choices = names(csv_data())
      )
    })

    # Observe button presses for continuation
    observeEvent(input$continue_col, {
      continue_col(TRUE)
    })
    observeEvent(input$continue_col2, {
      continue_col2(TRUE)
    })

    # Revert continuation flags when new columns are selected
    observeEvent(input$column_selector, {
      continue_col(FALSE)
    })
    observeEvent(input$column_selector2, {
      continue_col2(FALSE)
    })

    # Handle columns with more than MAX_VAL categorical values
    output$error_col <- renderUI({
      req(input$column_selector)
      col <- input$column_selector
      vals <- unique(csv_data()[[col]])

      if (length(vals) > MAX_VAL && !is.numeric(vals) && !continue_col()) {
        # Warning for excessive categorical values
        ui <- fluidRow(column(12,
          HTML(ERROR_MESSAGE),
          br(), br(),
          actionButton(ns("continue_col"), "Fortfahren"),
          hr()
        ))
        main_col(NULL) # No selection until user proceeds
      } else {
        main_col(col) # Store the current column
        ui <- hr()
      }
      ui
    })

    # Render UI for the filter selector based on group column
    output$filter_selector <- renderUI({
      req(csv_data(), input$column_selector2)
      col_name <- input$column_selector2
      col <- csv_data()[[col_name]]
      col_uniq <- unique(col)

      if (is.numeric(col)) {
        group_col(col_name) # Store group column
        num(TRUE) # Set numeric flag
        fluidRow(column(12,
          numericInput(ns("filter_selector_min"), "Minimum:", value = min(col, na.rm = TRUE)),
          numericInput(ns("filter_selector_max"), "Maximum:", value = max(col, na.rm = TRUE))
        ))
      } else if (length(col_uniq) > MAX_VAL && !continue_col2()) {
        # Warning for excessive categorical values
        group_col(NULL)
        fluidRow(column(12,
          HTML(ERROR_MESSAGE),
          br(), br(),
          actionButton(ns("continue_col2"), "Fortfahren")
        ))
      } else {
        group_col(col_name)
        num(FALSE)
        # Categorical filter selector
        pickerInput(ns("filter_selector_cat"),
          label = "Filter:",
          choices = col_uniq,
          selected = col_uniq,
          multiple = TRUE,
          options = list(`actions-box` = TRUE,
                         `deselect-all-text` = "Alle abwählen",
                         `select-all-text` = "Alle auswählen",
                         `none-selected-text` = "Keine ausgewählt")
        )
      }
    })

    # Apply filters for a numeric column
    observeEvent(input$filter_selector_min | input$filter_selector_max, {
      req(csv_data(), group_col(), input$filter_selector_min,
          input$filter_selector_max)
      filter_vals <- list(min = input$filter_selector_min,
                          max = input$filter_selector_max)
      index <- get_filter_index(csv_data()[[group_col()]], filter_vals, TRUE)
      filtered_data(csv_data()[index, ])
    })

    # Apply filters for a categorical column
    observeEvent(input$filter_selector_cat, {
      req(csv_data(), group_col(), input$filter_selector_cat)
      filter_vals <- input$filter_selector_cat
      index <- get_filter_index(csv_data()[[group_col()]], filter_vals, FALSE)
      filtered_data(csv_data()[index, ])
    })

    # Render summary table for the selected column
    output$column_summary <- renderDT({
      req(filtered_data(), main_col())
      with_timeout(summarise_col(filtered_data(), main_col()),
                  timeout = 5)
    })

    # Render univariate distribution plot
    output$dist_plot <- renderPlotly({
      req(filtered_data(), main_col())
      fig <- with_timeout(plot_univariate(filtered_data(), main_col()),
                          timeout = 5)
      if (input$log_scale) {
        fig <- fig %>% layout(yaxis = list(type = "log"))
      }
      fig
    })

    # Render bivariate distribution plot
    output$bivariate_plot <- renderPlotly({
      req(filtered_data(), main_col(), group_col())
      with_timeout(plot_bivariate(filtered_data(), main_col(), group_col()),
                   timeout = 5)
    })
  })
}