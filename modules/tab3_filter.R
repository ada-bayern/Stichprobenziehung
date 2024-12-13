#' Module: Data Selection and Filter Application
#'
#' This module provides a Shiny UI and server logic for data selection and
#' filtering. It allows users to interactively select features, apply filters,
#' and view data summaries and distributions in the context of a Shiny
#' application.
#'
#' Libraries:
#' - Uses Shiny for UI and server structure, DT for data tables, shinyWidgets
#'   for enhanced UI input, and plotly for interactive visuals.
#'
#' Imports:
#' - Imports plot_univariate, filter_data, summarise_col, MAX_VAL, and
#'   ERROR_MESSAGE from the utils module located in "modules/utils.R".
#'
#' UI Structure:
#' - `filter_ui`: Constructs the user interface for selecting data columns and
#'   applying filters on the selected data.
#'   - Sidebar: Facilitates feature selection, filter application, and filter
#'     reset with a summary of currently applied filters.
#'   - Main Panel: Displays a distribution plot for the selected feature.
#'
#' Server Functionality:
#' - `filter_server`: Implements the server-side logic for facilitating feature
#'   selection, applying filters, rendering plots, and maintaining a record of
#'   active filters.
#'   - Manages reactive values for current data state, selected features, and
#'     filter details.
#'   - Provides logic for both numeric and categorical feature selection and
#'     filtering.
#'   - Handles filter application and reset, updating both the UI and reactive
#'     data upon
#'     interactions.
#'   - Effectively updates and manages display elements based on filter changes.

# Load necessary libraries
library(shiny)
library(DT)
library(shinyWidgets)
library(plotly)

# Source necessary functions from external utility script
source("modules/helpers/utils.R")


# UI function for the data selection and filter module
filter_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Datenauswahl"),
    # Sidebar Layout for actions and data selection
    sidebarLayout(
      sidebarPanel(
        selectInput(
          inputId = ns("column_selector"),
          label = "Merkmal:",
          choices = NULL, # Placeholder, will be updated by server
          selected = NULL
        ),
        fluidRow(
          column(1),
          column(11, uiOutput(ns("filter_selector")))
        ),
        hr(),
        actionButton(ns("filter_btn"), "Filter anwenden"),
        actionButton(ns("reset_btn"), "Alle Filter zurücksetzen"),
        hr(),
        tags$b("Aktuelle Filter:"),
        tabsetPanel(id = ns("current_filters")),
        hr(),
        dataTableOutput(ns("column_summary")) # Displays column summary
      ),
      # Main Panel for data visualization
      mainPanel(
        box(
          title = "Merkmalsverteilung (Haputmerkmal)",
          width = 12,
          plotlyOutput(ns("dist_plot"), height = "80vh"),
          switchInput(
            inputId = ns("log_scale"),
            label = "Log-Skala",
            value = FALSE,
            onLabel = "An",
            offLabel = "Aus"
          )
        )
      )
    )
  )
}

# Server function for managing data filters and visualization
filter_server <- function(id, csv_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store selected features and applied filters
    filtered_data <- reactiveVal(NULL)
    selected_col <- reactiveVal(NULL)
    filter_vals <- reactiveVal(NULL)
    all_vals <- reactiveVal(NULL)
    numeric_filter <- reactiveVal(NULL)
    filters <- reactiveVal(list()) # Store applied filters
    num <- reactiveVal(NULL) # Identify numeric vs categorical columns
    continue <- reactiveVal(FALSE) # Continuation flag
    output_data <- reactiveVal(NULL) # Output data store

    # Initialize output data with the CSV data
    observeEvent(csv_data(), {
      output_data(csv_data())
    })

    # Dynamic update of available column choices
    observeEvent(csv_data(), {
      updateSelectInput(
        session, "column_selector",
        choices = names(csv_data())
      )
    })

    # Handle continuation button presses
    observeEvent(input$continue, {
      continue(TRUE)
    })
    # Reset on new selection
    observeEvent(input$column_selector, {
      continue(FALSE)
    })

    # Dynamic UI rendering based on selected column
    output$filter_selector <- renderUI({
      req(output_data(), input$column_selector)
      col_name <- input$column_selector
      col <- output_data()[[col_name]]
      col_uniq <- unique(col)

      if (is.numeric(col)) {
        selected_col(col_name)
        num(TRUE)
        min <- min(col)
        max <- max(col)
        all_vals(list(min = min, max = max))

        # Numeric input for range filtering
        fluidRow(column(12,
          numericInput(ns("filter_selector_min"), "Minimum:", value = min),
          numericInput(ns("filter_selector_max"), "Maximum:", value = max)
        ))
      } else if (length(col_uniq) > MAX_VAL && !continue()) {
        selected_col(NULL)

        # Warning for categorical values exceeding limit
        fluidRow(column(12,
          HTML(ERROR_MESSAGE),
          br(), br(),
          actionButton(ns("continue"), "Fortfahren")
        ))
      } else {
        selected_col(col_name)
        num(FALSE)
        all_vals(col_uniq)

        # Picker input for categorical filter selection
        pickerInput(ns("filter_selector_cat"),
          label = "Filter:",
          choices = col_uniq,
          selected = col_uniq,
          multiple = TRUE,
          options = list(`actions-box` = TRUE)
        )
      }
    })

    # Capture numeric filter values
    observeEvent(c(input$filter_selector_min, input$filter_selector_max), {
      req(input$filter_selector_min, input$filter_selector_max)
      filter_vals(list(min = input$filter_selector_min,
                       max = input$filter_selector_max))
      numeric_filter(TRUE)
    })

    # Capture categorical filter values
    observeEvent(input$filter_selector_cat, {
      filter_vals(input$filter_selector_cat)
      numeric_filter(FALSE)
    })

    # Apply filters to the data
    observeEvent(filter_vals(), {
      req(output_data(), selected_col(), filter_vals())
      filtered_data(
        filter_data(output_data(), selected_col(), filter_vals(),
                    numeric_filter())
      )
    })

    # Store filter on application
    observeEvent(input$filter_btn, {
      req(filtered_data())
      output_data(filtered_data())

      # Store filter details
      fltrs <- filters()
      filter <- list(col = selected_col(),
                     used_vals = filter_vals(),
                     all_vals = all_vals(),
                     type = ifelse(numeric_filter(), "numeric", "categorical"))
      fltrs[[selected_col()]] <- filter
      filters(fltrs)
    })

    # Tab management for displaying current filters
    tab_ids <- reactiveVal(list())
    observeEvent(filters(), {
      for (tid in tab_ids()) {
        removeTab(session = session, inputId = "current_filters", target = tid)
      }
      tab_ids(list())
      for (filter in filters()) {
        vals <- lapply(filter$used_vals, function(val) tags$li(val))
        tab <- tabPanel(filter$col,
          value = filter$col,
          actionButton(ns(paste(filter$col, "_tab")), "Filter entfernen"),
          vals
        )
        insertTab(
          session = session,
          inputId = "current_filters",
          tab,
          select = TRUE
        )
        tab_ids(c(tab_ids(), filter$col))
      }
    })

    # Observe removal actions for filters
    observe({
      for (filter in filters()) {
        observeEvent(input[[paste(filter$col, "_tab")]], {
          fltrs <- filters()
          fltrs[[filter$col]] <- NULL
          filters(fltrs)
        })
      }
    })

    # Apply reset for all filters
    observeEvent(input$reset_btn, {
      output_data(csv_data()) # Reset output data
      filters(list()) # Clear filter storage
    })

    # Render summary table for the current column
    output$column_summary <- renderDataTable({
      req(filtered_data(), selected_col())
      summarise_col(filtered_data(), selected_col())
    })

    # Render distribution plot for the selected feature
    output$dist_plot <- renderPlotly({
      req(filtered_data(), selected_col())
      fig <- plot_univariate(filtered_data(), selected_col())
      if (input$log_scale) {
        fig <- fig %>% layout(yaxis = list(type = "log"))
      }
      fig
    })

    # Return output and filter information for use in higher modules
    return(list(data = output_data, filters = filters))
  })
}