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
#' - Imports plot_univariate, get_filter_index, summarise_col, MAX_VAL, and
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

# BUG: TabPanel visible with first tab

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
    titlePanel("Datenfilterung"),
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
        actionButton(ns("filter_btn"), "Filter anwenden"),
        actionButton(ns("or_btn"), "Filter zu ODER-Verknüpfung hinzufügen"),
        actionButton(ns("reset_btn"), "Alle Filter zurücksetzen"),
        uiOutput(ns("or_ui")),
        hr(),
        tags$b("Aktuelle Filter:"),
        tabsetPanel(id = ns("current_filters")),
        hr(),
        tags$b("Kennwerte:"),
        div(DTOutput(ns("column_summary")),
            style = "max-height: 50vh; overflow-y: auto;")
      ),
      # Main Panel for data visualization
      mainPanel(
        box(
          title = "Merkmalsverteilung",
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
filter_server <- function(id, csv_data, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive values to store selected features and applied filters
    output_data <- reactiveVal(NULL) # Output data store
    filtered_data <- reactiveVal(NULL)
    selected_col <- reactiveVal(NULL)
    filter_vals <- reactiveVal(NULL)
    all_vals <- reactiveVal(NULL)
    numeric_filter <- reactiveVal(NULL)
    filters <- reactiveVal(list()) # Store applied filters

    # Fill filters with presets
    observe({
      req(presets(), csv_data())
      fltrs <- list()
      for (f in presets()$filters) {

        # Define function to get all values (returns NULL if col not in data)
        get_all_vals <- function(col) {
          # remove running number in or_clause for indexing in data
          dcol <- gsub("_\\d*", "", col)
          if (is.numeric(csv_data()[[dcol]])) {
            list(paste0(col, " \U2208 [",
                        paste(f$filter_vals[[col]], collapse = ", "), "]"),
                  paste0(selected_col(), " \U00AC\U2208 [",
                        paste(f$filter_vals[[col]], collapse = ", "), "]"))
          } else {
            unique(csv_data()[[dcol]])
          }
        }

        # Apply function to every relevant column for this filter
        if (f$type == "orclause") {
          # Get all values for each feature in the or-clause
          f$all_vals <- lapply(names(f$used_vals), get_all_vals)
        } else {
          # Get all values for this feature as interval or vector of uniques
          f$all_vals <- get_all_vals(f$col)
        }

        # Add filter to reactive list
        if (!is.null(f$all_vals) && length(f$all_vals) > 0) {
          fltrs[[f$col]] <- f
        }
      }
      filters(fltrs)
    })

    ###############################################
    # Filter selection
    ###############################################
    continue <- reactiveVal(FALSE) # Continuation flag for categorical filters
    num <- reactiveVal(NULL) # Identify numeric vs categorical columns
    # Initialize output data with the CSV data
    observeEvent(csv_data(), {
      output_data(csv_data())

      # Dynamic update of available column choices
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
        min <- min(col, na.rm = TRUE)
        max <- max(col, na.rm = TRUE)
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
          options = list(`actions-box` = TRUE,
                         `deselect-all-text` = "Alle abwählen",
                         `select-all-text` = "Alle auswählen",
                         `none-selected-text` = "Keine ausgewählt")
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
      index <- get_filter_index(output_data()[[selected_col()]], filter_vals(),
                                numeric_filter())
      filtered_data(output_data()[index, ])
    })

    ###############################################
    # Data Summary and Visualization
    ###############################################
    # Render summary table for the current column
    output$column_summary <- renderDT({
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

    ###############################################
    # Filter management (AND clause)
    ###############################################
    # Stringify filter values for display
    used_vals <- reactiveVal(NULL)
    observeEvent(filter_vals(), {
      req(filter_vals(), all_vals(), selected_col())

      if (numeric_filter()) {
        used_vals(
          list(paste0(selected_col(), " \U2208 [",
                      paste(filter_vals(), collapse = ", "), "]"))
        )
        all_vals(
          list(paste0(selected_col(), " \U2208 [",
                      paste(filter_vals(), collapse = ", "), "]"),
               paste0(selected_col(), " \U00AC\U2208 [",
                      paste(filter_vals(), collapse = ", "), "]"))
        )
      } else {
        used_vals(filter_vals())
      }
    })

    # Store filter on application
    observeEvent(input$filter_btn, {
      req(selected_col())

      # Store filter details
      fltrs <- filters()
      filter <- list(
        col = selected_col(),
        filter_vals = filter_vals(), # filter values
        used_vals = used_vals(), # filter values for display
        all_vals = all_vals(), # all possible values (for display)
        type = ifelse(numeric_filter(), "numeric", "categorical")
      )
      fltrs[[selected_col()]] <- filter
      filters(fltrs)
    })

    # Tab management for displaying current filters
    tab_ids <- reactiveVal(list())
    observeEvent(filters(), {
      req(csv_data(), filters())

      for (tid in tab_ids()) {
        removeTab(session = session, inputId = "current_filters", target = tid)
      }
      tab_ids(list())
      out_data <- csv_data()
      filter_index <- rep(TRUE, nrow(out_data))
      for (filter in filters()) {
        # Construct conjunction of filters
        if (filter$type == "orclause") {
          filter_index <- filter_index & filter$index
        } else {
          filter_index <- filter_index &
            get_filter_index(out_data[[filter$col]],
                             filter$filter_vals,
                             filter$type == "numeric")
        }

        # Define tab UI
        tab <- tabPanel(filter$col,
          value = filter$col,
          actionButton(ns(paste0(filter$col, "_rmtab")), "Filter entfernen"),
          div(tableOutput(ns(paste0(filter$col, "_vals"))),
              style = "max-height: 50vh; overflow-y: auto;")
        )
        insertTab(
          session = session,
          inputId = "current_filters",
          tab,
          select = TRUE
        )
        tab_ids(c(tab_ids(), filter$col))
      }
      # Filter data based on AND clause
      output_data(out_data[filter_index, ])
    })

    # On tab switch
    observeEvent(input$current_filters, {
      cur_tab <- input$current_filters

      # Render value table
      output[[paste0(cur_tab, "_vals")]] <- renderTable({
        req(length(filters()) > 0)
        filter <- filters()[[cur_tab]]

        # Stringify or clause as cartesian product for display
        if (filter$type == "orclause") {
          # Flatten used values to be searchable
          uv <- unlist(lapply(filter$used_vals, unlist))
          av <- expand.grid(filter$all_vals)
          in_uv <- apply(av, 1, function(row) any(row %in% uv))
          av <- apply(av, 1, paste, collapse = " & ")
        } else {
          av <- unlist(filter$all_vals)
          in_uv <- unlist(lapply(filter$all_vals,
                                 function(val) val %in% filter$used_vals))
        }

        # Create filter view
        data.frame(
          Werte = av,
          Filter = in_uv
        )
      })

      # Observe removal actions for filters
      observeEvent(input[[paste0(cur_tab, "_rmtab")]], {
        fltrs <- filters()
        fltrs[[cur_tab]] <- NULL
        filters(fltrs)
      })
    })

    # Apply reset for all filters
    observeEvent(input$reset_btn, {
      output_data(csv_data()) # Reset output data
      filtered_data(output_data())
      filters(list()) # Clear filter storage
    })

    ###############################################
    # OR clause management
    ###############################################
    # Reactive values for OR clause management
    or_clause_open <- reactiveVal(FALSE)
    or_colname <- reactiveVal(NULL)
    or_clause <- reactiveVal(list()) # Store filters of the current OR clause
    or_clause_col <- reactiveVal(NULL)
    or_tab_ids <- reactiveVal(list())
    running_id <- reactiveVal(1)

    # Open UI for OR clauses
    observeEvent(input$or_btn, {
      or_clause_open(TRUE)

      # Store filter details
      fltrs <- or_clause()
      filter <- list(
        col = selected_col(),
        filter_vals = filter_vals(), # filter values
        used_vals = used_vals(), # filter values for display
        all_vals = all_vals(), # all possible values (for display)
        type = ifelse(numeric_filter(), "numeric", "categorical")
      )

      # Find unique index for OR clause
      # This step enables multiple entries for the same column
      i <- 1
      idx <- paste(selected_col(), i, sep = "_")
      while (idx %in% names(fltrs)) {
        i <- i + 1
        idx <- paste(selected_col(), i, sep = "_")
      }

      fltrs[[idx]] <- filter
      or_clause(fltrs)
    })

    # Render OR UI
    observeEvent(or_clause_open(), {
      if (or_clause_open()) {
        output$or_ui <- renderUI({
          fluidRow(column(12,
            hr(),
            textInput(ns("or_colname"),
                      label = "Aktuelle ODER-Klausel:",
                      placeholder = "Bezeichnung der ODER-Klausel"),
            actionButton(ns("filter_or_btn"),
                         label = "ODER-Klausel als Filter anwenden"),
            actionButton(ns("reset_or_btn"),
                         label = "ODER-Klausel zurücksetzen"),
            tabsetPanel(id = ns("or_clause"))
          ))
        })
      } else {
        output$or_ui <- renderUI(br())
      }
    })

    # Store OR clause column name
    observe({
      # Avoid empty or double names
      if (is.null(input$or_colname) ||
            input$or_colname == "" ||
            input$or_colname %in% names(filters()) ||
            input$or_colname %in% colnames(output_data())) {
        or_colname(paste0("ODER", running_id()))
      }
    })

    # Add OR clause as filter
    observeEvent(input$filter_or_btn, {
      req(or_colname())
      # Store filter details of OR clause
      fltrs <- filters()
      filter <- list(
        col = or_colname(),
        filter_vals = lapply(or_clause(), function(f) f$filter_vals), # filter values
        used_vals = lapply(or_clause(), function(f) f$used_vals), # filter values for display
        all_vals = lapply(or_clause(), function(f) f$all_vals), # all possible values (for display)
        index = or_clause_col(),
        type = "orclause"
      )

      fltrs[[or_colname()]] <- filter
      filters(fltrs)

      or_clause_open(FALSE)
      or_clause(list())
      running_id(running_id() + 1)
    })

    observeEvent(input$reset_or_btn, {
      or_clause_open(FALSE)
      or_clause(list())
      or_clause_col(NULL)
      filtered_data(output_data())
    })

    # Tab management for displaying current or clause
    observeEvent(or_clause(), {
      req(csv_data(), or_colname(), or_clause())
      for (tid in or_tab_ids()) {
        removeTab(session = session, inputId = "or_clause", target = tid)
      }
      or_tab_ids(list())

      # Create False vector
      out_data <- csv_data()
      filter_index <- rep(FALSE, nrow(out_data))

      for (idx in names(or_clause())) {
        filter <- or_clause()[[idx]]
        # Construct disjunction of filters
        filter_index <- filter_index |
          get_filter_index(out_data[[filter$col]],
                           filter$filter_vals,
                           filter$type == "numeric")

        # Define tab UI
        tab <- tabPanel(filter$col,
          value = idx,
          actionButton(ns(paste0(idx, "_or_rmtab")), "Filter entfernen"),
          div(tableOutput(ns(paste0(idx, "_or_vals"))),
              style = "max-height: 50vh; overflow-y: auto;")
        )
        insertTab(
          session = session,
          inputId = "or_clause",
          tab,
          select = TRUE
        )
        or_tab_ids(c(or_tab_ids(), filter$col))
      }
      # Store filter vector for OR clause
      or_clause_col(filter_index)
    })

    # On tab switch
    observeEvent(input$or_clause, {
      cur_tab <- input$or_clause

      # Render value table
      output[[paste0(cur_tab, "_or_vals")]] <- renderTable({
        req(length(or_clause()) > 0)

        filter <- or_clause()[[cur_tab]]
        # Create filter view
        data.frame(
          Werte = unlist(filter$all_vals),
          Filter = unlist(lapply(filter$all_vals,
                                 function(val) val %in% filter$used_vals))
        )
      })

      # Observe removal actions for filters
      observeEvent(input[[paste0(cur_tab, "_or_rmtab")]], {
        fltrs <- or_clause()
        fltrs[[cur_tab]] <- NULL
        or_clause(fltrs)
      })
    })

    # Return output and filter information for use in higher modules
    return(list(data = output_data, filters = filters))
  })
}