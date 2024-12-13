#' Module: Categories Selection and Creation
#'
#' This module defines the UI and server logic for selecting variables and
#' creating categories within a Shiny application. It provides functionality
#' for dynamically adding, configuring, and removing stratification layers.
#' Stratification layers allow users to categorize data based on defined
#' parameters and view the results in a crosstable format.
#'
#' Libraries and Imports:
#' - Utilizes libraries such as shiny and DT for UI creation and dynamic
#'   interaction.
#' - Imports functions and variables from external R scripts for modular
#'   functionality (`define_layer.R`, `utils.R`).
#'
#' UI Structure:
#' - Main Panel: Contains action buttons to add new stratification layers and
#'   dynamic UI elements for layer configuration.
#' - Sidebar Panel: Provides selections for columns in a cross tabulation and
#'   displays the resulting crosstable.
#'
#' Server Functionality:
#' - Handles dynamic generation and management of stratification layers based on
#'   user inputs.
#' - Manages reactive states for updating and displaying cross table results.
#' - Ensures communication between this module and the main application and
#'   other parts of the module.
#'
#' Important Variables/Functions:
#' - `categories_ui`: Defines the layout for the categories tab, including main
#'   controls and display areas.
#' - `categories_server`: Handles the logic for interacting with the data,
#'   adding and managing stratification layers, and produces the cross table
#'   output.
#' - `strat_layers`: Reactive value storing information about current
#'   stratification layers, used for organizing and displaying categorical data.
#' - UI elements such as action buttons and selection inputs are managed
#'   dynamically to create a responsive data categorizing interface.

# Load necessary libraries
library(shiny)
library(DT)

# TODO: Add presets...

# Import necessary functions and variables
source("modules/helpers/tab4_1_define_layer.R")
source("modules/helpers/utils.R")


# Define UI function for Categories module
categories_ui <- function(id) {
  ns <- NS(id)

  # Fluid page layout for interactive features
  fluidPage(
    titlePanel("Variablen auswählen und Kategorien erstellen"),
    sidebarLayout(
      mainPanel(
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen",
                     icon = icon("plus")),

        # UI output for stratification layer buttons
        uiOutput(ns("strat_layer_buttons_ui")),

        # Tabset for stratification layer configurations, initially hidden
        tabsetPanel(id = ns("strata_rename_input_ui"), type = "hidden")
      ),
      sidebarPanel(
        uiOutput(ns("layer_display")),
        fluidRow(
          column(6, selectInput(ns("ct_column_one"),
                                label = "1. Spalte auswählen",
                                choices = NULL)),
          column(6, selectInput(ns("ct_column_two"),
                                label = "2. Spalte auswählen",
                                choices = NULL))
        ),
        # Output for cross table
        tableOutput(ns("crosstable"))
      )
    )
  )
}

# Define server logic for Categories module
categories_server <- function(id, csv_data, presets) {
  moduleServer(id, function(input, output, session) {

    # Reactive value for storing uploaded data
    dataset <- reactiveVal(NULL)

    # Update dataset when csv_data changes
    observeEvent(csv_data(), {
      dataset(csv_data())
    })

    # Reactive value for stratification layers
    strat_layers <- reactiveVal(list())

    # Reactive value for tracking the number of stratification layers
    tab_num <- reactiveVal(0)

    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {
      req(dataset())

      # Create new UI for defining layer
      ns <- session$ns
      tab_num(tab_num() + 1)
      layer_id <- paste0("layer_", tab_num())
      unselected_cols <- setdiff(
        colnames(csv_data()),
        features(strat_layers(), "name")
      )

      # Create tab for stratification layer UI
      new_def_layer_ui <- tabPanel(
        title = paste0("Schicht ", tab_num()),
        value = ns(paste0("panel_def_", layer_id)),
        define_layer_ui(ns(paste0("def_", layer_id)), unselected_cols)
      )

      # Insert new tab into panel for layer configurations
      insertTab(
        inputId = "strata_rename_input_ui",
        new_def_layer_ui,
        select = TRUE
      )

      # Handle output from layer server logic
      dl_out <- define_layer_server(paste0("def_", layer_id), dataset)

      # Process stratification layer details once defined
      observeEvent(dl_out$categories, {
        req(length(dl_out$categories) > 0)

        # Extract layer information
        name <- dl_out$name
        dtype <- dl_out$data_type
        cats <- dl_out$categories

        # Categorize column based on data type
        col_categorized <- switch(
          dtype,
          "numerical" = cut(csv_data()[[name]], breaks = c(0, cats),
                            right = FALSE, include.lowest = TRUE),
          select_groups(csv_data()[[name]], cats, NA)
        )

        # Update stratification layers list
        strat_layers_new <- strat_layers()
        strat_layers_new[[layer_id]] <- list(
          id = layer_id,
          name = name,
          data_type = dtype,
          categories = cats,
          col = col_categorized,
          cat_counts = table(col_categorized)
        )
        strat_layers(strat_layers_new)
      })

      # Allow for removal of stratification layers
      observeEvent(dl_out$remove, {
        if (dl_out$remove) {
          strat_layers(strat_layers()[names(strat_layers()) != layer_id])
          removeTab(
            inputId = "strata_rename_input_ui",
            target = ns(paste0("panel_def_", layer_id))
          )
        }
      })
    })

    # Update column selection for cross table preview based on layers
    observeEvent(strat_layers(), {
      names <- features(strat_layers(), "name")
      updateSelectInput(session, "ct_column_one", choices = names)
      updateSelectInput(session, "ct_column_two", choices = names)
    })

    # Output stratification layer action buttons
    output$strat_layer_buttons_ui <- renderUI({
      ns <- session$ns
      lapply(strat_layers(), function(layer) {
        actionButton(ns(paste0("button_", layer$id)), label = layer$name)
      })
    })

    # Switch to corresponding stratification tab when button is clicked
    observeEvent(strat_layers(), {
      ns <- session$ns
      lapply(strat_layers(), function(layer) {
        observeEvent(input[[paste0("button_", layer$id)]], {
          updateTabsetPanel(inputId = "strata_rename_input_ui",
                            selected = ns(paste0("panel_def_", layer$id)))
        })
      })
    })

    # Render and return crosstable based on selected stratification layers
    output$crosstable <- renderTable({
      req(input$ct_column_one, input$ct_column_two, strat_layers())

      # Retrieve columns based on names
      col_one <- Find(
        function(layer) layer$name == input$ct_column_one, strat_layers()
      )$col
      col_two <- Find(
        function(layer) layer$name == input$ct_column_two, strat_layers()
      )$col

      # Create crosstabulation and return as data frame
      ct <- table(col_one, col_two)
      as.data.frame.matrix(ct)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    # Return the list of stratification layers
    return(list(strat_layers = strat_layers))
  })
}