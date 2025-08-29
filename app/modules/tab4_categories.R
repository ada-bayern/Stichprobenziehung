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
library(rintrojs)

# Import necessary functions and variables
source("modules/helpers/tab4_1_define_layer.R")
source("modules/helpers/tab4_2_select_groups.R")
source("modules/helpers/utils.R")
source("modules/helpers/manual.R")


# Define UI function for Categories module
categories_ui <- function(id) {
  ns <- NS(id)

  # Fluid page layout for interactive features
  fluidPage(
    introjsUI(),
    actionButton(ns("info"), "Info",
                 icon = icon("question-circle")),

    titlePanel("Variablen auswählen und Kategorien erstellen"),
    sidebarLayout(
      mainPanel(
        div(id = ns("add"),
          actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen",
                       icon = icon("plus"))
        ),

        div(id = ns("buttons"),
          # UI output for stratification layer buttons
          uiOutput(ns("strat_layer_buttons_ui")),

          # Tabset for stratification layer configurations, initially hidden
          tabsetPanel(id = ns("strata_rename_input_ui"), type = "hidden")
        )
      ),
      sidebarPanel(id = ns("overview"),
        fluidRow(
          column(6, selectInput(ns("ct_column_one"),
                                label = "1. Spalte auswählen",
                                choices = NULL)),
          column(6, selectInput(ns("ct_column_two"),
                                label = "2. Spalte auswählen",
                                choices = NULL))
        ),
        div(
          tableOutput(ns("crosstable")),
          style = "overflow-x: auto;"
        )
      )
    )
  )
}

# Define server logic for Categories module
categories_server <- function(id, dataset, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Steps for Guided Tour
    steps <- reactive(data.frame(
      element = c(NA, paste0("#", ns("add")), paste0("#", ns("buttons")),
                  paste0("#", ns("overview")), paste0("#", ns("buttons")),
                  paste0("#", ns("buttons")), paste0("#", ns("buttons")),
                  paste0("#", ns("buttons")), paste0("#", ns("buttons"))),
      intro = MANUAL$categories
    ))

    # Info button for Guided Tour
    observeEvent(input$info, introjs(session, options = c(
      list("steps" = steps()),
      INTRO_OPTIONS
    )))

    # Reactive value for tracking the number of stratification layers
    tab_num <- reactiveVal(0)

    # Reactive value for stratification layers
    strat_layers <- reactiveVal(list())
    strat_layer_servers <- reactiveVal(list())
    layer_data <- reactiveVal(NULL)

    # Add a "hidden" column to the dataframe that has only one value
    # This is needed to calculate the parameters for pure random sampling
    observeEvent(dataset(), {
      layer_data(data.frame(Gesamtheit = rep("ja", nrow(dataset()))))
    })

    # Fill strat_layers and UI with presets
    observeEvent(presets(), {
      sls <- list()
      for (s in presets()$strat_layers) {
        # Create new UI for defining layer
        tab_num(tab_num() + 1)
        layer_id <- paste0("layer_", tab_num())
        unselected_cols <- setdiff(
          colnames(dataset()),
          lapply(strat_layers(), function(layer) layer$name)
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
        server <- define_layer_server(paste0("def_", layer_id),
          dataset,
          preset_name = s$name,
          preset_data_type = s$data_type,
          preset_categories = s$categories
        )
        sls[[layer_id]] <- server
      }
      strat_layer_servers(c(strat_layer_servers(), sls))
    })

    observe({
      req(strat_layer_servers())

      for (layer_id in names(strat_layer_servers())) {
        server <- strat_layer_servers()[[layer_id]]

        # Process stratification layer details once defined
        observeEvent(server$categories, {
          # Extract layer information
          name <- server$name
          dtype <- server$data_type
          cats <- server$categories

          # Update stratification layers list
          sl <- strat_layers()
          sl[[layer_id]] <- list(
            id = layer_id,
            name = name,
            data_type = dtype,
            categories = cats
          )
          strat_layers(sl)

          # Update strat layer data
          ld <- layer_data()
          # Categorize column based on data type
          ld[[name]] <- switch(
            dtype,
            "numeric" = cut(dataset()[[name]], breaks = c(0, cats),
                            right = FALSE, include.lowest = TRUE),
            select_groups(dataset()[[name]], cats, NA)
          )
          layer_data(ld)
        })

        # Allow for removal of stratification layers
        observeEvent(server$remove, {
          if (server$remove) {
            # Update stratification layers list
            sl <- strat_layers()
            sl[[layer_id]] <- NULL
            strat_layers(sl)
            # Update strat layer data
            ld <- layer_data()
            ld[[server$name]] <- NULL
            layer_data(ld)
            # Update UI by removing corresponding tab
            removeTab(
              inputId = "strata_rename_input_ui",
              target = ns(paste0("panel_def_", layer_id))
            )
          }
        })
      }
    })


    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {
      req(dataset())

      # Create new UI for defining layer
      tab_num(tab_num() + 1)
      layer_id <- paste0("layer_", tab_num())
      unselected_cols <- setdiff(
        colnames(dataset()),
        lapply(strat_layers(), function(layer) layer$name)
      )

      # Create tab for stratification layer UI
      new_def_layer_ui <- tabPanel(
        title = paste("Schicht", tab_num()),
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
      server <- define_layer_server(paste0("def_", layer_id), dataset)
      sls <- strat_layer_servers()
      sls[[layer_id]] <- server
      strat_layer_servers(sls)
    })

    # Update column selection for cross table preview based on layers
    observeEvent(strat_layers(), {
      cnames <- list()
      for (layer in strat_layers()) {
        cnames[[layer$name]] <- layer$name
      }
      updateSelectInput(session, "ct_column_one", choices = cnames)
      updateSelectInput(session, "ct_column_two", choices = cnames)
    })

    # Output stratification layer action buttons
    output$strat_layer_buttons_ui <- renderUI({
      req(strat_layers())
      lapply(strat_layers(), function(layer) {
        actionButton(ns(paste0("button_", layer$id)), label = layer$name)
      })
    })

    # Switch to corresponding stratification tab when button is clicked
    observeEvent(strat_layers(), {
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
      col_one <- layer_data()[[input$ct_column_one]]
      col_two <- layer_data()[[input$ct_column_two]]

      # Create crosstabulation and return as data frame
      ct <- table(col_one, col_two)
      as.data.frame.matrix(ct)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    # Return the list of stratification layers
    return(list(data = layer_data, strat_layers = strat_layers))
  })
}