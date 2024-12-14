#' Module: Define Layer
#'
#' This module provides the UI and server logic for defining stratification
#' layers in a Shiny application. Users can select dataset columns, specify
#' the number of categories, and define category ranges or groupings for
#' stratification.
#'
#' Libraries and Imports:
#' - Utilizes libraries such as shiny, sortable, and stringr for UI
#'   elements and string manipulations.
#' - Imports utility functions from external R script `utils.R`.
#'
#' UI Structure:
#' - A fluid page layout with controls for:
#'   - Selecting columns and data types (numerical vs categorical).
#'   - Defining and visualizing category boundaries or groupings.
#'   - Action buttons for applying or removing the defined stratification.
#'
#' Server Functionality:
#' - Manages reactive values to track user selections, data types, and
#'   category definitions.
#' - Adjusts UI dynamically based on user inputs and interactions.
#' - Returns selected column information and category definitions.
#'
#' Important Variables/Functions:
#' - `define_layer_ui`: Generates UI components to facilitate stratification
#'   layer definition.
#' - `define_layer_server`: Handles logic for user interactions with the
#'   UI, updating and returning category definitions.
#' - `selected_column`: Stores details about current column selections,
#'   data types, and categories.
#' - User actions (e.g., button clicks) trigger UI updates and logical
#'   operations, leading to categorization of data.

# Load necessary libraries
library(sortable)
library(shiny)
library(stringr)

# Import necessary functions and constants
source("modules/helpers/utils.R")


# Define the UI for defining stratification layers
define_layer_ui <- function(id, col_options) {
  ns <- NS(id)

  # Create fluid page for layer definition UI with styles and controls
  fluidPage(
    tags$style(HTML("
      .form-group {
        margin-top: 0px;
        margin-bottom: 5px;
      }
      .default-sortable.rank-list-container {
        margin: 0px;
      }
      .default-sortable.bucket-list-container {
        padding: 0px;
        margin-left: 0px;
        margin-right: 0px;
        margin-top: 0px;
        margin-bottom: 15px;
      }
    ")),
    tagList(
      hr(),
      actionButton(ns("apply_button"), label = "Anwenden"),
      actionButton(ns("remove_button"), label = "Entfernen"),
      hr(),
      h4(textOutput(ns("column_name"))),
      selectInput(ns("column_select"),
                  "Spalte auswählen:",
                  choices = col_options),
      radioButtons(ns("data_type"), "Datentyp auswählen:",
                   choices = c("Numerisch" = "numeric",
                               "Kategorisch" = "categorical"),
                   selected = "numeric"),
      numericInput(ns("num_categories"),
                   "Anzahl der Kategorien",
                   value = 2, min = 2, step = 1),
      hr(),
      uiOutput(ns("def_categories_ui"))
    )
  )
}

# Define the server logic for managing stratification layer definitions
define_layer_server <- function(id, dataset, preset_name = NULL,
                                preset_data_type = NULL,
                                preset_categories = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Initialize reactive values
    selected_column <- reactiveValues(name = NULL,
                                      data_type = NULL,
                                      categories = c(),
                                      remove = FALSE)
    num_categories <- reactiveVal(NULL)
    continue <- reactiveVal(FALSE)
    presets <- reactiveVal(FALSE)

    # Observe changes in presets and update UI
    observe({
      req(preset_name, preset_data_type, preset_categories)
      presets(TRUE)
      updateSelectInput(inputId = "column_select", selected = preset_name)
      updateRadioButtons(inputId = "data_type", selected = preset_data_type)
      selected_column$name <- preset_name
      selected_column$data_type <- preset_data_type

      # Render preset categories
      output$def_categories_ui <- renderUI({
        categories <- preset_categories
        num_categories(length(categories))
        updateNumericInput(
          session = session,
          inputId = "num_categories",
          value = length(categories)
        )
        if (selected_column$data_type == "categorical") {
          cat_names <- names(categories)
          vals <- unique(dataset()[[selected_column$name]])
          selected_vals <- unlist(categories)
          unselected_vals <- setdiff(vals, selected_vals)
          fluidRow(
            column(6,
              bucket_list(
                header = NULL,
                group_name = "bucket_list_group",
                class = c("default-sortable"),
                add_rank_list(
                  text = "Werte in Spalte",
                  labels = unselected_vals,
                  input_id = ns("list_orignal_values")
                )
              )
            ),
            column(6,
              lapply(1:input$num_categories, function(n) {
                tagList(
                  textInput(
                    ns(paste0("name_cat_", n)),
                    width = "100%",
                    label = NULL,
                    value = cat_names[[n]],
                    placeholder = paste("Kategorie", n)
                  ),
                  bucket_list(
                    header = NULL,
                    group_name = "bucket_list_group",
                    orientation = "vertical",
                    class = c("default-sortable"),
                    add_rank_list(
                      text = NULL,
                      labels = categories[[n]],
                      input_id = ns(paste0("list_cat_", n))
                    )
                  )
                )
              })
            )
          )
        } else {
          min <- min(dataset()[[selected_column$name]], na.rm = TRUE)
          list(numericInput(ns(paste0("min_cat_", 1)),
                            paste0("Kategorie 1", " von"),
                            value = min),
               lapply(1:input$num_categories, function(i) {
                 numericInput(ns(paste0("max_cat_", i)),
                              paste0("Kategorie ", i, " bis"),
                              value = categories[i])
               }))
        }
      })
      selected_column$categories <- preset_categories
    })

    # Update reactive values based on user input
    observeEvent(input$column_select, {
      selected_column$name <- input$column_select
    })

    observeEvent(selected_column$name, {
      if (all(is.numeric(dataset()[[input$column_select]]) |
                is.na(dataset()[[input$column_select]]))) {
        updateRadioButtons(inputId = "data_type", selected = "numeric")
      } else {
        updateRadioButtons(inputId = "data_type", selected = "categorical")
      }
    })

    observeEvent(input$data_type, {
      selected_column$data_type <- input$data_type
    })

    # Respond to continue button
    observeEvent(input$continue, {
      continue(TRUE)
    })

    observeEvent(input$column_select, {
      continue(FALSE)
    })

    # Update column name display
    output$column_name <- renderText({
      req(input$column_select)
      input$column_select
    })

    # Define UI for category selection based on data type
    output$def_categories_ui <- renderUI({
      if (selected_column$data_type == "categorical") {
        vals <- unique(dataset()[[selected_column$name]])
        if (length(vals) > MAX_VAL && !continue()) {
          if (all(is.numeric(dataset()[[selected_column$name]]))) {
            HTML(paste("Das Merkmal enthält mehr als", MAX_VAL, "numerische
                        Werte. Bitte wählen Sie die Option Numerisch"))
          } else {
            fluidRow(column(12,
              HTML(ERROR_MESSAGE),
              br(), br(),
              actionButton(ns("continue"), "Fortfahren")
            ))
          }
        } else {
          fluidRow(
            column(6, bucket_list(
              header = NULL,
              group_name = "bucket_list_group",
              class = c("default-sortable"),
              add_rank_list(
                text = "Werte in Spalte",
                labels = vals,
                input_id = ns("list_orignal_values")
              )
            )),
            column(6, lapply(1:input$num_categories, function(n) {
              tagList(
                textInput(ns(paste0("name_cat_", n)),
                          width = "100%",
                          label = NULL,
                          placeholder = paste("Kategorie", n)),
                bucket_list(
                  header = NULL,
                  group_name = "bucket_list_group",
                  orientation = "vertical",
                  class = c("default-sortable"),
                  add_rank_list(text = NULL,
                                labels = NULL,
                                input_id = ns(paste0("list_cat_", n)))
                )
              )
            }))
          )
        }
      } else {
        if (!all(is.numeric(dataset()[[selected_column$name]]))) {
          HTML("Diese Daten sind nicht numerisch, wählen sie die Option
                'kategorisch'")
        } else {
          min <- min(dataset()[[selected_column$name]], na.rm = TRUE)
          max <- max(dataset()[[selected_column$name]], na.rm = TRUE)
          avg_dist <- (max - min) / input$num_categories
          list(numericInput(ns(paste0("min_cat_", 1)),
                            paste0("Kategorie 1", " von"),
                            value = min),
               lapply(1:input$num_categories, function(i) {
                 numericInput(ns(paste0("max_cat_", i)),
                              paste0("Kategorie ", i, " bis"),
                              value = min + (i * avg_dist))
               }))
        }
      }
    })

    # Handle apply button event to update category definitions
    observeEvent(input$apply_button, {
      req(selected_column$data_type)
      if (selected_column$data_type == "categorical") {
        categories <- lapply(1:input$num_categories, function(i) {
          req(input[[paste0("list_cat_", i)]])
          input[[paste0("list_cat_", i)]]
        })
        names <- lapply(1:input$num_categories, function(i) {
          name <- input[[paste0("name_cat_", i)]]
          ifelse(name != "", name, paste("Kategorie", i))
        })
      } else {
        categories <- lapply(1:input$num_categories, function(i) {
          req(input[[paste0("max_cat_", i)]])
          input[[paste0("max_cat_", i)]]
        })
        names <- lapply(1:input$num_categories, function(i) {
          paste("Kategorie", i)
        })
      }
      # Replace spaces with dots in category names for consistency
      names <- lapply(names, function(n) str_replace_all(n, " ", "."))
      names(categories) <- names
      selected_column$categories <- categories
      updateActionButton(
        session,
        "apply_button",
        label = "Angewendet",
        disabled = TRUE
      )
    })

    # Handle remove button event
    observeEvent(input$remove_button, {
      selected_column$remove <- TRUE
    })

    # Return selected column information including name and categories
    return(selected_column)
  })
}