library(sortable)
library(shiny)

# Define the module UI
define_layer_ui <- function(id, col_options) {
  ns <- NS(id)

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
    ")
    ),
    tagList(
      h4(textOutput(ns("column_name"))),
      selectInput(ns("column_select"),
                  "Spalte auswählen:",
                  choices = col_options),
      # This could also be recognized automatically
      radioButtons(ns("data_type"), "Datentyp auswählen:",
                   choices = c("Numerisch", "Kategorisch"),
                   selected = "Numerisch"),
      numericInput(ns("num_categories"),
                   "Anzahl der Kategorien",
                   value = 2, min = 2, step = 1),
      hr(),
      actionButton(ns("apply_button"), label = "Anwenden"),
      hr(),
      uiOutput(ns("def_categories_ui")),
    )
  )
}

# Define the module server logic
define_layer_server <- function(id, dataset, preset_name = NULL,
                                preset_data_type = NULL,
                                preset_categories = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive value to store the selected column
    selected_column <- reactiveValues(name = NULL, data_type = NULL,
                                      categories = list())

    # reactive value to store number of categories
    num_categories <- reactiveVal(NULL)

    pr_name <- reactiveVal(preset_name)
    pr_dt <- reactiveVal(preset_data_type)
    pr_cat <- reactiveVal(preset_categories)

    observeEvent(pr_name(), {
      updateSelectInput(inputId = "column_select", selected = pr_name())
    }, ignoreNULL = TRUE)

    observeEvent(pr_dt(), {
      if (pr_dt() == "categorical") {
        updateRadioButtons(inputId = "data_type", selected = "Kategorisch")
      } else {
        updateRadioButtons(inputId = "data_type", selected = "Numerisch")
      }
    }, ignoreNULL = TRUE)

    # Update selected column, number of categories, and data type
    observeEvent(input$column_select, {
      selected_column$name <- input$column_select
    })

    # checking data type of values and updating selected data type
    observeEvent(selected_column$name, {
      if (all(is.numeric(dataset()[[input$column_select]]) |
                is.na(dataset()[[input$column_select]]))) {
        updateRadioButtons(inputId = "data_type", selected = "Numerisch")
      } else {
        updateRadioButtons(inputId = "data_type", selected = "Kategorisch")
      }
    })

    observeEvent(input$num_categories, {
      num_categories(input$num_categories)
    })

    #also resetting category definitions when different data type is selected
    observeEvent(input$data_type, {
      if (input$data_type == "Kategorisch") {
        selected_column$data_type <- "categorical"
      } else {
        selected_column$data_type <- "numerical"
      }
    })

    # Update the column name display
    output$column_name <- renderText({
      req(input$column_select)
      input$column_select
    })

    output$def_categories_ui <- renderUI({

      if (selected_column$data_type == "categorical") {
        vals <- unique(dataset()[[selected_column$name]])

        if (length(vals) > 100 &&
              all(is.numeric(dataset()[[selected_column$name]]))) {
          HTML("Mehr als 100 numerische Werte, bitten wählen Sie die Option
                Kategorisch.")
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
            column(6, lapply(1:num_categories(), function(n) {
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
                                input_id = ns(paste0("list_cat_", n))),
                )
              )
            }))
          )
        }
      } else {
        if (!all(is.numeric(dataset()[[selected_column$name]]))) {
          HTML("Diese Daten sind nicht numerisch, wählen sie die Option
                Kategorisch")
        } else {
          min <- min(dataset()[[selected_column$name]], na.rm = TRUE)
          max <- max(dataset()[[selected_column$name]], na.rm = TRUE)
          avg_dist <- (max - min) / num_categories()
          list(numericInput(ns(paste0("min_cat_", 1)),
                            paste0("Kategorie 1", " von"),
                            value = min),
               lapply(1:num_categories(), function(i) {
                 numericInput(ns(paste0("max_cat_", i)),
                              paste0("Kategorie ", i, " bis"),
                              value = min + (i * avg_dist))
               }))
        }
      }
    })

    observeEvent(pr_cat(), {
      # Rendering the ui for defining categories
      output$def_categories_ui <- renderUI({
        categories <- pr_cat()
        num_categories(length(categories))
        updateNumericInput(inputId = "num_categories",
                           value = length(categories))

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
                   lapply(1:num_categories(), function(n) {
                     tagList(
                       textInput(ns(paste0("name_cat_", n)),
                                 width = "100%",
                                 label = NULL,
                                 value = cat_names[[n]],
                                 placeholder = paste("Kategorie", n)),
                       bucket_list(
                         header = NULL,
                         group_name = "bucket_list_group",
                         orientation = "vertical",
                         class = c("default-sortable"),
                         add_rank_list(
                           text = NULL,
                           labels = categories[[n]],
                           input_id = ns(paste0("list_cat_", n))
                         ),
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
               lapply(1:num_categories(), function(i) {
                 numericInput(ns(paste0("max_cat_", i)),
                              paste0("Kategorie ", i, " bis"),
                              value = categories[i])
               }))
        }
      })
    }, ignoreNULL = TRUE)

    # Update category definitions
    observeEvent(input$apply_button, {
      req(selected_column$data_type)
      if (selected_column$data_type == "categorical") {
        categories <- lapply(1:num_categories(), function(i) {
          req(input[[paste0("list_cat_", i)]])
          input[[paste0("list_cat_", i)]]
        })
        names <- lapply(1:num_categories(), function(i) {
          name <- input[[paste0("name_cat_", i)]]
          ifelse(!is.null(name), name, paste("Kategorie", i))
        })
      } else {
        categories <- lapply(1:num_categories(), function(i) {
          req(input[[paste0("max_cat_", i)]])
          input[[paste0("max_cat_", i)]]
        })
        names <- lapply(1:num_categories(), function(i) {
          paste("Kategorie", i)
        })
      }
      names(categories) <- names
      selected_column$categories <- categories
    })

    # Return name of selected column.
    return(selected_column)
  })

}
