library(shiny)
library(DT)
library(dplyr)

# TODO: presets...
# TODO: unified modularization

#' ### Imports
#' * define_layer_ui
#' * define_layer_server
#' * MAX_VAL
#' * ERROR_MESSAGE
#' * features
source("modules/define_layer.R")
source("modules/utils.R")

# Define UI
categories_ui <- function(id) {
  ns <- NS(id)
  # Defining the layout elements. Action button to add new stratification layers
  # adds button for each layer. These buttons are tied to dynamically generated
  # layouts for defining stratification layers
  # In the sidebar panel on the right, a crosstable of strata can be displayed.
  fluidPage(
    titlePanel("Variablen auswählen und Kategorien erstellen"),
    sidebarLayout(
      mainPanel(
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen",
                     icon = icon("plus")),
        uiOutput(ns("strat_layer_buttons_ui")),
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
        tableOutput(ns("crosstable"))
      )
    )
  )
}


# Define server logic
categories_server <- function(id, csv_data, presets) {
  moduleServer(id, function(input, output, session) {

    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(csv_data(), {
      dataset(csv_data())
    })

    # reactive value to store the output
    strat_layers <- reactiveVal(list())

    # increasing reactive value
    tab_num <- reactiveVal(0)

    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {
      req(dataset())

      # Create new UI for defining layer and adding it to list of tabs
      ns <- session$ns

      tab_num(tab_num() + 1)
      layer_id <- paste0("layer_", tab_num())

      # column values which haven't been selected yet, so no columns
      # is selected for two stratification layers
      unselected_cols <- setdiff(colnames(csv_data()),
                                 features(strat_layers(), "name"))

      # Creates a new tab for the tabsetpanel. This tab containts the UI
      # to define a stratification layer. The defined parameters are defined
      # and saved
      new_def_layer_ui <- tabPanel(
        title = paste0("Schicht ", tab_num()),
        value = ns(paste0("panel_def_", layer_id)),
        define_layer_ui(ns(paste0("def_", layer_id)), unselected_cols)
      )
      # Inserts tab into tabsetpanel
      insertTab(
        inputId = "strata_rename_input_ui",
        new_def_layer_ui,
        select = TRUE
      )

      # get information from UI
      dl_out <- define_layer_server(paste0("def_", layer_id), dataset)

      # apply information from UI
      observeEvent(dl_out$categories, {
        # check whether applied button was pressed
        req(length(dl_out$categories) > 0)

        name <- dl_out$name
        dtype <- dl_out$data_type
        cats <- dl_out$categories

        # categorize the column
        if (dtype == "numerical") {
          breaks <- c(0, cats)
          col_categorized <- cut(
            csv_data()[[name]],
            breaks = breaks, right = FALSE, include.lowest = TRUE
          )
        } else {
          col_categorized <-
            select_groups(csv_data()[[name]], cats, NA)
        }

        # add new layer information to output
        stl_tmp <- strat_layers()
        # VERY IMPORTANT: ~ communication protocol between this tab and the
        # next one!! -> TODO: S4-class in utils.R?
        stl_tmp[[layer_id]] <- list(
          id = layer_id, # id, which is also used for indexing in strat_layers
          name = name, # name of the variable
          data_type = dtype, # dtype of the variable
          categories = cats, # value-category mapping as a list of lists
          uniq_cats = names(cats), # unique categories
          col = col_categorized, # categorized column
          sel_kind = NULL, # needed for nextstep: sample_server
          sel_params = NULL # needed for nextstep: sample_server
        )
        strat_layers(stl_tmp)
      })

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

    # update the column selection for the cross table preview
    observeEvent(strat_layers(), {
      # col_names
      names <- features(strat_layers(), "name")

      # update UI in main panel
      updateSelectInput(
        session,
        "ct_column_one",
        choices = names
      )
      updateSelectInput(
        session,
        "ct_column_two",
        choices = names
      )
    })


    # Fill strat layers with presets
    # observeEvent(presets(), {
    #   # Create new UI for defining layer and adding it to list of tabs
    #   ns <- session$ns

    #   strat_layers$ids <- presets()$ids

    #   lapply(strat_layers$ids, function(layer_id) {
    #     # all column names as options. Reading parameters defined in preset file
    #     unselected_cols <- colnames(dataset())
    #     preset_name <- presets()$columns[[layer_id]]
    #     preset_data_type <- presets()$data_types[[layer_id]]
    #     preset_categories <- presets()$categories[[layer_id]]

    #     # Creates a new tab for the tabsetpanel. This tab contains the UI
    #     # to define a stratification layer. The defined parameters are defined
    #     # and saved. Inserts tab into tabsetpanel
    #     new_def_layer_ui <- tabPanel(ns(paste0("panel_def_", layer_id)),
    #                                  define_layer_ui(ns(paste0("def_", layer_id)), # nolint
    #                                                  unselected_cols))
    #     layer_define_output <- define_layer_server(paste0("def_", layer_id),
    #                                                dataset,
    #                                                preset_name,
    #                                                preset_data_type,
    #                                                preset_categories)
    #     insertTab(inputId = "strata_rename_input_ui", new_def_layer_ui)
    #     observe({
    #       strat_layers$columns[[layer_id]] <- layer_define_output$name
    #       strat_layers$data_types[[layer_id]] <- layer_define_output$data_type
    #       strat_layers$categories[[layer_id]] <- layer_define_output$categories
    #     })
    #   })


    # }, ignoreNULL = TRUE)

    # Outputting strat layer buttons UI
    output$strat_layer_buttons_ui <- renderUI({
      ns <- session$ns
      lapply(strat_layers(), function(layer) {
        column_name <- layer$name
        actionButton(inputId = ns(paste0("button_", layer$id)),
                     label = column_name)
      })
    })

    # Event handler for strat layer button click. Switches to the corresponding
    # tab in the hidden tabset
    observeEvent(strat_layers(), {
      ns <- session$ns
      lapply(strat_layers(), function(layer) {
        button_id <- paste0("button_", layer$id)
        observeEvent(input[[button_id]], {
          updateTabsetPanel(inputId = "strata_rename_input_ui",
                            selected = ns(paste0("panel_def_", layer$id)))
        })
      })
    })

    # Creating crosstable of chosen row.
    output$crosstable <- renderTable({
      req(input$ct_column_one, input$ct_column_two, strat_layers())
      # get stratification layer by name
      col_one <- Find(function(layer) layer$name == input$ct_column_one,
                      strat_layers())$col
      col_two <- Find(function(layer) layer$name == input$ct_column_two,
                      strat_layers())$col

      ct <- table(col_one, col_two)
      as.data.frame.matrix(ct)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    return(list(strat_layers = strat_layers))
  })

}