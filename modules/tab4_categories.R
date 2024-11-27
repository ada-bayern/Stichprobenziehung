library(shiny)
library(DT)
# library(gtsummary)

# TODO: reaction to "Anwenden"
# TODO: presets...
# TODO: crosstable
# TODO: unified modularization

source("modules/define_layer_module.R")
source("modules/selection_probability_module.R")
source("modules/stratification/select_groups.R")

# Define UI
categories_ui <- function(id) {
  ns <- NS(id)
  # Defining the layout elements. Action button to add new stratification layers
  # adds button for each layer. These buttons are tied to dynamically generated
  # layouts for defining stratification layers
  # In main panel, a crosstable of strata can be displayed.
  fluidPage(
    titlePanel("Variablen auswählen und Kategorien erstellen"),
    sidebarLayout(
      mainPanel(
        actionButton(ns("add_strat_layer_button"), "Schicht hinzufügen",
                     icon = icon("plus")),
        #uiOutput(ns("strat_layer_buttons_ui")),
        tabsetPanel(id = ns("strata_rename_input_ui"))#, type = "hidden")
      ),
      sidebarPanel(
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

# layer <- setClass("layer",
#                   slots = c(id = "character",
#                             name = "character",
#                             data_type = "character",
#                             categories = "list",
#                             uniq_cats = "list",
#                             cols_categorized = "list",
#                             sel_kind = "character",
#                             sel_params = "list"),
#                   prototype = list(id = "layer_0",
#                                    name = "",
#                                    data_type = "categorical",
#                                    categories = list(),
#                                    uniq_cats = list(),
#                                    cols_categorized = list(),
#                                    sel_kind = "proportional",
#                                    sel_params = list()))

# Define server logic
categories_server <- function(id, csv_data, presets) {
  moduleServer(id, function(input, output, session) {

    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(csv_data(), {
      dataset(csv_data())
    })

    # All the relevant information for stratification
    # ids: character vector of layer ids (layer_1, layer_2, etc.)
    # columns: named list with the names of columns selected for stratification.
    #   The names correspond to ids
    # data_types: Named list indiating for each column whether the user has
    #   selected categorical or numerical values ("categorical" or "numerical")
    # categories: Named list of named lists of lists. The top-layer names
    #   correspond to layer_ids. Each top layer list element contains a named
    #   list in which the names define
    #   a category name and each element is a list defining the category.
    # data: The computed data with only the selected columns and the applied
    #   categorizations
    # cols_categorized: list of the columns with categorizations applied
    # sel_kind: Created here, but values are only inserted in tab 5. Named list
    #   indicating for each column how the user defines the selection
    #   probabilities or strata sizes in UI. ("proportional", "sample" or
    #   "population"). Names
    #   correspond to layer_ids
    # sel_params: Also computed in tab 5. The relevant parameters for specifying
    #   strata sizes using the R package.
    # strat_layers <- reactiveValues(ids = c(), columns = list(),
    #                                data_types = list(), categories = list(),
    #                                data = NULL, cols_categorized = list(),
    #                                sel_kind = list(), sel_params = list())

    r <- reactiveValues(selected_data = NULL,
                        strat_layers = c())

    # Adding a new stratification layer
    observeEvent(input$add_strat_layer_button, {

      # Create new UI for defining layer and adding it to list of tabs
      ns <- session$ns

      base_id <- length(r$strat_layers) + 1
      layer_id <- paste0("layer_", base_id)
      # strat_layers$ids <- c(strat_layers$ids, layer_id)

      # column values which haven't been selected yet so no columns
      # is selected for two stratification layers
      # sc_vec <- strat_layers$columns
      unselected_cols <- setdiff(colnames(dataset()),
                                 colnames(r$selected_data))

      # Creates a new tab for the tabsetpanel. This tab containts the UI
      # to define a stratification layer. The defined parameters are defined
      # and saved. Inserts tab into tabsetpanel
      # TODO: More parameters besides the column name
      new_def_layer_ui <- tabPanel(
        title = paste0("Schicht ", base_id),
        id = ns(paste0("panel_def_", layer_id)),
        define_layer_ui(ns(paste0("def_", layer_id)), unselected_cols)
      )
      insertTab(inputId = "strata_rename_input_ui",
                new_def_layer_ui,
                select = TRUE)

      # get information from UI
      dl_out <- define_layer_server(paste0("def_", layer_id), dataset) # nolint

      # observe({
      #   strat_layers$columns[[layer_id]] <- layer_define_output$name
      #   strat_layers$data_types[[layer_id]] <- layer_define_output$data_type
      #   strat_layers$categories[[layer_id]] <- layer_define_output$categories
      # })

      # apply information from UI
      observe({
        # check whether applied button was pressed
        req(length(dl_out$categories) > 0)

        name <- dl_out$name
        dtype <- dl_out$data_type
        cats <- dl_out$categories

        # categorize col with the given categories
        if (dtype == "numerical") {
          breaks <- c(0, cats)
          col_categorized <- cut(
            csv_data()[[name]],
            breaks = breaks, right = FALSE, include.lowest = TRUE
          )
        } else {
          col_categorized <-
            select_groups(csv_data()[[dl_out$name]], cats, NA)
        }

        # add new layer information to output
        r$strat_layers[[layer_id]] <- c(
          id = layer_id,
          name = name,
          data_type = dtype,
          categories = cats,
          uniq_cats = unique(cats),
          col_categorized = col_categorized,
          sel_kind = NULL, # needed for nextstep: sample_server
          sel_params = NULL # needed for nextstep: sample_server
        )

        # update output data
        if (is.null(r$selected_data)) {
          r$selected_data[[name]] <- col_categorized
          r$selected_data <- as.data.frame(r$selected_data)
        } else {
          r$selected_data[[name]] <- col_categorized
        }

        # update UI in main panel
        updateSelectInput(
          session,
          "ct_column_one",
          choices = colnames(r$selected_data)
        )
        updateSelectInput(
          session,
          "ct_column_two",
          choices = colnames(r$selected_data)
        )
      })
      # TODO make col uneditable, when applied + change apply to remove
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
    # output$strat_layer_buttons_ui <- renderUI({
    #   ns <- session$ns
    #   lapply(strat_layers$ids, function(button_id) {
    #     column_name <- strat_layers$columns[[button_id]]
    #     actionButton(inputId = ns(paste0("button_", button_id)),
    #                  label = column_name)
    #   })
    # })

    # Event handler for strat layer button click. Switches to the corresponding
    # tab in the hidden tabset
    # observeEvent(strat_layers, {
    #   ns <- session$ns
    #   lapply(strat_layers, function(layer) {
    #     button_id <- paste0("button_", layer@id)
    #     observeEvent(input[[button_id]], {
    #       updateTabsetPanel(inputId = "strata_rename_input_ui",
    #                         selected = ns(paste0("panel_def_", layer@layer_id)))
    #     })
    #   })
    # })


    # function to categorize columns.
    # TODO: leave column as it is when no categories have been defined.
    # TODO: generic function? unnest!
    # categorize_column <- function(id) {
    #   column_name <- strat_layers$columns[[id]]
    #   data_type <- strat_layers$data_types[[id]]
    #   req(unlist(strat_layers$categories[[id]]))
    #   categories <- strat_layers$categories[[id]]

    #   if (data_type == "categorical") {
    #     strat_layers$cols_categorized[[id]] <-
    #       select_groups(dataset()[[column_name]], categories, NA)
    #   } else {
    #     breaks <- c(0, categories)
    #     strat_layers$cols_categorized[[id]] <- cut(
    #       dataset()[[column_name]],
    #       breaks = breaks, right = FALSE, include.lowest = TRUE
    #     ) # TODO make col uneditable, when applied + change apply to remove
    #   }
    # }

    # observe({
    #   lapply(strat_layers, categorize_column)
    # })

    # saves a data frame of only the selected columns with the created
    # categories
    # observeEvent(strat_layers$cols_categorized, {
    #   cols <- strat_layers$cols_categorized
    #   names(cols) <- strat_layers$columns[names(cols)]
    #   strat_layers$data <- data.frame(cols)
    # })

    # # Select columns for crosstabs
    # observeEvent(strat_layers$data, {
    #   updateSelectInput(
    #     session,
    #     "ct_column_one",
    #     choices = colnames(strat_layers$data)
    #   )
    #   updateSelectInput(
    #     session,
    #     "ct_column_two",
    #     choices = colnames(strat_layers$data)
    #   )
    # })

    # Creating crosstable of chosen row.
    output$crosstable <- renderTable({
      req(input$ct_column_one, input$ct_column_two, r$selected_data)
      col_one <- r$selected_data[[input$ct_column_one]]
      col_two <- r$selected_data[[input$ct_column_two]]

      ct <- table(col_one, col_two)
      as.data.frame.matrix(ct)
    }, rownames = TRUE, striped = TRUE, bordered = TRUE)

    return(list(strat_layers = reactive(r$strat_layers),
                data = reactive(r$selected_data)))
  })

}