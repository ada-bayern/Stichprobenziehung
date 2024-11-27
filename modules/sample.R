library(shiny)
library(DT)
library(dplyr)

# Define the UI for the stratified sampling module (in German, using snake_case)
sample_ui <- function(id) {
  ns <- NS(id) # Namespace for the module
  titlePanel("Stichprobenziehung")
  sidebarLayout(
    sidebarPanel(
      # Sample size input
      numericInput(ns("sample_size"),
                   "Gesamtstichprobengröße",
                   value = 100,
                   min = 1,
                   max = 9999),

      # Toggle between sampling strategies
      radioButtons(
        ns("sampling_type"),
        "Stichprobenstrategie",
        choices = c("Garantierte Gesamtstichprobengröße" = "total_size",
                    "Garantierte Mindestgröße pro Kategorie" = "category_size"),
        selected = "total_size"
      ),

      # Dynamic UI for probability selection
      uiOutput(ns("select_prob")),

      # Button to trigger the sampling
      actionButton(ns("sample_button"), "Stichprobe generieren")
    ),

    mainPanel(
      h4("Zusammenfassung der Stichprobe"),
      dataTableOutput(ns("sample_summary"))
    )
  )
}

# Define the server logic for the stratified sampling module
sample_server <- function(id, strat_layers, presets) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns # Namespace for the module

    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(strat_layers$data, {
      dataset(strat_layers$data)
    })

    # computes the unique values of each column in the categorized data set
    observe({
      for (id in strat_layers$ids){
        strat_layers$unique_vals[[id]] <-
          unique(strat_layers$cols_categorized[[id]])
      }
    })

    # Reactive value to store the sampled dataset
    sampled_data <- reactiveVal()

    # Render the probability selection UI
    output$select_prob <- renderUI({
      req(strat_layers)

      tabs <- lapply(strat_layers$ids, function(id) {
        tabPanel(
          title = strat_layers$columns[[id]],
          checkboxInput(ns(paste0(id, "_select_all")),
                        "Alle auswählen",
                        value = FALSE),
          checkboxInput(ns(paste0(id, "_proportional")),
                        "Proportional zur Grundgesamtheit",
                        value = FALSE),
          lapply(strat_layers$categories$id, function(category) {
            sliderInput(
              ns(paste0(id, "_", category$category_name, "_prob")),
              label = paste("Wahrscheinlichkeit für", category$category_name),
              min = 0,
              max = 1,
              value = 1,
              step = 0.01
            )
          })
        )
      })

      do.call(tabsetPanel, tabs)
    })

    # Update probabilities when checkboxes are toggled
    observeEvent(strat_layers, {
      strat_layers_obj <- strat_layers
      lapply(strat_layers_obj, function(layer) {
        observeEvent(input[[paste0(layer$id, "_select_all")]], {
          if (input[[paste0(layer$id, "_select_all")]]) {
            lapply(layer$categories, function(category) {
              updateSliderInput(
                session,
                ns(paste0(layer$id, "_", category$category_name, "_prob")),
                value = 1
              )
            })
          }
        })

        observeEvent(input[[paste0(layer$id, "_proportional")]], {
          if (input[[paste0(layer$id, "_proportional")]]) {
            req(dataset())
            data <- dataset()
            total_count <- nrow(data)
            category_probs <- sapply(layer$categories, function(category) {
              sum(data[[layer$id]] %in% category$variable_values) / total_count
            })
            for (i in seq_along(layer$categories)) {
              updateSliderInput(
                session,
                ns(paste0(layer$id,
                          "_",
                          layer$categories[[i]]$category_name,
                          "_prob")),
                value = category_probs[i]
              )
            }
          }
        })
      })
    })

    # Perform sampling when the button is clicked
    observeEvent(input$sample_button, {
      req(dataset(), strat_layers)

      data <- dataset()
      layers <- strat_layers

      # Collect probabilities set by the user
      probabilities <- lapply(layers, function(layer) {
        sapply(layer$categories, function(category) {
          input[[paste0(layer$id, "_", category$category_name, "_prob")]]
        })
      })

      # TODO: Use probabilities in stratified sampling logic (implementation
      # can depend on sampling method)

      # Placeholder: Just return the dataset for now
      sampled_data(data)
    })

    # Render the summary table as a DT
    output$sample_summary <- renderDataTable({
      req(sampled_data())
      datatable(
        sampled_data(),
        options = list(pageLength = 10, autoWidth = TRUE),
        rownames = FALSE
      )
    })

    return(list(sample = sampled_data))
  })
}