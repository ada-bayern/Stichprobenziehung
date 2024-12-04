library(shiny)

# TODO: explanation

# Define the module
selection_probability_ui <- function(id, title) {
  ns <- NS(id)
  tabPanel(title,
           selectInput(inputId = ns("sp_kind"),
                       "Art der Auwahlwahrscheinlichkeit",
                       choices = c("Proportional", "Als Anteil an Stichprobe",
                                   "Alle auswählen"),
                       selected = "Als Anteil an Stichprobe"),
           uiOutput(ns("sp_inputs")))
}


# Define the module server logic
selection_probability_server <- function(id, vals, preset_sel_kind = NULL,
                                         preset_params = NULL) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns
    selection_params <- reactiveValues(kind = NULL, vec = list())
    values <- reactiveVal(na.omit(vals))

    pr_sk <- reactiveVal(preset_sel_kind)
    pr_par <- reactiveVal(preset_params)

    # reading input kind from preset file
    observe({
      req(pr_sk())
      if (pr_sk() == "population") {
        # for some (mysterious to me) reason, you need to use the input in some
        # function to update it. A print statement also works
        req(input$sp_kind)
        updateSelectInput(session,
                          inputId = "sp_kind",
                          selected = "Alle auswählen")
      } else if (pr_sk() == "sample") {
        req(input$sp_kind)
        updateSelectInput(inputId = "sp_kind",
                          selected = "Als Anteil an Stichprobe")
      } else {
        req(input$sp_kind)
        updateSelectInput(inputId = "sp_kind", selected = "Proportional")
      }
    })

    # reading parameters from preset file and rendering
    observeEvent(pr_par(), {
      # rendering UI to input category proportions
      output$sp_inputs <- renderUI({
        if (input$sp_kind == "Als Anteil an Stichprobe") {
          params <- pr_par()
          # Create a numeric input for each unique value
          inputs <- lapply(values(), function(value) {
            # TODO: could offer to input a percentage value
            numericInput(ns(paste0("ratio_", value)),
                         paste0("Anteil für ", value, ":"),
                         value = params[[value]],
                         min = 0,
                         max = 1,
                         step = 0.01)
          })
          do.call(tagList, inputs)
        } else if (input$sp_kind == "Alle auswählen") {
          params <- pr_par()
          selected_vals <- names(params[params == 1])

          # Create a numeric input for each unique value
          checkboxGroupInput(ns("value_select"),
                             "Alle Elemente dieses Stratum auswählen",
                             choices = values(), selected = selected_vals)
        } else if (input$sp_kind == "Proportional") {
          ## TODO: Placeholder for actually calculating and displaying selection
          ## probabilities
          HTML("Auswahlwahrscheinlichkeiten proportional zum Vorkommen in
                Grundgesamtheit")
        }
      })
    }, ignoreNULL = TRUE)


    # rendering UI to input category proportions
    output$sp_inputs <- renderUI({
      if (input$sp_kind == "Als Anteil an Stichprobe") {
        # Create a numeric input for each unique value
        inputs <- lapply(values(), function(value) {
          # TODO: could offer to input a percentage value
          numericInput(ns(paste0("ratio_", value)),
                       paste0("Anteil für ", value, ":"),
                       value = 1 / length(values()),
                       min = 0,
                       max = 1,
                       step = 0.01)
        })
        do.call(tagList, inputs)
      } else if (input$sp_kind == "Alle auswählen") {
        # Create a numeric input for each unique value
        checkboxGroupInput(ns("value_select"),
                           "Alle Elemente dieser Schicht auswählen",
                           choices = values())
      } else if (input$sp_kind == "Proportional") {
        ## TODO: Placeholder for actually calculating and displaying selection
        ## probabilities
        HTML("Auswahlwahrscheinlichkeiten proportional zum Vorkommen in
              Grundgesamtheit")
      }
    })

    # Calculating the selection params which define strata sizes by way of
    # defining category sizes. Differs based on what kind of interface user
    # has selected
    observe({
      req(input$sp_kind)
      if (input$sp_kind == "Als Anteil an Stichprobe") {
        ratios <- lapply(values(), function(value) {
          req(input[[paste0("ratio_", value)]])
          input[[paste0("ratio_", value)]]
        })
        ratios <- unlist(ratios)
        names(ratios) <- values()
        selection_params$kind <- "sample"
      }
      if (input$sp_kind == "Alle auswählen") {
        req(input$value_select)
        ratios <- ifelse(values() %in% input$value_select, 1, NA)
        names(ratios) <- values()
        selection_params$kind <- "population"
      }
      if (input$sp_kind == "Proportional") {
        selection_params$kind <- "proportional"
        ratios <- rep(NA, length(values()))
        names(ratios) <- values()
      }

      selection_params$vec <- ratios
    })

    # Return the ratios as a reactive value
    return(selection_params)

  })
}