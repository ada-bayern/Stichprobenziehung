library(DT)

# Define UI
tab3ui <- function(id){
  ns <- NS(id)
  # Defining the layout elements. The options for filtering are only shown when
  # the "Filter" button is pressed and disappear when it is pressed again
  fluidPage(
    titlePanel("Grundgesamtheit Auswählen"),
    sidebarLayout(
      sidebarPanel(
        actionButton(ns("filter_dropdown"), "Filtern", icon = icon("filter")),
        conditionalPanel(
          condition = "input.filter_dropdown % 2 != 0",
          ns = ns,
          actionButton(ns("update_button"), "Auswahl übernehmen", icon = icon("refresh")),
          uiOutput(ns("column_dropdown")),
          textOutput(ns("text6")),
          checkboxGroupInput(ns("value_dropdown"), label = NULL, choices = c())
        )
      ),
      mainPanel(
        textOutput(ns("pop_count")),
        DTOutput(ns("filtered_table"))
        #tableOutput(ns("filtered_table")),
      )
    )
  )
}

# Define server logic
tab3server <- function(id, data, old) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # this controls number of value choices in dropdown menu. If this is not done,
    #the app can crash or struggle with rendering when a column with many unique
    # values (like an id column) is selected for filtering
    max_choices = 100
    
    # The filtered population
    filtered_data <- reactiveVal(NULL)
    
    # Defining the column and the column values by which to filter
    # selected_column: string of column selected for filtering
    # selected_values: character vector of values selected from that column
    # value_choices: the possible values of the selected column plus "Alle auswählen"
    selected_column <- reactiveVal(NULL)
    selected_values <- reactiveVal(NULL)
    value_choices <- reactiveVal(NULL)
    
    observeEvent(data(), {
      filtered_data(data())
    })
    
    observeEvent(old(), {
      if (is.null(old())) {
        showNotification("Alte Stichprobe wurde nicht hochgeladen", type = "message")
      } else {
        showNotification("Alte Stichprobe wurde erfolgreich hochgeladen", type = "message")
      }
    })
    
    
##################
    # renders select input based on column names in dataset
    output$column_dropdown <- renderUI({
      selectInput(ns("column_dropdown"), label = "Spalte zum Filtern auswählen",
                  choices = colnames(data()))
    })
                 
    # Updates selected column
    observeEvent(input$column_dropdown, {
      selected_column(input$column_dropdown)
    })
    
    # updates the values to filter by when column is selected
    observeEvent(selected_column(),{
      choices = c("Alle auswählen", unique(data()[[selected_column()]]))
      choices = head(choices[!is.na(choices)], max_choices)
      value_choices(choices)
      selected_values(choices)
    })
    
    # displays the changed value options
    observeEvent(value_choices(), {
      updateCheckboxGroupInput(inputId = "value_dropdown",
                               choices = value_choices())
    })
    
    # Handles the logic behind select all button. The button should be unselected
    # as soon any other button is deselected, deselect all when it is deselected, 
    # and select all when it is selected. 
    eval_selected_values <- function(value_choices, input_values, selected_values){
      
      select_all <- "Alle auswählen" %in% selected_values
      select_all_input <- "Alle auswählen" %in% input_values
      all_selected_input <- all(value_choices %in% input_values) 
      all_selected <- all(value_choices %in% selected_values)
      
      if(select_all & !select_all_input){
        return(c())
      }
      if(select_all & !all_selected_input){
        return(input_values[input_values != "Alle auswählen"])
      }
      if(!select_all & select_all_input){
        return(c("Alle auswählen", value_choices))
      }
      return(input_values)
    }
    
    # evaluating change of the values selected by user, including logic for
    # "select all" button
    observeEvent(input$value_dropdown, {
      selected_values(eval_selected_values(value_choices(), input$value_dropdown,
                                           selected_values()))
    })
    
    # renders selected values as they change. Ignores NULL values as an empty vector
    # is NULL
    observeEvent(selected_values(), {
      if(length(selected_values()) == 0){
        to_select = ""
      } else {
        to_select = selected_values()
      }
      updateCheckboxGroupInput(inputId = "value_dropdown", 
                               selected = to_select)
    }, ignoreNULL = FALSE)
    
    
    # filters the population
    observe({
      req(data(), selected_column())
      filtered_data(subset(data(), data()[[selected_column()]] %in% selected_values()))
    })
      
    
    # displays number of rows in filtered data
    output$pop_count <- renderText({
      paste("Zeilen insgesamt:", nrow(filtered_data()))
    })
    
    
    # Showing the filtered table
    output$filtered_table <- renderDT({
      datatable(filtered_data(), 
                class = "cell-border stripe", 
                options = list(dom = "ltpr"))
    })
    
    ###############################################################################
    ##default
    observeEvent(!is.null(old()), {

      old_sample <- old()

      selected_column(old_sample[["selected_column"]])
      selected_values(old_sample[["selected_values"]])
      value_choices(old_sample[["value_choices"]])

      # renders select input based on column names in dataset
      output$column_dropdown <- renderUI({
        selectInput(ns("column_dropdown"), label = "Diese Spalte wurde zum filtern ausgewählt",
                    choices = colnames(data()), selected = selected_column())
      })

      observeEvent(input$column_dropdown, {
      selected_values(old_sample[["selected_values"]])
      to_select_1 = selected_values()
      updateCheckboxGroupInput(session, "value_dropdown",
                               selected = to_select_1)#old_sample[["selected_values"]])
      })

      # filters the population
      observe({
        req(data(), selected_column())
        filtered_data(subset(data(), data()[[selected_column()]] %in% selected_values()))
      })


      # displays number of rows in filtered data
      output$pop_count <- renderText({
        paste("Zeilen insgesamt:", nrow(filtered_data()))
      })


      # Showing the filtered table
      output$filtered_table <- renderDT({
        datatable(filtered_data(),
                  class = "cell-border stripe",
                  options = list(dom = "ltpr"))
      })

      observeEvent(input$update_button, {

      selected_column(old_sample[["selected_column"]])
      selected_values(old_sample[["selected_values"]])
      #value_choices(old_sample[["value_choices"]])

      # renders select input based on column names in dataset
      output$column_dropdown <- renderUI({
        selectInput(ns("column_dropdown"), label = "Diese Spalte wurde zum filtern ausgewählt",
                    choices = colnames(data()), selected = selected_column())
      })

      observeEvent(input$column_dropdown, {
        selected_values(old_sample[["selected_values"]])
        to_select_1 = selected_values()
        updateCheckboxGroupInput(session, "value_dropdown",
                                 selected = to_select_1)#old_sample[["selected_values"]])
      })


      # filters the population
      observe({
        req(data(), selected_column())
        filtered_data(subset(data(), data()[[selected_column()]] %in% selected_values()))
      })


      # displays number of rows in filtered data
      output$pop_count <- renderText({
        paste("Zeilen insgesamt:", nrow(filtered_data()))
      })


      # Showing the filtered table
      output$filtered_table <- renderDT({
        datatable(filtered_data(),
                  class = "cell-border stripe",
                  options = list(dom = "ltpr"))
      })
      })

    }, ignoreNULL = TRUE)
    
    observeEvent(is.null(old()), {
      observeEvent(input$update_button,{
        showNotification("Es wurde keine alte Stichprobe hochgeladen!", type = "error")
      })
    })
    
    ##default
    ################################################################################
   
    # return filtered data
    return(list(filtered_data = filtered_data,  selected_column = selected_column,
           selected_values = selected_values, value_choices = value_choices))
    
  })   
}