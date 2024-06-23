source("modules/selection_probability_module.R")
source(file.path("../aktenstichprobe/R", "strata_sizes.R"))

library(DT)


# Define UI
tab5ui <- function(id){
  ns <- NS(id)
  
  # Defining layout elements.Sidebar allows defining parameter for selection
  # probabilities. Main panel displays editable crosstable
  fluidPage(
    titlePanel("Stichprobenziehung definieren"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, numericInput(ns("sample_size"), "Stichprobengröße", value = 100, min = 1, max = 99999, width = "80px")),
          column(6, align = "center",
                 br(),
                 textOutput(ns("realized_sample_size"))
          )
        ),
        br(),
        # Tabset for defining sampling probabilities
        uiOutput(ns("define_selection_probs_ui"))
      ),
      mainPanel(
        radioButtons(ns("strat_table_select"), 
                     label = "Art der Stichprobenberechnung",
                     choiceNames = c("Stichprobengröße garantiert", "Kategoriengrößen garantiert"),
                     choiceValues = c("sample_size", "category_size")),
        DTOutput(ns("strata"))
      )
    )
  )
}

# Define server logic
tab5server <- function(id, strat_layers) {
  moduleServer(id, function(input, output, session) {
    
    # Saving the data uploaded in another tab and passes to this module.
    dataset <- reactiveVal(NULL)
    observeEvent(strat_layers$data, {
      dataset(strat_layers$data)
    })
    
    # table of computed strata sizes (computed based on inputs)
    strata <- reactiveVal(NULL)
    display_strata <- reactiveVal(data.frame(Hinweis = "Bitte alle Parameter eingeben, um Kreuztabelle zu erstellen."))
    
    # sample size (computed based on user input)
    sample_size <- reactiveVal(NULL)
    realized_sample_size <- reactiveVal(NULL)
    
    observeEvent(input$sample_size, {
      sample_size(input$sample_size)
      
    })
    
    
    # renders the tabs for the tabset panel in which the selection probabilities
    # are defined
    output$define_selection_probs_ui <- renderUI({
      ns <- session$ns
      tabs <-  lapply(strat_layers$ids, function(layer_id) {
        col_name <- strat_layers$columns[[layer_id]]
        tabPanel(title = col_name, value = layer_id,
                 selection_probability_ui(ns(paste0("sp_", layer_id)), col_name))
      })
      do.call(tabsetPanel, tabs)
    })
  
    
    # computes the unique values of each column in the categorized data set
    observe({
      for(id in strat_layers$ids){
        #print(unique(strat_layers$cols_categorized[[id]]))
        strat_layers$unique_vals[[id]] <- unique(strat_layers$cols_categorized[[id]])
      }
    }, priority = 2)
    
    
    # creating moduleservers for the selection probability logic and saving results
    observe({
      ns <- session$ns
      for (layer_id in strat_layers$ids){
        col_name <- strat_layers$columns[[layer_id]]
        #print("level1")
        #print(strat_layers$unique_vals[[layer_id]])
        observe({
          #print("level2")
          #print(strat_layers$unique_vals[[layer_id]])
          #print(length(strat_layers$unique_vals[[layer_id]]) > 0)
          req(length(strat_layers$unique_vals[[layer_id]]) > 0)
          #print(paste("creating server for", layer_id))
          ret <- selection_probability_server(paste0("sp_", layer_id), strat_layers$unique_vals[[layer_id]])
          observe({
            strat_layers$sel_kind[[layer_id]] = ret$kind
            strat_layers$sel_params[[layer_id]] = ret$vec
          })
        })
      }
    }, priority = 1)
    
    
    # Gathering the inputs for stratification size computation via package and 
    # putting inputs into named list for function call with do.call()
    observe({
      req(strat_layers$data, sample_size(), unlist(strat_layers$sel_kind), 
          length(strat_layers$sel_kind) == length(strat_layers$ids))
      args <- list(
        x = strat_layers$data,
        sample_size = sample_size(),
        strat_min = 3,
        strat_names = unlist(colnames(strat_layers$data)),
        ratio_types = unlist(strat_layers$sel_kind)
      )
      ratios <- lapply(strat_layers$ids, function(id){
        strat_layers$sel_params[[id]]
      })
      
      names(ratios) <- strat_layers$columns
      args <- c(args, ratios)
      str <- do.call(strata_sizes, args)
      strata(str)
    },
    priority = 0)
    
    
    # Creating the crosstable to render, which entails selecting relevant columns
    # and renaming columns to German
    observe({
      req(strata())
      strt <- strata()
      
      if (input$strat_table_select == "sample_size"){
        cols <- c("Stratum", "Size.Population", "Ratio.Population", 
                         "Size.MD", "Selection.Probability.MD")
      } else {
        cols <- c("Stratum", "Size.Population", "Ratio.Population", 
                  "Size.LP", "Selection.Probability.LP")
      }
      
      strt <- strt[, cols]
      colnames(strt) <- c("Stratum", "Größe in Grundgesamtheit", "Anteil Grundgesamtheit",
                          "Größe Stichprobe", "Auswahlwahrscheinlichkeit")
      rss <- sum(strt$`Größe Stichprobe`)
      realized_sample_size(rss)
      display_strata(strt)
    })
    
    
    # rendering the calculated strata sizes
    output$strata <- renderDT({
      
      options <- if (!is.null(strata()) && nrow(strata()) > 10) 
        list(dom = "ltpr", lengthMenu = c(10, 15, 20), pageLength = 10) else
        list(dom = "ltr")
        
      datatable(display_strata(), 
                class = "cell-border stripe", 
                editable = TRUE,
                rownames = FALSE,
                options = options)
    })
    
    output$realized_sample_size <- renderText({
      paste("Realisierte Stichprobengröße:", realized_sample_size())
    })
    
    
    # persisting table edits
    observeEvent(input$strata_cell_edit, {
      row <- input$strata_cell_edit$row
      col <- input$strata_cell_edit$col
      str <- display_strata()
      str[row, col] <- input$strata_cell_edit$value
      display_strata(str)
    })
   
    
    return(list(strata = display_strata, sample_size = sample_size))
  })
}