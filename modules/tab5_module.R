source("modules/selection_probability_module.R")
source(file.path("../aktenstichprobe/R", "strata_sizes.R"))
# Define UI
tab5ui <- function(id){
  ns <- NS(id)
  # Defining the layout elements. Action button to add new stratification layers
  # adds button for each layer. These buttons are tied to dynamically generated
  # layouts for defining stratification layers
  # In main panel, a crosstable of strata can be displayed.
  fluidPage(
    titlePanel("stichprobenziehung definieren"),
    mainPanel(
      uiOutput(ns("crosstab_columns")),
      tableOutput(ns("crosstable")),
      fluidRow(
        # Text input for sample size
        column(2, numericInput(ns("sample_size"), "Stichprobengröße", value = 100, min = 1, max = 99999, width = "80px")),
        # Tabset for defining sampling probabilities
        uiOutput(ns("define_selection_probs_ui"))
      ),
      tableOutput(ns("strata"))
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
    
    # sample size (computed based on user input)
    sample_size <- reactiveVal(NULL)
    
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
        req(strat_layers$columns[[id]] %in% colnames(strat_layers$data))
        strat_layers$unique_vals[[id]] <- unique(strat_layers$data[[strat_layers$columns[[id]]]])
      }
    })
    
    
    # creating moduleservers for the selection probability logic and saving results
    observe({
      ns <- session$ns
      for (layer_id in strat_layers$ids){
        col_name <- strat_layers$columns[[layer_id]]
        req(strat_layers$unique_vals[[layer_id]])
        ret <- selection_probability_server(paste0("sp_", layer_id), strat_layers$unique_vals[[layer_id]])
        observe({
          strat_layers$sel_kind[[layer_id]] = ret$kind
          strat_layers$sel_params[[layer_id]] = ret$vec
        })
        
      }
    })
    
    
    # Gathering the inputs for stratification size computation via package and 
    # putting inputs into named list for function call with do.call()
    observe({
      req(strat_layers$data, sample_size(), unlist(strat_layers$sel_kind))
      args <- list(
        x = strat_layers$data,
        sample_size = sample_size(),
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
    })
    
    
    # rendering the calculated strata sizes
    output$strata <- renderTable({
      strata()
    }
    )
    
    
  })
  
}