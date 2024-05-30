source(file.path("../aktenstichprobe/R", "strat_sample.R"))


# Define UI
tab6ui <- function(id){
  ns <- NS(id)
  fluidRow(
    mainPanel(
      br(),
      br(),
      tags$b("Allgemeine Informationen:"),
      "Hier können Sie die Dokumentation der Stichprobenziehung herunterladen. 
              Alle Angaben, die Sie getätigt haben, wurden gesichert und werden in einer PDF-Datei 
              herausgegeben. Neben der Dokumentation können Sie auch die CSV-Dateien der Stichprobe herunterladen. 
              Diese besteht aus zwei Versionen: Eine enthält die kompletten Meta-Daten und die zweite Version besteht nur 
              aus den Akten, die auch ausgewählt wurden. ",
      br(),
      actionButton(ns("sample_button"), "Stichprobe ziehen"),
      DTOutput(ns("sample_table"))),
    sidebarPanel(
      actionButton(ns("create_rmd"), "Erstellen der Dokumentation"),
      br(), 
      br(),
      downloadButton(ns("download_report"), "Dokumentation herunterladen")
    )
  )
}

tab6server <- function(id, strata, strat_layers) {
  moduleServer(id, function(input, output, session) {
    
    sample <- reactiveVal(NULL)
    
    ###############################################################################
    #### Create R-Markdown and list
    ###############################################################################
    observeEvent(input$create_rmd, {
      myJahr <- 2019 #connect with tab 1
      fs::file_create("Dokumentation_Stichprobe.Rmd")
      fileConn <- file("Dokumentation_Stichprobe.Rmd")
      writeLines(
        c(
          "---",
          #"title: Dokumentation der Stichprobe",
          paste("title: Dokumentation der Stichprobe", myJahr),
          "output: pdf_document",
          "---",
          "",
          "### Hier sieht man einen Einblick in den geladenen Datensatz",
          "```{r}",
          #"head(my_akten())",
          "```",
          "Hier sind alle gewählten Variablen für die Stichprobenziehung"
        ),
        fileConn
      )
      close(fileConn)
      
      #Render the R-Markdown file
      render_report <- reactive({
        # Render the R Markdown document
        render("Dokumentation_Stichprobe.Rmd", output_file = NULL, output_format = "pdf_document")
      })
      
      # Define download handler
      output$download_report <- downloadHandler(
        filename = function() {
          "Dokumentation_Stichprobe.pdf"  # Name of the downloaded file
        },
        content = function(file) {
          # Knit the R Markdown document and save it to a file
          rmarkdown::render("Dokumentation_Stichprobe.Rmd",
                            output_format = "pdf_document",
                            output_file = file)
        }
      )
      
    })
    
    
    observeEvent(input$sample_button, {
      req(strat_layers$data, nrow(strata()) > 1)
      args <- list(
        data = strat_layers$data,
        strata_sizes = strata()[, "Größe Stichprobe"])
      
      strat_names <- lapply(strat_layers$ids, function(id){
        names(strat_layers$sel_params[[id]])
      })
      
      names(strat_names) <- strat_layers$columns
      args <- c(args, strat_names)
      smpl <- do.call(strat_sample, args)
      sample(smpl)
    })
    
    
    output$sample_table <- renderDT({
      datatable(sample())
    })
    
    
  })
}






