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
              Diese besteht aus zwei Versionen: Eine enthält den kompletten Datensatz und die zweite Version besteht nur 
              aus den Datenpunkten, die auch ausgewählt wurden. ",
      br(),
      br(),
      br(),
      actionButton(ns("sample_button"), "Stichprobe ziehen"),
      br(),
      DTOutput(ns("sample_table"))),
    sidebarPanel(
      actionButton(ns("create_rmd"), "Erstellen der Dokumentation"),
      br(), 
      br(),
      downloadButton(ns("download_report"), "Dokumentation herunterladen"), 
      br(), 
      br(),
      downloadButton(ns("download_sample"), "Stichproben Informationen herunterladen")
    )
  )
}


tab6server <- function(id, data, ident_primary, ident_secondary, name_other, strat_layers, strata, sample_size,
                       selected_column, selected_values, value_choices, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    
    sample <- reactiveVal(NULL)
    
    ###############################################################################
    #### Create R-Markdown and list
    ###############################################################################
    observeEvent(input$create_rmd, {
    
      fs::file_create("Dokumentation_Stichprobe.Rmd")
      fileConn <- file("Dokumentation_Stichprobe.Rmd")
      writeLines(
        c(
          "---",
          #"title: Dokumentation der Stichprobe",
          #"title: Dokumentation der Stichprobe",
          "title: Dokumentation der Stichprobe",
          "date: Erstellt am `r format(Sys.Date(), '%d.%m.%Y')`",
          "author: '`r ident_primary()`'",
          "lang: de",
          #"toc: true",
          #"toc_depth: 1",
          #"highlight: tango",
          "output:",
          "pdf_document:",
          "df_print: kable",
          "---",
          "",
          "## Informationen zum verwendeten Datensatz",
          "```{r echo=FALSE}",
          "ident_secondary()",
          "```",
          "## Diese Spalte/n wurde/n bearbeitet um die Grundgesamtheit zu bestimmen.",
          "```{r}",
          "selected_column()",
          "```",
          "## Diese Ausprägungen standen zur Verfügung, um die Grundgesamtheit zu bestimmen.",
          "```{r}",
          "value_choices()",
          "```",
          "## Diese Ausprägungen wurde ausgewählt, um die Grundgesamtheit zu bestimmen.",
          "```{r}",
          "selected_values()",
          "```",
          "## Diese Spalte/n wurde/n zur neuen Gruppierung ausgewählt.",
          "```{r}",
          "reactiveValuesToList(strat_layers)$columns",
          "```",
          "## Hier sieht man die Datentyp/en der ausgewählen Spalte/n zur neuen Gruppierung.",
          "```{r}",
          "reactiveValuesToList(strat_layers)$data_types",
          "```",
          "## Hier sieht man die neuen Kategorien der ausgewählen Spalte/n zur neuen Gruppierung.",
          "```{r}",
          "reactiveValuesToList(strat_layers)$categories",
          "```",
          "## Hier sieht man ausgewählten Auswahlwahrscheinlichkeiten pro Stratum.",
          "```{r}",
          "reactiveValuesToList(strat_layers)$sel_params",
          "```",
          "## Hier sieht man die Kreuztabelle.",
          "```{r}",
          "strata()",
          "```",
          "## Hier sieht man die Größe der Stichprobe.",
          "```{r}",
          "sample_size()",
          "```"
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
    
    
    output$text6 <- renderText({
      #paste(strat_layers)
      paste(reactiveValuesToList(strat_layers))
      #paste("Bitte klicken Sie eine der beliebigen Objekte an, um die Auswahl der letzten Stichprobe anzuzeigen.")
    })
    
    output$download_sample <- downloadHandler(
      filename = function() {
        paste("Stichprobe", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        # my_list <- reactiveValuesToList(strat_layers)
        selected_values <- selected_values()
        exists_1 <- any(sapply(selected_values, function(x) x == "Alle auswählen"))
        if (exists_1){
          selected_values <- selected_values[-1]
        }

        value_choices <- value_choices()
        exists_2 <- any(sapply(value_choices, function(x) x == "Alle auswählen"))
        if (exists_2){
          value_choices <- value_choices[-1]
        }
          
        data_type <- reactiveValuesToList(strat_layers)$data_types
        for(i in 1:length(data_type)){
          if(data_type[i] == "categorical"){
            data_type[i] <- "Kategorisch"
          } else if(data_type[i] == "continuous"){
            data_type[i] <- "Numerisch"
          }
        }
            
        my_list <- list(
          selected_column = selected_column(),
          selected_values = selected_values, 
          value_choices = value_choices, 
          ids = reactiveValuesToList(strat_layers)$ids,
          columns = reactiveValuesToList(strat_layers)$columns,
          data_types = data_type, #reactiveValuesToList(strat_layers)$data_types,
          categories = reactiveValuesToList(strat_layers)$categories,
          #data = reactiveValuesToList(strat_layers)$data,
          sel_kind = reactiveValuesToList(strat_layers)$sel_kind,
          sel_params = reactiveValuesToList(strat_layers)$sel_params,
          strata = strata(),
          sample_size = sample_size())
          
        saveRDS(my_list, file = file)
      }
    )

                          
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
      smpl <- uploaded_data()[rownames(smpl),]
      sample(smpl)
    })
    
    
    output$sample_table <- renderDT({
      datatable(sample())
    })
    
  })
}




