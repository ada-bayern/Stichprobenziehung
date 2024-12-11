library(shiny)
library(DT)

source("modules/stratification/strat_sample.R")
source("modules/utils.R")


# Define UI
overview_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    titlePanel("Überblick und Download"),
    mainPanel(
      # TODO
      p("Hier können Sie die Dokumentation der Stichprobenziehung
      herunterladen. Alle Angaben, die Sie getätigt haben, wurden gesichert
      und werden in einer PDF-Datei herausgegeben. Neben der Dokumentation
      können Sie auch die CSV-Dateien der Stichprobe herunterladen. Diese
      besteht aus zwei Versionen: Eine enthält den kompletten Datensatz und
      die zweite Version besteht nur aus den Datenpunkten, die auch ausgewählt
      wurden."),
      hr(),
      actionButton(ns("sample_button"), "Stichprobe ziehen"),
      hr(),
      DTOutput(ns("sample_table"))
    ),
    sidebarPanel(
      fluidRow(column(6, textInput(ns("ident_primary"), "Author-ID")),
               column(6, textInput(ns("ident_secondary"), "Datensatz-ID"))),
      actionButton(ns("create_rmd"), "Erstellen der Dokumentation"),
      hr(),
      downloadButton(ns("download_report"), "Dokumentation herunterladen"),
      hr(),
      downloadButton(ns("download_sample"),
                     "Stichprobeninformationen herunterladen")
    )
  )
}


overview_server <- function(id, uploaded_data, filters, strat_layers, ratios,
                            strata, sample_size) {
  moduleServer(id, function(input, output, session) {

    sample <- reactiveVal(NULL)

    ############################################################################
    #### Create R-Markdown and list
    ############################################################################
    observeEvent(input$create_rmd, {

      fs::file_create("Dokumentation_Stichprobe.Rmd")
      file_conn <- file("Dokumentation_Stichprobe.Rmd")
      writeLines(
        c(
          "---",
          #"title: Dokumentation der Stichprobe",
          #"title: Dokumentation der Stichprobe",
          "title: Dokumentation der Stichprobe",
          "date: Erstellt am `r format(Sys.Date(), '%d.%m.%Y')`",
          "author: '`r input$ident_primary`'",
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
          "input$ident_secondary",
          "```",
          "## Diese Spalte/n wurde/n bearbeitet um die Grundgesamtheit zu bestimmen.",
          "```{r}",
          "colnames(uploaded_data())",
          "```",
          # TODO?
          # "## Diese Ausprägungen standen zur Verfügung, um die Grundgesamtheit zu bestimmen.",
          # "```{r}",
          # "value_choices()",
          # "```",
          "## Diese Ausprägungen wurde ausgewählt, um die Grundgesamtheit zu bestimmen.",
          "```{r}",
          "filters()",
          "```",
          "## Diese Spalte/n wurde/n zur neuen Gruppierung ausgewählt.",
          "```{r}",
          "features(strat_layers(), 'name')",
          "```",
          "## Hier sieht man die Datentyp/en der ausgewählen Spalte/n zur neuen Gruppierung.",
          "```{r}",
          "features(strat_layers(), 'data_type')",
          "```",
          "## Hier sieht man die neuen Kategorien der ausgewählen Spalte/n zur neuen Gruppierung.",
          "```{r}",
          "features(strat_layers(), 'categories')",
          "```",
          "## Hier sieht man ausgewählten Auswahlwahrscheinlichkeiten pro Stratum.",
          "```{r}",
          "ratios()",
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
        file_conn
      )
      close(file_conn)

      #Render the R-Markdown file
      render_report <- reactive({
        # Render the R Markdown document
        render("Dokumentation_Stichprobe.Rmd",
               output_file = NULL,
               output_format = "pdf_document")
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

    output$text6 <- renderText({ # TODO
      paste(strat_layers())
      #paste("Bitte klicken Sie eine der beliebigen Objekte an, um die Auswahl der letzten Stichprobe anzuzeigen.")
    })

    output$download_sample <- downloadHandler(
      filename = function() {
        paste("Stichprobe", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        rds <- list(
          cols = colnames(uploaded_data()),
          filters = filters(),
          strat_layers = strat_layers(),
          strata = strata(),
          sample_size = sample_size()
        )

        saveRDS(rds, file = file)
      }
    )


    observeEvent(input$sample_button, {
      req(strat_layers(), nrow(strata()) > 1)

      args <- list(data = data.frame(features(strat_layers(), "col")),
                   strata_sizes = strata()[, "Größe Stichprobe"])

      strat_names <- lapply(strat_layers(), function(layer) {
        names(layer$ratios)
      })
      
      names(strat_names) <- features(strat_layers(), "name")
      args <- c(args, strat_names)
      smpl <- do.call(strat_sample, args) # TODO
      smpl <- uploaded_data()[rownames(smpl), ]
      sample(smpl)
    })

    output$sample_table <- renderDT({
      datatable(sample())
    })

  })
}
