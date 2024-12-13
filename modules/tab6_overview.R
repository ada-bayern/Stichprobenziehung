library(shiny)
library(DT)

source("modules/stratification/strat_sample.R")
source("modules/utils.R")


# Define UI
overview_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Überblick und Download"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(column(6, textInput(ns("id_name"), "Author-ID")),
                 column(6, textInput(ns("id_data"), "Datensatz-ID"))),
        downloadButton(ns("download_report"), "Dokumentation als PDF laden"),
        hr(),
        downloadButton(ns("load_sample_button"), "Stichprobe als CSV laden"),
        br(), br(),
        downloadButton(ns("load_dataset_button"),
                       "Markierten Datensatz als CSV laden"),
        br(), br(),
        downloadButton(ns("download_sample"),
                       "Stichprobeninformationen als RDS laden")
      ),
      mainPanel(
        br(),
        actionButton(ns("sample_button"), "Stichprobe ziehen"),
        hr(),
        div(dataTableOutput(ns("sample_table")),
            style = "overflow-x: auto;")
      )
    )
  )
}


overview_server <- function(id, uploaded_data, filters, strat_layers, ratios,
                            strata, sample_size) {
  moduleServer(id, function(input, output, session) {

    sample <- reactiveVal(NULL)
    marked_dataset <- reactiveVal(NULL)

    # Define download handler
    output$download_report <- downloadHandler(
      filename = function() {
        paste("Dokumentation_Aktenstichprobe_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Create a temporary R Markdown file
        temp_report <- file.path(tempdir(), "report.Rmd")

        file.copy("report.Rmd", temp_report, overwrite = TRUE)

        # Knit the R Markdown document and save it to a file
        rmarkdown::render(temp_report,
                          output_format = "pdf_document",
                          output_file = file,
                          envir = new.env(parent = globalenv()),
                          params = list(
                            id_name = input$id_name,
                            id_data = input$id_data,
                            selected_cols = colnames(uploaded_data()),
                            filters = filters(),
                            strat_layers = strat_layers(),
                            ratios = ratios(),
                            strata = strata(),
                            sample_size = sample_size()
                          ))
      },
      contentType = "application/pdf"
    )

    output$download_sample <- downloadHandler(
      filename = function() {
        paste("Aktenstichprobe_", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        strl <- strat_layers()
        for (l in strl) {
          l$col <- NULL
        }
        rds <- list(
          cols = colnames(uploaded_data()),
          filters = filters(),
          strat_layers = strl,
          strata = strata(),
          sample_size = sample_size()
        )

        saveRDS(rds, file = file)
      }
    )


    observeEvent(input$sample_button, {
      req(strat_layers(), nrow(strata()) > 0)

      data <- data.frame(features(strat_layers(), "col"))
      colnames(data) <- features(strat_layers(), "name")

      smpl <- strat_sample(
        data = data,
        strata_sizes = strata()[, "Größe Stichprobe"],
        cat_names = lapply(ratios(), names)
      )
      # store as dataset with extra binary column
      dataset <- uploaded_data()
      dataset$Stichprobe <- rep("nein", nrow(dataset))
      dataset[rownames(smpl), "Stichprobe"] <- "ja"

      # store as sample dataset
      marked_dataset(dataset)
      smpl <- uploaded_data()[rownames(smpl), ]
      sample(smpl)
    })

    output$sample_table <- renderDT({
      datatable(sample())
    })

    output$load_sample_button <- downloadHandler(
      filename = function() {
        paste("Aktenstichprobe_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(sample(), file)
      }
    )

    output$load_dataset_button <- downloadHandler(
      filename = function() {
        paste("Datensatz_markiert_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(marked_dataset(), file)
      }
    )
  })
}
