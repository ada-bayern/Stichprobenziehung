#' Module: Overview over the sample
#'
#' This module provides the UI and server logic for the "Overview and Download"
#' page of a Shiny application. It allows users to view sample data, trigger
#' the final sampling process, and download various reports and data samples in
#' different formats such as PDF, CSV, and RDS.
#'
#' UI Components:
#' - Text inputs for author and dataset ID.
#' - Buttons for downloading report, sample, and dataset.
#' - Action button to trigger sampling process.
#' - Data table displaying the sample data.
#'
#' Server Functionality:
#' - Manages reactive values for sample data and marked datasets.
#' - Downloads are provided via `downloadHandler()` for reports and data files.
#' - Executes sampling process upon button click.
#'
#' Important Variables/Functions:
#' - `overview_ui`: Constructs the UI for the overview tab.
#' - `overview_server`: Handles the server-side logic, including sampling and
#'   downloading processes.
#' - `sample()`: Reactive value holding the current sample data.
#' - `marked_dataset()`: Reactive value containing the dataset with a binary
#'   column marking sampled rows.
#' - `downloadReport()`, `downloadSample()`, `loadSampleButton()`,
#'   `loadDatasetButton()`: Functions handling download actions.

# Load necessary libraries
library(shiny)
library(DT)

# Source external module scripts
source("modules/helpers/tab6_1_strat_sample.R")
source("modules/helpers/utils.R")


# Define UI for the overview module
overview_ui <- function(id) {
  ns <- NS(id)

  # Construct fluid page layout with sidebar and main panel
  fluidPage(
    titlePanel("Überblick und Download"),
    sidebarLayout(
      sidebarPanel(
        fluidRow(
          column(6, textInput(ns("id_name"), "Author-ID")),
          column(6, textInput(ns("id_data"), "Datensatz-ID"))
        ),
        downloadButton(ns("save_pdf"),
                       "Dokumentation als PDF speichern"),
        hr(),
        downloadButton(ns("save_rds"),
                       "Stichprobeninformationen als RDS speichern"),
        hr(),
        uiOutput(ns("save_sample"))
      ),
      mainPanel(
        br(),
        actionButton(ns("sample_button"), "Stichprobe ziehen"),
        hr(),
        div(dataTableOutput(ns("sample_table")), style = "overflow-x: auto;")
      )
    )
  )
}

# Define server logic for the overview module
overview_server <- function(id, uploaded_data, sample_data, settings) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Create reactive values to hold sample data and marked datasets
    sample <- reactiveVal(NULL)
    marked_dataset <- reactiveVal(NULL)

    # Define download handler for documentation report
    output$save_pdf <- downloadHandler(
      filename = function() {
        paste("Dokumentation_Aktenstichprobe_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        # Create a temporary R Markdown file
        temp_report <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", temp_report, overwrite = TRUE)
        params <- settings()
        params$id_name <- input$id_name
        params$id_data <- input$id_data
        # Knit the R Markdown document and save it to a file
        rmarkdown::render(temp_report,
          output_format = "pdf_document",
          output_file = file,
          envir = new.env(parent = globalenv()),
          params = params
        )
      },
      contentType = "application/pdf"
    )

    # Define download handler for sample information as RDS
    output$save_rds <- downloadHandler(
      filename = function() {
        paste("Aktenstichprobe_", Sys.Date(), ".rds", sep = "")
      },
      content = function(file) {
        saveRDS(settings(), file = file)
      }
    )

    # Observer for sample button click to execute sampling
    observeEvent(input$sample_button, {
      strat_layers <- settings()$strat_layers
      strata <- settings()$strata
      ratios <- settings()$ratios
      req(strat_layers, nrow(strata) > 0)

      smpl <- strat_sample(
        data = sample_data(),
        strata_sizes = strata$`Größe Stichprobe`,
        cat_names = lapply(ratios, names)
      )

      # Store as dataset with extra binary column
      dataset <- uploaded_data()
      dataset$Stichprobe <- rep("nein", nrow(dataset))
      dataset[rownames(smpl), "Stichprobe"] <- "ja"

      # Store as sample dataset
      marked_dataset(dataset)
      smpl <- uploaded_data()[rownames(smpl), ]
      sample(smpl)
    })

    # Render data table for displaying sample data
    output$sample_table <- renderDT({
      datatable(sample())
    })

    output$save_sample <- renderUI({
      req(sample())
      fluidRow(column(12,
        downloadButton(ns("save_sample_button"),
                       "Stichprobe als CSV speichern"),
        br(), br(),
        downloadButton(ns("save_dataset_button"),
                       "Markierten Datensatz als CSV speichern"),
        br(), br()
      ))
    })

    # Define download handler for sample as CSV
    output$save_sample_button <- downloadHandler(
      filename = function() {
        paste("Aktenstichprobe_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(sample(), file)
      }
    )

    # Define download handler for marked dataset as CSV
    output$save_dataset_button <- downloadHandler(
      filename = function() {
        paste("Datensatz_markiert_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(marked_dataset(), file)
      }
    )
  })
}