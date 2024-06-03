#library(rmarkdown)
library(DT)

# Define UI
tab2ui <- function(id){
  ns <- NS(id)
  fluidPage(
    fluidRow(
      br(),
      tags$b(textOutput(ns("data_text")), style="color:red"),
      br(),
      tags$b(textOutput(ns("text_main"))),
      br()
    ),
    br(),
    fluidRow(
    sidebarPanel(
              radioButtons(
                width = 300,
                inputId = ns("chosen_data"),
                label = "Bitte wählen Sie einen Datensatz aus, den Sie einsehen möchten",
                choices = c("Metadaten der Akten", "Karten-Datensatz"),
                selected = "Metadaten der Akten")),
    mainPanel(
      tags$b(textOutput(ns("text1"))),
      verbatimTextOutput(ns("summary")))),
    fluidRow(
      sidebarPanel("Dieser Abschnitt zeigt die Statistiken der Akten-Datei"),
      mainPanel(
        #tableOutput(ns("head")),
        br(),
        tags$b(textOutput(ns("text2"))),
        #tableOutput(ns("head.akten")),
        DTOutput(ns("head.akten")),
        br(),
        tags$b(textOutput(ns("text3"))),
        br(),
        plotOutput(ns("plot1")),
        br(), 
        # selectInput(inputId = "my_col", 
        #             label = "Wählen Sie die Spalte aus, die Sie näher einsehen möchten.",
        #             choices = names(my_akten()),
        #             selected = "Gericht"),
        # tableOutput(ns("summary1"))
      )
    ),
    br(),
    fluidRow(
      sidebarPanel("Dieser Abschnitt zeigt die Karten Grafiken"),
      mainPanel(
        #verbatimTextOutput(ns("summary1")),
        tags$b(textOutput(ns("text4"))),
        br(),
        plotOutput(ns("plot2"), width = "100%", height = "100%"),
        br(),
        tags$b(textOutput(ns("text5"))),
        br(),
        plotOutput(ns("plot3"), width = "100%", height = "100%"),
        br(),
        br(),
        #radioButtons("format", "Document format", c("PDF", "HTML", "Word"), inline = TRUE),
        #downloadButton("downloadReport")
      ),
    fluidRow(
      column(width = 10),
      # column(width = 5, downloadButton(ns("download_report"), "Report herunterladen"),
      #        br(),
      #        actionButton(ns("create_rmd"), "Erstelle den Report")),
      column(width = 2, actionButton(ns("next_tab"), "Nächste Seite")), 
      br(), 
      br()),
    fluidRow(
      tags$b(textOutput(ns("next1")))
      ) 
    )
  )
}

tab2server <- function(id, data, map_file) {
  moduleServer(id, function(input, output, session) {
    
    #Include a text, when the data is missing 
    data_loaded <- reactiveVal(FALSE)
    observeEvent(data(), {
      data_loaded(TRUE)
    })
    output$data_text <- renderText({
      if (!data_loaded()) {
        "Bitte laden Sie die hierfür benötigen Daten im voherigen Tab hoch!"
      }
    })
    
    
    observeEvent(data(), {
      
      #Introduction
      output$text_main <- renderText({
        "Hier können Sie die Daten kennenlernen und alle wichtigen deskriptiven Statistiken und Graphen einsehen."
      })
      
      #show head
      output$text1 <- renderText({
        "Hier wird die Zusammenfassung des jeweiliges Datensatzes gezeigt. "
      })
      
      # output$head <- renderTable({
      #   head(uploaded_data())
      # })
      output$summary <- renderPrint({
        if (input$chosen_data == "Metadaten der Akten") {
          summary(data())
        } else {
          summary(map_file())
        }
      })
      
      output$text2 <- renderText({
        "Diese Tabelle zeigt die sechs ersten Reihen des hochgeladenen Datensatzes."
      })
      
      output$head.akten <- renderDT({
        datatable(head(data()), 
                  class = "cell-border stripe", 
                  options = list(dom = "t"))
      })
      
        # output$summary1 <- renderPrint({
        #   summary(my_karte())
        # })
      output$text3 <- renderText({
        "Diese Grafik zeigt die Summe der Akten pro Amtsgerichtsbezirk."
      })
      
        #plot
          output$plot1 <- renderPlot({
            ggplot(data(), aes(Gericht)) +
              geom_bar() +
              scale_x_discrete(guide = guide_axis(angle = 90)) +
              ylab("Anzahl der Fälle") +
              xlab("Amtsgerichtbezirke")
          })
          
          
          # #get summary for a column in my_akten()
          # group_var <- reactive(({input$my_col}))
          # 
          # output$summary1 <- renderTable({
          #   summary(my_akten()[,group_var()])
          # })
    }) 
      
    observeEvent(map_file(), {
      
      output$text4 <- renderText({
      "Diese Grafik zeigt die Aufteilung der Amtsgerichtbezirke, die durch die hochgeladene 
              Karten-Datei definiert wurde."
      })
      
          output$plot2 <- renderPlot({
            ggplot(data = map_file(), aes(fill = court)) +
              geom_sf(aes(geometry = geometry), color = "white") +
              geom_sf_text(
                aes(geometry = geometry, label = court),
                size = 3,
                color = "white",
                fun.geometry = function(x)
                  sf::st_centroid(x)
              ) + scale_fill_viridis_d(option = "viridis") +
              guides(fill="none") +
              theme_void()
          },  width = 800, height = 800)

          
          output$text5 <- renderText({
            "Diese Grafik zeigt die Anzahl der Fälle pro Amtsgerichtbezirk."
          })
          
          output$plot3 <- renderPlot({
            ggplot(data = map_file(), aes(fill = `Anzahl der Fälle`)) +
              geom_sf(aes(geometry = geometry), color = "transparent") +
              geom_sf_text(
                aes(geometry = geometry, label = court),
                size = 3,
                color = "white",
                fun.geometry = function(x)
                  sf::st_centroid(x)
              ) +
              #scale_color_manual(guide = FALSE, values = c("black", "white")) +
              scale_fill_viridis_b(option = "viridis") +
              theme_void()
          }, width = 800, height = 800)
          
          ###added
          # output$downloadReport <- downloadHandler(
          #   filename = function() {
          #     paste("my-report", sep = ".", switch(
          #       input$format, PDF = "pdf", HTML = "html", Word = "docx"
          #     ))
          #   },
          #   
          #   content = function(file) {
          #     src <- normalizePath("Report_pdf.Rmd")
          #     
          #     owd <- setwd(tempdir())
          #     on.exit(setwd(owd))
          #     file.copy(src, "Report_pdf.Rmd", overwrite = TRUE)
          #     
          #     out <- render("Report_pdf.Rmd", switch(
          #       input$format,
          #       PDF = pdf_document(), HTML = html_document(), Word = word_document()),
          #       params = list(data = input$file1$datapath)
          #     )
          #     file.rename(out, file)
          #   }
          # )
          ###############
          # 
          # output$report <- renderUI({
          #   params <- list(data = my_akten())
          #   rmarkdown::render("Report_pdf.Rmd", params = params)
          #   HTML('<a href="report.html" target="_blank">Click here to view the report</a>')
          # })
          # 
          # # Create download link for the knitted R Markdown report
          # output$download_link <- renderUI({
          #   downloadButton("download_report", "Download Report")
          # })
          # 
          # # Function to download the knitted R Markdown report
          # output$download_report <- downloadHandler(
          #   filename = function() {
          #     "Report_pdf.Rmd"
          #   },
          #   content = function(file) {
          #     file.copy("Report_pdf.Rmd", file)
          #   }
          # )
          
          
          #########################
          # # Render the R-Markdown file 
          # render_report <- reactive({
          #   # Render the R Markdown document
          #   render("Report_pdf.Rmd", output_file = NULL, output_format = "pdf_document")
          # })
          # 
          # # Define download handler
          # output$download_report <- downloadHandler(
          #   filename = function() {
          #     "report.pdf"  # Name of the downloaded file
          #   },
          #   content = function(file) {
          #     my_data1 <- my_akten()
          #     params <- list(data = my_data1)
          #     # Knit the R Markdown document and save it to a file
          #     rmarkdown::render("Report_pdf.Rmd", 
          #                       output_format = "pdf_document",
          #                       params = params,
          #                       output_file = file)
          #   }
          # )
          
            #next button ?
            # observeEvent(input$next_tab, {
            #   current_tab <- session$input$tabs
            #   output$next1 <- renderText(
            #     paste(id)
            # 
            #   )
            # })
          
          # observeEvent(input$next_tab, {
          #   updateTabsetPanel(inputId = "one",
          #                     selected = "Grundgesamtheit auswählen")
          # })

    })
    
  })
}