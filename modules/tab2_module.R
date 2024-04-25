
# Define UI
tab2ui <- function(id){
  ns <- NS(id)
  fluidPage(
    mainPanel(
      br(),
      br(),
      tags$b(textOutput(ns("data_text"))),
      tags$b(textOutput(ns("text1"))),
      tableOutput(ns("head")),
      br(),
      tags$b(textOutput(ns("text2"))),
      tableOutput(ns("head.akten")),
      br(),
      verbatimTextOutput(ns("summary1")),
      plotOutput(ns("plot1")),
      plotOutput(ns("plot2"), width = "100%", height = "100%"),
      plotOutput(ns("plot3"), width = "100%", height = "100%")
    )
  )
}

tab2server <- function(id, data, karten, akten, clean) {
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
    
    # Saving the data uploaded in another tab and passes to this module.
    uploaded_data <- reactiveVal(NULL)
    my_akten <- reactiveVal(NULL)
    clean_akten <- reactiveVal(NULL)
    my_karte <- reactiveVal(NULL)
    
    observeEvent(data(), {
      uploaded_data(data())
      clean_akten(clean())
      my_akten(akten())
      
      #show head
      output$text1 <- renderText({
        paste("Diese Tabelle zeigt die sechs ersten Reihen des hochgeladenen Datensatzes.")
      })
      output$head <- renderTable({
        head(uploaded_data())
      })
      
      output$text2 <- renderText({
        paste("Diese Tabelle zeigt die sechs ersten Reihen des hochgeladenen Datensatzes auf der transformierten Aktenebene.")
      })
      output$head.akten <- renderTable({
        head(my_akten())
      })
        output$summary1 <- renderPrint({
          summary(my_karte())
        })

        #plot
          output$plot1 <- renderPlot({
            ggplot(my_akten(), aes(Gericht)) +
              geom_bar() +
              scale_x_discrete(guide = guide_axis(angle = 90))
          })
    }) 
      
    observeEvent(karten(), {
      my_karte(karten())
      
          output$plot2 <- renderPlot({
            ggplot(data = my_karte(), aes(fill = court)) +
              geom_sf(aes(geometry = geometry), color = "white") +
              geom_sf_text(
                aes(geometry = geometry, label = court),
                size = 1.5,
                color = "black",
                fun.geometry = function(x)
                  sf::st_centroid(x)
              ) + scale_fill_viridis_d(option = "viridis") +
              guides(fill="none") +
              theme_void()
          },  width = 1000, height = 1000)

          output$plot3 <- renderPlot({
            ggplot(data = my_karte(), aes(fill = `Anzahl der Fälle`)) +
              geom_sf(aes(geometry = geometry), color = "transparent") +
              geom_sf_text(
                aes(geometry = geometry, label = court),
                size = 1.5,
                col = "black",
                fun.geometry = function(x)
                  sf::st_centroid(x)
              ) +
              scale_fill_viridis_b(option = "viridis") +
              theme_void()
          }, width = 1000, height = 1000)
      
    })
    # # Reactive value to store the uploaded dataframe
    # uploaded_data <- reactiveVal(NULL)
    # my_akten <- reactiveVal(NULL)
    # clean_akten <- reactiveVal(NULL)
    # my_karte <- reactiveVal(NULL)
    # 
    # # Upload file
    # observeEvent(input$file, {
    #   #upload data
    #   uploaded_data(read.csv(input$file$datapath))
    #   
    #   #dataset on the "akten" level
    #   akten <- group_by(uploaded_data(), 
    #                     `Gericht`,
    #                     `Aktenzeichen`,
    #                     `Streitwert.in.EURO`,
    #                     `Gesamtstreitgegenstand`,
    #                     `Erledigungsgrund`,
    #                     `Dauer.des.Verfahrens.in.Tagen`,
    #                     `Anzahl.Termine`,
    #                     `Archivstatus`,
    #                     `Anbietungsgrund..manuell.erfasst.`,
    #                     `Anbietungsgrund`)
    #   akten <- summarise(akten, `Anzahl Beteiligte` = n()) 
    #   akten <- as.data.frame(akten)
    #   akten <- mutate(akten,Index = row_number())
    #   
    #   my_akten(akten)
    #   
    #   #clean the "akten" file and mutate
    #   tmp_Gericht <- sub("Amtsgericht ", "", my_akten()$Gericht)
    #   tmp_Gericht <- sub(" Zweigstelle.*", "", tmp_Gericht)
    #   tmp_Gericht <- sub(" i.d. ", " i. d. ", tmp_Gericht)
    #   tmp_Gericht <- sub(" a.d. ", " a. d. ", tmp_Gericht)
    #   tmp_Gericht <- sub(" am ", " a. ", tmp_Gericht)
    #   tmp_Gericht <- sub(" i.OB", " i. OB", tmp_Gericht)
    #   
    #   tmp_Gericht1 <- mutate(my_akten(), `Bezirk` = tmp_Gericht)
    #   clean_akten(tmp_Gericht1)
    #   
    #  
    #   #karte_amtsgerichte$`Anzahl der Fälle` <- fallzahl_pro_bezirk$Anzahl[match(karte_amtsgerichte$court, fallzahl_pro_bezirk$Bezirk)]
    # })
    # 
    # observeEvent(input$file2, {
    #   my_karte(readRDS(input$file2$datapath))
    #   
    #   #regions and their numbers of files 
    #   fallzahlen1 <- group_by(clean_akten(), Bezirk)
    #   fallzahlen1 <- summarise(fallzahlen1, Anzahl = n())
    #   fallzahlen1 <- as.data.frame(fallzahlen1)
    #   
    #   
    #   my_karte1 <- my_karte()
    #   my_karte1$`Anzahl der Fälle` <- fallzahlen1$Anzahl[match(my_karte1$court, fallzahlen1$Bezirk)]
    #   my_karte(my_karte1)
    # })
    
  
    
     # Return uploaded data
     return(clean_akten)
  })
}