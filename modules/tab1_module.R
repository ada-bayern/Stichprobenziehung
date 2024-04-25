
# Define UI
tab1ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Informationen"),
    br(),
    h3("Bitte laden Sie hier die gewünschte Datei im CSV-Format hoch. (Max. 160mb) "), 
    fileInput(ns("file"), "Daten", buttonLabel = "Hochladen"),
    h3("Hier können Sie optional die Karten-Datei im RDS-Format hochladen."), 
    fileInput(ns("file2"), "Karte", buttonLabel = "Hochladen"),
    br()
  )
}

tab1server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store the uploaded dataframe
    uploaded_data <- reactiveVal(NULL)
    my_akten <- reactiveVal(NULL)
    clean_akten <- reactiveVal(NULL)
    my_karte <- reactiveVal(NULL)
    
    # Upload file
    observeEvent(input$file, {
      #upload data
      uploaded_data(read.csv(input$file$datapath))
      
      #dataset on the "akten" level
      akten <- group_by(uploaded_data(), 
                        `Gericht`,
                        `Aktenzeichen`,
                        `Streitwert.in.EURO`,
                        `Gesamtstreitgegenstand`,
                        `Erledigungsgrund`,
                        `Dauer.des.Verfahrens.in.Tagen`,
                        `Anzahl.Termine`,
                        `Archivstatus`,
                        `Anbietungsgrund..manuell.erfasst.`,
                        `Anbietungsgrund`)
      akten <- summarise(akten, `Anzahl Beteiligte` = n()) 
      akten <- as.data.frame(akten)
      akten <- mutate(akten,Index = row_number())
      
      my_akten(akten)
      
      #clean the "akten" file and mutate
      tmp_Gericht <- sub("Amtsgericht ", "", my_akten()$Gericht)
      tmp_Gericht <- sub(" Zweigstelle.*", "", tmp_Gericht)
      tmp_Gericht <- sub(" i.d. ", " i. d. ", tmp_Gericht)
      tmp_Gericht <- sub(" a.d. ", " a. d. ", tmp_Gericht)
      tmp_Gericht <- sub(" am ", " a. ", tmp_Gericht)
      tmp_Gericht <- sub(" i.OB", " i. OB", tmp_Gericht)
      
      tmp_Gericht1 <- mutate(my_akten(), `Bezirk` = tmp_Gericht)
      clean_akten(tmp_Gericht1)
      
      
      #karte_amtsgerichte$`Anzahl der Fälle` <- fallzahl_pro_bezirk$Anzahl[match(karte_amtsgerichte$court, fallzahl_pro_bezirk$Bezirk)]
    })
    
    observeEvent(input$file2, {
      my_karte(readRDS(input$file2$datapath))
      
      #regions and their numbers of files 
      fallzahlen1 <- group_by(clean_akten(), Bezirk)
      fallzahlen1 <- summarise(fallzahlen1, Anzahl = n())
      fallzahlen1 <- as.data.frame(fallzahlen1)
      
      
      my_karte1 <- my_karte()
      my_karte1$`Anzahl der Fälle` <- fallzahlen1$Anzahl[match(my_karte1$court, fallzahlen1$Bezirk)]
      my_karte(my_karte1)
    })
    
    # Return uploaded data
    return(list(clean_akten = clean_akten, my_karte = my_karte, 
                my_akten = my_akten, uploaded_data = uploaded_data))
    
  })
}