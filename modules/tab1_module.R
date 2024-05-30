
# Define UI
tab1ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
    br(),
    column(width = 8,
           tags$b("Allgemeine Informationen:"),
              h5("Bitte laden Sie hier alle notwendigen Dateien hoch und geben alle wichtigen Informationen in die 
                 vorgegebenen Textfelder ein. Alle Informationen, die Sie manuell eingeben, werden gespeichert und 
                 in die Dokumentation der Stichprobe übernommen. Diese können Sie in dem letzten Tab 'Stichprobe einsehen' herunterladen. Bitte beachten Sie, dass Sie die Tabs immer von links nach rechts bedienen und keine 
                 Tabs überspringen.")),
    br()),
    tags$b("Erstellen Sie eine neue Stichprobe:"),
    br(),
    br(),
    fluidRow(
      sidebarPanel(
        h5("Bitte laden Sie hier die Metadaten der Akten und die Kartendatei hoch."), 
        fileInput(ns("file"), "Metadaten (CSV-Format, max. 160mb) ", buttonLabel = "Hochladen"),
        fileInput(ns("file2"), "Kartendatei (RDS-Format)", buttonLabel = "Hochladen"),
      ),
    sidebarPanel(
      h5("Diese Angaben werden in die Stichproben Dokumentation übernommen."),
           textInput(ns("the_name"), "Bitte geben Sie hier Identifikation Informationen an."),
           textInput(ns("name_other"), "Bitte geben Sie hier alle relevanten Informationen der Stichprobenziehung an. (z.B. Jahr, Aktentyp,...)")
           )
    ),
    br(),
    tags$b("ODER sehen Sie eine bisher gezogene Stichprobe ein:"),
    br(),
    br(),
    fluidRow(
      sidebarPanel(
        h5("Hier können Sie Ihre vorherig gezogene Stichprobe hochladen und alle deskriptiven Statistiken 
           und Informationen einsehen. Laden Sie hierfür die ...Datei hoch, die Sie bei der Stichprobe neben der Dokumentation und 
           CSV-Datei herunterladen haben."), 
        fileInput(ns("file3"), "Stichprobendatei", buttonLabel = "Hochladen")
      )
      
    )
  )
}

tab1server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store the uploaded dataframe
    uploaded_data <- reactiveVal(NULL)
    my_akten <- reactiveVal(NULL)
    clean_akten <- reactiveVal(NULL)
    my_karte <- reactiveVal(NULL)
    
    old_sample <- reactiveVal(NULL)
    the_name <- reactiveVal(NULL)
    name_other <- reactiveVal(NULL)
    
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
    
    observeEvent(input$file3, {
      
      old_sample(readRDS(input$file3$datapath))
      
    })
    
    observeEvent(input$the_name, {
      the_name(input$the_name)
      
    })
    
    observeEvent(input$name_other, {
      name_other(input$name_other)
      
    })
    
    # Return uploaded data
    return(list(clean_akten = clean_akten, my_karte = my_karte, 
                my_akten = my_akten, uploaded_data = uploaded_data, 
                old_sample = old_sample, the_name = the_name, 
                name_other = name_other))
    
  })
}