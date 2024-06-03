
# Define UI
tab1ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
    br(),
    column(width = 8,
           tags$b("Allgemeine Informationen:"),
              h5("Bitte laden Sie hier alle notwendigen Dateien hoch und geben Sie alle wichtigen Informationen in die 
                 vorgegebenen Textfelder ein. Alle Informationen, die Sie manuell eingeben, werden gespeichert und 
                 in die Dokumentation der Stichprobe übernommen. Diese können Sie in dem letzten Tab 'Stichprobe einsehen' herunterladen. Bitte beachten Sie, dass Sie die Tabs immer von links nach rechts bedienen und keine 
                 Tabs überspringen.")),
    br()),
    tags$b("Erstellen Sie eine neue Stichprobe:"),
    br(),
    br(),
    fluidRow(
      sidebarPanel(
        h5("Bitte laden Sie hier die Daten und die Kartendatei hoch."), 
        fileInput(ns("file"), "Daten (CSV-Format, max. 160mb) ", buttonLabel = "Hochladen"),
        fileInput(ns("file2"), "Kartendatei (RDS-Format)", buttonLabel = "Hochladen"),
      ),
    sidebarPanel(
      h5("Diese Angaben werden in die Stichproben Dokumentation übernommen."),
           textInput(ns("the_name"), "Bitte geben Sie hier eine Identifikation an."),
           textInput(ns("name_other"), "Bitte geben Sie hier alle relevanten Informationen der Stichprobenziehung an. (z.B. Jahr, Aktentyp,...)")
           )
    )
    # ,
    # br(),
    # tags$b("ODER sehen Sie eine bisher gezogene Stichprobe ein:"),
    # br(),
    # br(),
    # fluidRow(
    #   sidebarPanel(
    #     h5("Hier können Sie Ihre vorherig gezogene Stichprobe hochladen und alle deskriptiven Statistiken 
    #        und Informationen einsehen. Laden Sie hierfür die ...Datei hoch, die Sie bei der Stichprobe neben der Dokumentation und 
    #        CSV-Datei herunterladen haben."), 
    #     fileInput(ns("file3"), "Stichprobendatei", buttonLabel = "Hochladen")
    #   )
    #   
    # )
  )
}

tab1server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive value to store the uploaded dataframe
    uploaded_data <- reactiveVal(NULL)
    old_sample <- reactiveVal(NULL)
    ident_primary <- reactiveVal(NULL)
    ident_secondary <- reactiveVal(NULL)
    map_file <- reactiveVal(NULL)
    
    # Upload file
    observeEvent(input$file, {
      #upload data
      #TODO: possibly only chose rows chosen by user
      uploaded_data(read.csv(input$file$datapath))
      
    })
    
    observeEvent(input$file2, {
      
      #regions and their numbers of files 
      # TODO: user should be able to pick columns with map region, or this could
      # be recognized automatically
      data_counts <- group_by(uploaded_data(), Bezirk)
      data_counts <- summarise(data_counts, Anzahl = n())
      data_counts <- as.data.frame(data_counts)
      
      
      mf <- readRDS(input$file2$datapath)
      mf$`Anzahl der Fälle` <- data_counts$Anzahl[match(mf$court, data_counts$Bezirk)]
      map_file(mf)
    })
    
    observeEvent(input$file3, {
      
      old_sample(readRDS(input$file3$datapath))
      
    })
    
    observeEvent(input$the_name, {
      ident_primary(input$the_name)
      
    })
    
    observeEvent(input$name_other, {
      ident_secondary(input$name_other)
      
    })
    
    # Return uploaded data
    return(list(uploaded_data = uploaded_data, map_file = map_file, old_sample = old_sample,
               ident_primary = ident_primary, ident_secondary = ident_secondary))
    
  })
}