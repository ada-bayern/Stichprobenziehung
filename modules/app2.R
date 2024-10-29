
library(shiny)
library(shinydashboard)

# TODO: to Dashboard
db_header <- dashboardHeader(title='Datenerkundung')
db_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Start", tabName="start", icon=icon("dashboard")),
        menuItem("Datenansicht", tabName="dashboard", icon=icon("dashboard")),
        menuItem("Datenauswahl", tabName="selction", icon=icon("dashboard"))
    )
)
db_body <- dashboardBody()

ui <- function(id){
    dashboardPage(db_header, db_sidebar, db_body)
    # TODO: DataFrame widget
    # TODO: Description widget
    # TODO: Map Widget
    # TODO: Bar plot Widget
}

server <- function(id, data, map_file) {
    moduleServer(id, function(input, output, session) {})
}

shinyApp(ui, server)