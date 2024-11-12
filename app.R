library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tinytex)

source("modules/start.R")
source("modules/dashboard.R")
source("modules/selection.R")
# source("modules/categories")
# source("modules/sample")
# source("modules/overview")

db_header <- dashboardHeader(
  title = "ADA Bayern Stichprobenziehung",
  titleWidth = 400
)
db_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menu",
    menuItem("Start", tabName = "start", icon = icon("play")),
    menuItem("Datenansicht", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Datenauswahl", tabName = "selection", icon = icon("arrow-pointer")), # nolint
    menuItem("Kategorien", tabName = "categories", icon = icon("icons")),
    menuItem("Stichprobe", tabName = "sample", icon = icon("table")),
    menuItem("Überblick", tabName = "overview", icon = icon("list-alt")),
    menuItem(""),
    menuItem("Manual", tabName = "manual", icon = icon("question"))
  )
)
db_body <- dashboardBody(
  useShinyjs(), # init shinyjs
  tabItems(
    tabItem("start", start_ui("start")),
    tabItem("dashboard", dashboard_ui("dashboard")),
    tabItem("selection", selection_ui("selection"))
  ),
  actionButton("next_button", "Weiter")
)
ui <- dashboardPage(
  db_header,
  db_sidebar,
  db_body,
  skin = "green"
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 160*1024^2)
  ret_start <- start_server("start")
  dashboard_server("dashboard", csv_data = ret_start$data)
  selection_server("selection", csv_data = ret_start$data)

  # Observe the Next button in the main server function
  observeEvent(input$next_button, {
    current_tab <- input$menu
    next_tab <- case_when(
      current_tab == "start" & ret_start$done() ~ "dashboard",
      current_tab == "dashboard" ~ "selection",
      .default = current_tab
    )
    updateTabItems(session, "menu", selected = next_tab)
  })
}

shinyApp(ui, server)