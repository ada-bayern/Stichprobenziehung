library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tinytex)

source("modules/tab1_start.R")
source("modules/tab2_dashboard.R")
source("modules/tab3_selection.R")
source("modules/tab4_categories.R")
#source("modules/sample.R")
# source("modules/tab5_sample.R")
# source("modules/tab6_overview.R")
source("modules/manual.R")

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
    menuItem("Anleitung", tabName = "manual", icon = icon("question"))
  )
)
db_body <- dashboardBody(
  #useShinyjs(), # init shinyjs
  tabItems(
    tabItem("start", start_ui("start")),
    tabItem("dashboard", dashboard_ui("dashboard")),
    tabItem("selection", selection_ui("selection")),
    tabItem("categories", categories_ui("categories")),
    #tabItem("sample", sample_ui("sample")),
    tabItem("manual", manual_ui("manual"))
  ),
  hr(),
  actionButton("next_button", "Weiter")
)
ui <- dashboardPage(
  db_header,
  db_sidebar,
  db_body,
  skin = "green"
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 160 * 1024^2)
  ret_start <- start_server("start")
  dashboard_server("dashboard", csv_data = ret_start$data)
  ret_selection <- selection_server("selection", csv_data = ret_start$data)
  ret_categories <- categories_server("categories",
                                      csv_data = ret_selection$data,
                                      presets = reactiveVal(NULL))
#   ret_sample <- sample_server("sample",
#                               strat_layers = ret_categories$strat_layers,
#                               data = ret_categories$data,
#                               presets = reactiveVal(NULL))
  manual_server("manual")

  # Observe the Next button in the main server function
  observeEvent(input$next_button, {
    current_tab <- input$menu
    next_tab <- case_when(
      current_tab == "start" & ret_start$done() ~ "dashboard",
      current_tab == "dashboard" ~ "selection",
      current_tab == "selection" ~ "categories",
      #current_tab == "categories" ~ "sample",
      #current_tab == "sample" ~ "overview",
      .default = current_tab
    )
    updateTabItems(session, "menu", selected = next_tab)
  })
}

shinyApp(ui, server)