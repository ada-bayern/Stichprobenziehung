library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tinytex)

source("modules/manual.R")
source("modules/tab1_start.R")
source("modules/tab2_dashboard.R")
source("modules/tab3_filter.R")
source("modules/tab4_categories.R")
source("modules/tab5_sample.R")
# source("modules/tab6_overview.R")

db_header <- dashboardHeader(
  title = "ADA Bayern Stichprobenziehung",
  titleWidth = 400
)
db_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "menu",
    menuItem("Anleitung", tabName = "manual", icon = icon("question")),
    menuItem(""),
    menuItem("Upload", tabName = "start", icon = icon("play")),
    menuItem("Datenansicht", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Datenfilterung", tabName = "filter", icon = icon("arrow-pointer")), # nolint
    menuItem("Kategorien", tabName = "categories", icon = icon("icons")),
    menuItem("Stichprobe", tabName = "sample", icon = icon("table")),
    menuItem("Überblick", tabName = "overview", icon = icon("list-alt"))
  )
)
db_body <- dashboardBody(
  tabItems(
    tabItem("start", start_ui("start")),
    tabItem("dashboard", dashboard_ui("dashboard")),
    tabItem("filter", filter_ui("filter")),
    tabItem("categories", categories_ui("categories")),
    tabItem("sample", sample_ui("sample")),
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
  manual_server("manual")
  ret_start <- start_server("start")
  dashboard_server(
    "dashboard",
    csv_data = ret_start$data
  )
  ret_filter <- filter_server(
    "filter",
    csv_data = ret_start$data
  )
  ret_categories <- categories_server(
    "categories",
    csv_data = ret_filter$data,
    presets = reactiveVal(NULL)
  )
  ret_sample <- sample_server(
    "sample",
    strat_layers = ret_categories$strat_layers,
    presets = reactiveVal(NULL)
  )

  # Observe the Next button in the main server function
  observeEvent(input$next_button, {
    current_tab <- input$menu
    next_tab <- case_when(
      current_tab == "manual" ~ "start",
      current_tab == "start" & !is.null(ret_start$data) ~ "dashboard",
      current_tab == "dashboard" ~ "selection",
      current_tab == "selection" ~ "categories",
      current_tab == "categories" ~ "sample",
      #current_tab == "sample" ~ "overview",
      .default = current_tab
    )
    updateTabItems(session, "menu", selected = next_tab)
  })
}

shinyApp(ui, server)