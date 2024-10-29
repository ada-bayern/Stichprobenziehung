library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(sf)
library(tinytex)

source("modules/start.R")
source("modules/dashboard.R")
# source("modules/selection")
# source("modules/categories")
# source("modules/sample")
# source("modules/overview")

db_header <- dashboardHeader(
  title = "ADA Bayern Stichprobenziehung",
  titleWidth = 400
)
db_sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Start", tabName = "start", icon = icon("play")),
    menuItem("Datenansicht", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Datenauswahl", tabName = "selection", icon = icon("arrow-pointer")), # nolint
    menuItem("Kategorien", tabName = "categories", icon = icon("icons")),
    menuItem("Stichprobe", tabName = "sample", icon = icon("table")),
    menuItem("Überblick", tabName = "overview", icon = icon("list-alt"))
  )
)
db_body <- dashboardBody(
  tabItems(
    tabItem("start", start_ui("start")),
    tabItem("dashboard", dashboard_ui("dashboard")),
    tabItem("selection", div(p("Datenauswahl")))
  )
)
ui <- dashboardPage(
  db_header,
  db_sidebar,
  db_body,
  skin = "red"
)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 160*1024^2)
  data <- start_server("start")
  dashboard_server("dashboard",
                   csv_data = data$csv_data,
                   map_data = data$rds_data)
}

shinyApp(ui, server)

#   tabsetPanel(
#     tabPanel("Startseite", tab1ui("tab1")),
#     tabPanel("Daten kennenlernen", tab2ui("tab2")),
#     #tabPanel("Alte Stichprobe einsehen", tab2_1ui("tab2_1")),
#     tabPanel("Grundgesamtheit auswählen", tab3ui("tab3")),
#     # tabPanel("Kategorien erstellen", tab4ui("tab4")),
#     # tabPanel("Stichprobe definieren", tab5ui("tab5")),
#     # tabPanel("Stichprobe einsehen", tab6ui("tab6"))
#   )

# server <- function(input, output, session) {
#   options(shiny.maxRequestSize = 160*1024^2)
  
#   output$panel <- renderText({
#     paste("Current panel: ", input$tabset)
#   })
  
#   ret_tab1 <- tab1server("tab1")
#   tab2server("tab2", data = ret_tab1$uploaded_data, map_file = ret_tab1$map_file)
#   #tab2_1server("tab2_1", old = ret_tab1$old_sample, data = ret_tab1$uploaded_data)
#   ret_tab3 <- tab3server("tab3", data = ret_tab1$uploaded_data, old = ret_tab1$old_sample)
#   strat_layers <- tab4server("tab4", data = ret_tab3$filtered_data, presets = ret_tab1$old_sample)
#   ret_tab5 <- tab5server("tab5", strat_layers = strat_layers, presets = ret_tab1$old_sample)
#   tab6server("tab6", 
#              data = ret_tab1$uploaded_data, 
#              ident_primary = ret_tab1$ident_primary,
#              ident_secondary = ret_tab1$ident_secondary,
#              strat_layers = strat_layers, 
#              strata = ret_tab5$strata, 
#              sample_size = ret_tab5$sample_size, 
#              selected_column = ret_tab3$selected_column, 
#              selected_values = ret_tab3$selected_values,
#              value_choices = ret_tab3$value_choices,
#              uploaded_data = ret_tab1$uploaded_data)
# }

