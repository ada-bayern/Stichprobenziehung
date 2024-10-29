#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#source('R/Helpers.R')

library(shiny)
library(shinydashboard)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(sf)
library(tinytex)

source('modules/start.R')
# source('modules/dashboard.R')
# source('modules/selection')
# source('modules/categories')
# source('modules/sample')
# source('modules/overview')
# source('modules/tab1_module.R')
# source('modules/tab2_module.R')
# #source('modules/tab2_1_module.R')
# source('modules/tab3_module.R')
# source('modules/tab4_module.R')
# source('modules/tab5_module.R')
# source('modules/tab6_module.R')


# Define UI
# ui <- fluidPage(
#     theme = shinythemes::shinytheme('flatly'),
#     #themeSelector(),
#     titlePanel( 'ADA Bayern Stichprobenziehung'
#         # app title/description
#     ),
#     p('Willkommen beim Stichproben-Tool. Hier können Sie Ihre Daten laden und direkt 
#                 alle wichtigen deskriptiven Statistiken einsehen. In den weiteren Tabs, können Sie 
#                 die geschichtete Stichprobe durchführen und Ihre Stichprobe und die dazugehörige 
#                     Dokumentation herunterladen.'),
  
db_header <- dashboardHeader(title='ADA Bayern Stichprobenziehung', titleWidth=400)
db_sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem('Start', tabName='start', icon=icon('play')),
        menuItem('Datenansicht', tabName='dashboard', icon=icon('dashboard')),
        menuItem('Datenauswahl', tabName='selection', icon=icon('arrow-pointer')),
        menuItem('Kategorien', tabName='categories', icon=icon('icons')),
        menuItem('Stichprobe', tabName='sample', icon=icon('table')),
        menuItem('Überblick', tabName='overview', icon=icon('list-alt'))
    )
)
db_body <- dashboardBody(
    tabItems(
        tabItem('start', start_ui('start')),
        tabItem('dashboard', div(p('Dashboard'))),
        tabItem('selection', div(p('Datenauswahl')))
    )
)
ui <- dashboardPage(
    db_header,
    db_sidebar,
    db_body,
    skin='red'
)

server <- function(input, output, session) {
    start_server('start')
}
#   tabsetPanel(
#     tabPanel('Startseite', tab1ui('tab1')),
#     tabPanel('Daten kennenlernen', tab2ui('tab2')),
#     #tabPanel('Alte Stichprobe einsehen', tab2_1ui('tab2_1')),
#     tabPanel('Grundgesamtheit auswählen', tab3ui('tab3')),
#     # tabPanel('Kategorien erstellen', tab4ui('tab4')),
#     # tabPanel('Stichprobe definieren', tab5ui('tab5')),
#     # tabPanel('Stichprobe einsehen', tab6ui('tab6'))
#   )

# server <- function(input, output, session) {
#   options(shiny.maxRequestSize=160*1024^2)
  
#   output$panel <- renderText({
#     paste('Current panel: ', input$tabset)
#   })
  
#   ret_tab1 <- tab1server('tab1')
#   tab2server('tab2', data = ret_tab1$uploaded_data, map_file = ret_tab1$map_file)
#   #tab2_1server('tab2_1', old = ret_tab1$old_sample, data = ret_tab1$uploaded_data)
#   ret_tab3 <- tab3server('tab3', data = ret_tab1$uploaded_data, old = ret_tab1$old_sample)
#   strat_layers <- tab4server('tab4', data = ret_tab3$filtered_data, presets = ret_tab1$old_sample)
#   ret_tab5 <- tab5server('tab5', strat_layers = strat_layers, presets = ret_tab1$old_sample)
#   tab6server('tab6', 
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

shinyApp(ui, server)
