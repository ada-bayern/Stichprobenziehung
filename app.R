#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

#source("R/Helpers.R")

library(shiny)
library(ggplot2)
library(shinythemes)
library(tidyverse)
library(sf)

source("modules/tab1_module.R")
source("modules/tab2_module.R")
source("modules/tab3_module.R")
source("modules/tab4_module.R")
source("modules/tab5_module.R")
source("modules/tab6_module.R")



ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  #themeSelector(),
  titlePanel( "ADA Bayern Stichprobenziehung"
    # app title/description
  ),
  p("Willkommen beim Stichproben-Tool. Hier können Sie Ihre Daten laden und direkt 
               alle wichtigen deskriptiven Statistiken einsehen. In den weiteren Tabs, können Sie 
               die geschichtete Stichprobe durchführen und Ihre Stichprobe und die dazugehörige 
                Dokumentation herunterladen."),
  
  tabsetPanel(
    tabPanel("Startseite", tab1ui("tab1")),
    tabPanel("Daten kennenlernen", tab2ui("tab2")),
    tabPanel("Grundgesamtheit auswählen", tab3ui("tab3")),
    tabPanel("Stichprobe festlegen", tab4ui("tab4")),
    tabPanel("Stichprobe definieren", tab5ui("tab5")),
    tabPanel("Stichprobe einsehen", tab6ui("tab6"))
  )
)
server <- function(input, output, session) {
  options(shiny.maxRequestSize=160*1024^2)
  
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
  
  #tab1server("tab1")
  uploaded_data <- tab1server("tab1")
  tab2server("tab2", data = uploaded_data$uploaded_data, 
                               map_file = uploaded_data$map_file)
  filtered_data <- tab3server("tab3", data = uploaded_data$uploaded_data)
  strat_layers <- tab4server("tab4", data = filtered_data)
  tab5server("tab5", strat_layers = strat_layers)
  tab6server("tab6")
  
}

shinyApp(ui, server)
