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


ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  #themeSelector(),
  titlePanel( "ADA Bayern Stichprobenziehung"
    # app title/description
  ),
  p("Beschreibung..."),
  tabsetPanel(
    tabPanel("Informationen zur Stichprobe", tab1ui("tab1")),
    tabPanel("Daten kennenlernen", tab2ui("tab2")),
    tabPanel("Grundgesamtheit auswählen", tab3ui("tab3")),
    tabPanel("Kategorien angeben", tab4ui("tab4")),
    tabPanel("Stichprobe definieren", tab5ui("tab5")),
    tabPanel("Stichprobe einsehen")
  )
)
server <- function(input, output, session) {
  options(shiny.maxRequestSize=160*1024^2)
  
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
  
  #tab1server("tab1")
  uploaded_data <- tab1server("tab1")
  uploaded_data1 <- tab2server("tab2", data = uploaded_data$uploaded_data, 
                               karten = uploaded_data$my_karte, akten = uploaded_data$my_akten,
                               clean = uploaded_data$clean_akten)
  filtered_data <- tab3server("tab3", data = uploaded_data1)
  strat_layers <- tab4server("tab4", data = filtered_data)
  tab5server("tab5", strat_layers = strat_layers)
  
  
}

shinyApp(ui, server)
