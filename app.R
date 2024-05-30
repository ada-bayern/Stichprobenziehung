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
library(tinytex)

source("modules/tab1_module.R")
source("modules/tab2_module.R")
source("modules/tab2_1_module.R")
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
    tabPanel("Alte Stichprobe einsehen", tab2_1ui("tab2_1")),
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
  tab2_1server("tab2_1", old = uploaded_data$old_sample, data = uploaded_data$uploaded_data)
  
  filtered <- tab3server("tab3", data = uploaded_data$uploaded_data)
  strat_layers <- tab4server("tab4", data = filtered$filtered_data)
  strata <- tab5server("tab5", strat_layers = strat_layers)
  tab6server("tab6", data = uploaded_data1, name = uploaded_data$the_name, strat_layers = strat_layers, strata = my_strata$strata, 
             sample_size = my_strata$sample_size, name_other = uploaded_data$name_other, 
             selected_column = filtered$selected_column, selected_values = filtered$selected_values,
             value_choices = filtered$value_choices)
}

shinyApp(ui, server)
