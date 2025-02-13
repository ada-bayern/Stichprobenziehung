#' Main: ADA Bayern Stichprobenziehung Shiny App
#'
#' This module creates a Shiny Application for the ADA Bayern
#' Stichprobenziehung, a system for processing sample data.
#' The application uses a dashboard layout with different tabs for various
#' stages of data processing, including file upload, data viewing, filtering,
#' categorizing, sampling, and overview.
#'
#' Libraries and Imports:
#' - Utilizes libraries such as shiny, shinydashboard, shinythemes, dplyr, and
#'   tinytex for UI creation and data processing.
#' - Imports functions from external R scripts for modular tab handling
#'   (`manual.R`, `tab1_start.R`, `tab2_dashboard.R`, `tab3_filter.R`,
#'   `tab4_categories.R`, `tab5_sample.R`, `tab6_overview.R`).
#'
#' UI Structure:
#' - Header: Contains the application title.
#' - Sidebar: Provides navigation through menu items, each representing a
#'            different processing stage.
#' - Body: Displays content of currently selected tab, each managing UI for a
#'         specific data process.
#'
#' Server Functionality:
#' - Sets maximum file upload size.
#' - Orchestrates communication between different server modules (tabs),
#'   passing data and states through.
#' - Contains logic for the "Next" button to advance through tabs sequentially.
#'
#' Important Variables/Functions:
#' - `ui`: Defines the overall layout of UI components.
#' - `server`: Manages the functional logic of the app including interactivity
#'   and data management.
#' - `dashboardHeader`, `dashboardSidebar`, `dashboardBody`: Define the main
#'   sections of the Shiny dashboard (header, sidebar menu, main body).
#' - Different tab server functions (`manual_server`, `start_server`, etc.) for
#'   handling logic specific to each tab.
#' - Reactive values are utilized to hold and manage state and data flows
#'   between tabs.
#' - `observeEvent`: Reacts to interactions with UI components, such as button
#'   clicks.

# Load necessary libraries
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(tinytex)

# Source external module scripts
source("modules/tab0_manual.R")
source("modules/tab1_start.R")
source("modules/tab2_dashboard.R")
source("modules/tab3_filter.R")
source("modules/tab4_categories.R")
source("modules/tab5_sample.R")
source("modules/tab6_overview.R")

# Define dashboard header
db_header <- dashboardHeader(
  title = "ADA Bayern Stichprobenziehung",
  titleWidth = 400
)

# Define dashboard sidebar with menu items
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
  ),
  collapsed = TRUE
)

# Define dashboard body including tab items
db_body <- dashboardBody(
  # Send signal to server that tab was closed
  tags$script(
    '
    window.onbeforeunload = function() {
      Shiny.setInputValue("tab_closed", true);
    };
    '
  ),
  actionButton("next_button", "Weiter"),
  hr(),
  tabItems(
    tabItem("start", start_ui("start")),
    tabItem("dashboard", dashboard_ui("dashboard")),
    tabItem("filter", filter_ui("filter")),
    tabItem("categories", categories_ui("categories")),
    tabItem("sample", sample_ui("sample")),
    tabItem("overview", overview_ui("overview")),
    tabItem("manual", manual_ui("manual"))
  )
)

# Construct UI for the Shiny app
ui <- dashboardPage(
  db_header,
  db_sidebar,
  db_body,
  skin = "green"
)

# Define server logic required by Shiny app
server <- function(input, output, session) {

  # Set maximum request size for file uploads
  options(shiny.maxRequestSize = 500 * 1024^2)

  # Store settings of this session in reactive value
  settings <- reactiveVal(list())

  # Call server logic for each tab module
  manual_server("manual")
  ret_start <- start_server("start")
  dashboard_server("dashboard", csv_data = ret_start$data)
  ret_filter <- filter_server("filter",
                              csv_data = ret_start$data,
                              presets = ret_start$presets)
  ret_categories <- categories_server("categories",
                                      dataset = ret_filter$data,
                                      presets = ret_start$presets)
  ret_sample <- sample_server("sample",
                              dataset = ret_categories$data,
                              presets = ret_start$presets)
  overview_server("overview",
                  uploaded_data = ret_start$data,
                  sample_data = ret_categories$data,
                  settings = settings)

  # Store settings in reactive value
  observe({
    settings(list(
      cols = colnames(ret_start$data()),
      filters = ret_filter$filters(),
      strat_layers = ret_categories$strat_layers(),
      ratios = ret_sample$ratios(),
      strata = ret_sample$strata(),
      sample_size = ret_sample$sample_size()
    ))
  })
  # Observer for "Next" button clicks to navigate through tabs
  observeEvent(input$next_button, {
    current_tab <- input$menu
    next_tab <- case_when(
      current_tab == "manual" ~ "start",
      current_tab == "start" & ret_start$done() ~ "dashboard",
      current_tab == "dashboard" ~ "filter",
      current_tab == "filter" ~ "categories",
      current_tab == "categories" ~ "sample",
      current_tab == "sample" & !is.null(ret_sample$strata()) ~ "overview",
      .default = current_tab
    )
    updateTabItems(session, "menu", selected = next_tab)

    if (current_tab == "overview") {
      showModal(modalDialog(
        title = "Schließen bestätigen",
        "Sind Sie sicher, dass Sie die App schließen möchten?",
        footer = tagList(
          modalButton("Abbrechen"),
          actionButton("confirm_close", "Ja, App schließen")
        )
      ))
    }

  })

  # Change Next button to Close button in the last tab
  observeEvent(input$menu, {
    if (input$menu == "overview") {
      updateActionButton(
        session = session,
        inputId = "next_button",
        label = "Schließen"
      )
    } else {
      updateActionButton(
        session = session,
        inputId = "next_button",
        label = "Weiter"
      )
    }
  })

  # Close App by user command
  observeEvent(input$confirm_close, {
    removeModal()
    stopApp("App durch Nutzer*in geschlossen.")
  })
  observeEvent(input$tab_closed, {
    removeModal()
    # TODO: save RDS in tmp folder to load later
    stopApp("App durch Nutzer*in geschlossen.")
  })
}

# Run the Shiny application
shinyApp(ui, server)