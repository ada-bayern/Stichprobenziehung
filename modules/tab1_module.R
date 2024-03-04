
# Define UI
tab1ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Informationen"),
    sidebarLayout(
      sidebarPanel(
        # Add any sidebar content here
      ),
      mainPanel(
        textOutput(ns("text"))
      )
    )
  )
}

tab1server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # TODO: Explanations of the following process
    output$text <- renderText({
      "TODO"
    })
  })
}