library(shiny)

ui <- fluidPage(
  column(
    3, 
    titlePanel("CROWN Sitemap Generator"),
    uiOutput("prov"), br(), 
    fluidRow(
      column(
        6, uiOutput("col"), br()
        ),
      column(
        6, 
        uiOutput("states"), br(),
        uiOutput("yrs"), br()
        )
      ),
    numericInput(
      "sz", "Map size", 
      1400, min = 100, max = 2000, 
      step = 50, width = '100%'
      ), 
    br(),
    downloadButton("savebtn", "Save map to image (may take ~5 seconds)"),
    actionButton("reset", "Reset zoom", icon = icon("undo", class = "fa-xs"))
    ),
  column(9, shinycssloaders::withSpinner(uiOutput("map_r"), type = 6))
)

