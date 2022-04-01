library(shiny)

rotate <- function(x, n = 1) {
  if (n == 0) x else c(tail(x, -n), head(x, n))
}

tile_choices <- c(
  "OpenStreetMap", "OpenStreetMap.BlackAndWhite", "OpenStreetMap.France",
  "OpenStreetMap.HOT", "Stamen.TonerLite", "Stamen.Terrain", 
  "Esri", "Esri.WorldTopoMap", "CartoDB", "HikeBike"
)


radio_tiles <- radioButtons(
  "prov", "Base map",
  choices = tile_choices, 
  selected = "OpenStreetMap"
)

slider_background <- sliderInput(
  "background", 
  label = "Mute base tiles", 
  min = 0, max = 1, 
  value = 0.9, step = 0.05,
  ticks = F
)

slider_world <- sliderInput(
  "world_mask", 
  label = "Mute background", 
  min = 0, max = 1, 
  value = 0.15, step = 0.05,
  ticks = F
)

slider_state <- sliderInput(
  "state_mask", 
  label = "Mute inactive states", 
  min = 0, max = 1, 
  value = 0.2, step = 0.05,
  ticks = F
)

slider_outlines <- sliderInput(
  "state_outline", 
  label = "Outline active states", 
  min = 0, max = 1, 
  value = 0.05, step = 0.05,
  ticks = F
)

checkbox_projects <- checkboxGroupInput(
  "exps", "Projects",
  choices = c(
    "On-farm" = "onfarm",
    "CE1" = "CE1",
    "CE2" = "CE2",
    "Education" = "Ed"
  ),
  selected = c("onfarm", "CE1", "CE2", "Ed")
)

marker_cols <- radioButtons(
  "col", "Marker colors",
  choices = c(
    RColorBrewer::brewer.pal.info %>% 
      filter(category == "qual") %>% 
      rownames()
  ),
  selected = "Set1"
)


ui <- fluidPage(
  waiter::useWaiter(),
  column(
    3, 
    titlePanel("PSA Sitemap Generator"),
    fluidRow(
      column(
        6, 
        radio_tiles, br(),
        slider_background,
        slider_world,
        slider_state,
        slider_outlines, br(),
        marker_cols
        ),
      column(
        6,
        checkbox_projects, br(), 
        uiOutput("affiliations"), br(),
        uiOutput("yrs"), br()
        )
      ),
    numericInput(
      "sz", "Map width", 
      1400, min = 100, max = 2000, 
      step = 50, width = '100%'
      ), 
    br(),
    downloadButton("savebtn", "Save map to image (may take ~5 seconds)"),
    actionButton("reset", "Reset zoom", icon = icon("undo", class = "fa-xs"))
    ),
  column(9, shinycssloaders::withSpinner(uiOutput("map_r"), type = 6))
)

