library(shiny)



tile_choices <- c(
  "OpenStreetMap", 
  "OpenStreetMap.HOT", 
  "Stamen.TonerLite", "Stamen.Terrain", 
  "Esri", "Esri.WorldTopoMap", "CartoDB"
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
  value = 0.10, step = 0.05,
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
      rownames() %>% 
      stringr::str_subset("Pastel", negate = T) %>% 
      stringr::str_subset("Accent", negate = T),
    LETTERS[1:4]
  ),
  selected = "B"
)


ui <- fluidPage(
  waiter::useWaiter(),
  column(
    4, 
    titlePanel("PSA Sitemap Generator"),
    fluidRow(
      column(
        6, 
        radio_tiles, br(),
        slider_background,
        slider_world,
        slider_state,
        slider_outlines, br(),
        marker_cols,
        actionButton(
          "shuffle",
          "Shuffle colors"
        )
        ),
      column(
        6,
        checkbox_projects,
        uiOutput("affiliations"),
        uiOutput("yrs"),
        actionButton(
          "shift",
          "Shift NE points"
        ), 
        br(),
        br(),
        radioButtons(
          "chart_type",
          label = NULL,
          choices = c("Pie" = "pie", "Pacman" = "polar-area"),
          inline = T,
          selected = "pie"
        ),
        numericInput(
          "sz", "Map width", 
          1200, min = 100, max = 2000, 
          step = 50, width = '100%'
        ),
        sliderInput(
          "ar", "Aspect ratio", 
          min = 0.5, max = 1.5, 
          value = 0.85, step = 0.05,
          ticks = F
        ),
        actionButton("reset", "Reset zoom", icon = icon("undo", class = "fa-xs")),
        downloadButton(
          "savebtn", 
          span("Save map to image", br(), "(may take ~5 seconds)")
          )
        )
      ),
    ),
  column(8, shinycssloaders::withSpinner(uiOutput("map_r"), type = 6))
)

