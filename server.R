library(leaflet)
library(mapview)
library(purrr)
library(dplyr)



library(dbplyr)
library(RPostgres)

# HTTP2 Chrome fix
httr::set_config(httr::config(http_version = 0))

source("secret.R")

server <- function(input, output, session) {

  
  choices <- c(
    "OpenStreetMap", "OpenStreetMap.BlackAndWhite", "OpenStreetMap.France",
    "OpenStreetMap.HOT", "Stamen.TonerLite", "Stamen.Terrain", 
    "Esri", "Esri.WorldTopoMap", "CartoDB", "HikeBike", "Wikimedia"
  )
  
  
  output$prov <- renderUI({
    radioButtons(
      "prov", "Base map",
      choices = choices, selected = "Wikimedia"
    )
  })
  
  output$col <- renderUI({
    radioButtons(
      "col", "Marker color",
      choices = c(
        'red', 'darkred', 'orange', 'green', 'darkgreen', 
        'blue', 'purple', 'darkpurple', 'cadetblue'
        )
    )
  })  
  
  sites <- safely(tbl)(con, "site_information")
  
  if (is.null(sites$result)) {
    con <- dbConnect(
      Postgres(),
      dbname = pg_dbname,
      host = pg_host,
      port = pg_port,
      user = pg_user,
      password = pg_password,
      sslmode = "require"
    )
    
    sites <- safely(tbl)(con, "site_information")
  }
  

  all_sites <- sites$result %>% 
    select(code, year, state, longitude, latitude, producer_id) %>% 
    filter(!is.na(latitude)) %>% 
    collect() %>% 
    mutate(
      state = replace(state, state == "PA", "MD"),
      state = replace(state, state == "SC", "NC")
      ) %>% 
    distinct(code, .keep_all = TRUE)
  
  
  output$states <- renderUI({
    checkboxGroupInput(
      "states", "Show teams:", inline = F,
      choices = sort(unique(all_sites$state)),
      selected = sort(unique(all_sites$state))
    )
  })  
  
  output$yrs <- renderUI({
    checkboxGroupInput(
      "yrs", "Show years:", inline = F,
      choices = sort(unique(all_sites$year)),
      selected = sort(unique(all_sites$year))
    )
  }) 
  

  
  
  ico <- reactive({awesomeIcons(icon = "unchecked", markerColor = input$col)})
  
  mobj <- reactive({
    if (is.null(input$prov)) return(NULL)
    
    leaflet(options = leafletOptions(attributionControl = F)) %>% 
      addProviderTiles(input$prov) %>% 
      addAwesomeMarkers(
        data = all_sites %>% filter(state %in% input$states, year %in% input$yrs),
        lat = ~latitude, lng = ~longitude, icon = ico(),
        options = markerOptions(opacity = 0.75)
        )
    
  })
  
  output$map <- renderLeaflet({
    input$reset
    mobj() %>% clearBounds()
  })

  output$map_r <- renderUI({
    leafletOutput(
      "map", 
      width = min(input$sz, 2000), 
      height = min(input$sz, 2000)
      )
  })
  
  if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }
  
  output$savebtn <- downloadHandler(
    filename = function() {
      paste0("CROWN_map_", Sys.time(), ".png")
      },
    
    content = function(file) {
      mapshot(mobj(), file = file, vwidth = input$sz, vheight = input$sz)
    }
  )
  
}


