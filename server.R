library(leaflet)
library(mapview)
library(purrr)
library(dplyr)



library(dbplyr)
library(RPostgres)

# HTTP2 Chrome fix
httr::set_config(httr::config(http_version = 0))

source("secret.R")

ce_locations <- readr::read_csv("common_experiments.csv")
css_awesome_cols <- readr::read_csv("css_awesome_cols.csv")


server <- function(input, output, session) {

  
  choices <- c(
    "OpenStreetMap", "OpenStreetMap.BlackAndWhite", "OpenStreetMap.France",
    "OpenStreetMap.HOT", "Stamen.TonerLite", "Stamen.Terrain", 
    "Esri", "Esri.WorldTopoMap", "CartoDB", "HikeBike"
  )
  
  
  output$prov <- renderUI({
    radioButtons(
      "prov", "Base map",
      choices = choices, selected = "OpenStreetMap"
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
    select(code, year, affiliation, longitude, latitude, producer_id, protocols_enrolled) %>% 
    filter(!is.na(latitude)) %>% 
    filter(is.na(protocols_enrolled) | protocols_enrolled == "-999") %>% 
    collect() %>% 
    distinct(code, .keep_all = TRUE) %>% 
    filter(
      between(latitude, 24, 50),
      between(longitude, -125, -65)
    )
  
  
  output$affiliations <- renderUI({
    checkboxGroupInput(
      "affiliations", "Show teams:", inline = F,
      choices = sort(unique(all_sites$affiliation)),
      selected = sort(unique(all_sites$affiliation))
    )
  })  
  
  output$yrs <- renderUI({
    checkboxGroupInput(
      "yrs", "Show years:", inline = F,
      choices = sort(unique(all_sites$year)),
      selected = sort(unique(all_sites$year))
    )
  }) 
  

  
  sites
  ico <- reactive({
    awesomeIcons(
      icon = "unchecked", 
      #squareMarker = T, 
      iconColor = css_awesome_cols %>% 
        filter(color == input$col) %>% 
        pull(col),
      markerColor = input$col
      )
    })
  
  mobj <- reactive({
    if (is.null(input$prov)) return(NULL)
    
    leaflet(options = leafletOptions(attributionControl = F)) %>% 
      addProviderTiles(input$prov) %>% 
      addAwesomeMarkers(
        data = all_sites %>% 
          filter(
            affiliation %in% input$affiliations, 
            year %in% input$yrs
          ),
        lat = ~latitude, lng = ~longitude, icon = ico(),
        options = markerOptions(opacity = 0.75)
        ) %>% 
      addCircleMarkers(
        lat = ~latitude, lng = ~longitude,
        radius = ~ifelse(ce == "CE1", 10, 20),
        fill = NA,
        color = ~ifelse(ce == "CE1", "#03F", "#F30"),
        data = ce_locations,
        opacity = 1
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
      paste0(
        "PSA_map_", 
        format(Sys.time(), "%F %H%M%S"), 
        ".png"
        )
      },
    
    content = function(file) {
      mapshot(mobj(), file = file, vwidth = input$sz, vheight = input$sz)
    }
  )
  
}


