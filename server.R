library(leaflet)
library(mapview)
library(purrr)
library(dplyr)



library(dbplyr)
library(RPostgres)

# HTTP2 Chrome fix
httr::set_config(httr::config(http_version = 0))

source("secret.R")

program_locations <- readr::read_csv("programs.csv")
#css_awesome_cols <- readr::read_csv("css_awesome_cols.csv")


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
      "col", "Marker colors",
      choices = c(
        RColorBrewer::brewer.pal.info %>% filter(category == "qual") %>% rownames()
        ),
      selected = "Set1"
    )
  })  
  
  cols <- reactive({
    scales::brewer_pal(palette = input$col)(4)
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
  

  
  #sites
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
      addProviderTiles(
        input$prov,
        options = providerTileOptions(opacity = 0.9)
        ) %>% 
      addCircleMarkers(
        data = all_sites %>% 
          filter(
            affiliation %in% input$affiliations, 
            year %in% input$yrs
          ),
        lat = ~latitude, lng = ~longitude,
        opacity = 1,
        radius = 7.5,
        color = cols()[1] #  "#4daf4a"
      ) %>% 
      addCircleMarkers(
        lat = ~latitude + 
          case_when(
            program == "Ed" ~ .15, 
            program == "CE1" ~ -.15, 
            program == "CE2" ~ 0
            ), 
        lng = ~longitude + 
          case_when(
            program == "Ed" ~ -.15, 
            program == "CE1" ~ -.15, 
            program == "CE2" ~ 0.15
          ), 
        radius = 15,
        fill = NA,
        color = ~case_when(
          program == "Ed" ~ cols()[2],
          program == "CE1" ~ cols()[3], 
          program == "CE2" ~ cols()[4]
          ),
        data = program_locations,
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


