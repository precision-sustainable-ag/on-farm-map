library(leaflet)
library(mapview)
library(purrr)
library(dplyr)
library(httr)



library(dbplyr)
library(RPostgres)

# HTTP2 Chrome fix
httr::set_config(httr::config(http_version = 0))

source("secret.R")

program_locations <- readr::read_csv("programs.csv", show_col_types = F)
mask_world <- read_sf("background_mask.geojson")

abb <- tibble(name = state.name, abb = state.abb)
states <- sf::read_sf("us-states.geojson") %>% 
  left_join(abb) %>% 
  filter(abb != "AK")

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
  
  # output$col <- renderUI({
  #   radioButtons(
  #     "col", "Marker colors",
  #     choices = c(
  #       RColorBrewer::brewer.pal.info %>% 
  #         filter(category == "qual") %>% 
  #         rownames()
  #       ),
  #     selected = "Set1"
  #   )
  # })  
  
  cols <- reactive({
    scales::brewer_pal(palette = input$col)(length(input$exps)) %>% 
      set_names(input$exps)
  })
  
  # sites <- safely(tbl)(con, "site_information")
  # 
  # if (is.null(sites$result)) {
  #   con <- dbConnect(
  #     Postgres(),
  #     dbname = pg_dbname,
  #     host = pg_host,
  #     port = pg_port,
  #     user = pg_user,
  #     password = pg_password,
  #     sslmode = "require"
  #   )
  #   
  #   sites <- safely(tbl)(con, "site_information")
  # }
  
  sites_res <- purrr::safely(RETRY)(
    "GET",
    url = "https://api.precisionsustainableag.org/onfarm/locations",
    query = list(
      output = "json"
    ),
    add_headers(
      "x-api-key" = api_key
    )
  )
  
  all_sites <- content(sites_res$result, as = "text") %>% 
    jsonlite::fromJSON() %>% 
    filter(affiliation != "DV") %>% 
    filter(
      !is.na(lat), !is.na(lon),
      lat != -999, lon != -999
      ) %>% 
    filter(lat > 0, lat < 90, lon < 0, lon > -180) %>% 
    group_by(code, year, affiliation) %>% 
    summarise(
      lat = mean(lat, na.rm = T), 
      lon = mean(lon, na.rm = T)
      ) 

  # all_sites <- sites$result %>% 
  #   filter(affiliation != "DV") %>% 
  #   select(code, year, affiliation, longitude, latitude, producer_id, protocols_enrolled) %>% 
  #   filter(!is.na(latitude)) %>% 
  #   filter(is.na(protocols_enrolled) | protocols_enrolled == "-999") %>% 
  #   collect() %>% 
  #   distinct(code, .keep_all = TRUE) %>% 
  #   filter(
  #     between(latitude, 24, 55),
  #     between(longitude, -125, -65)
  #   )
  # 
  
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
  # ico <- reactive({
  #   awesomeIcons(
  #     icon = "unchecked", 
  #     #squareMarker = T, 
  #     iconColor = css_awesome_cols %>% 
  #       filter(color == input$col) %>% 
  #       pull(col),
  #     markerColor = input$col
  #     )
  #   })
  
  some_sites <- reactive({
    all_sites %>% 
    filter(
      affiliation %in% input$affiliations, 
      year %in% input$yrs
    ) %>% 
    st_as_sf(
      coords = c("lon", "lat"), #c("longitude", "latitude"), 
      remove = F,
      crs = 4326
    ) %>% 
    mutate(program = "onfarm") %>% 
    filter(program %in% input$exps)
  })
  
  some_programs <- reactive({
    program_locations %>% 
    filter(program %in% input$exps) %>% 
    st_as_sf(
      coords = c("longitude", "latitude"), 
      remove = F,
      crs = 4326
    )
  })
  
  
  mobj <- reactive({
    if (is.null(input$prov)) return(NULL)
    
    scl <- input$sz/1500

    mask_states <- st_filter(
      states, 
      st_union(some_sites()),
      .predicate = function(x, y) !quietly(st_intersects)(x, y)[["result"]]
    ) %>% 
      st_filter(
        st_union(some_programs()),
        .predicate = function(x, y) !quietly(st_intersects)(x, y)[["result"]]
      )
    
    leaflet(options = leafletOptions(attributionControl = F)) %>% 
      addProviderTiles(
        input$prov,
        options = providerTileOptions(opacity = input$background)
        ) %>% 
      addPolygons(
        data = mask_world,
        color = "#000000",
        opacity = 0.1,
        weight = 1*scl,
        fillOpacity = input$world_mask
      ) %>% 
      addPolygons(
        data = mask_states,
        color = "#000000",
        opacity = 0.0,
        fillOpacity = input$state_mask
      ) %>% 
      addCircleMarkers(
        data = some_sites(),
        opacity = 1,
        radius = 7.5*scl,
        color = unname(cols()["onfarm"])
      ) %>% 
      addCircleMarkers(
        lat = ~latitude + 
          scl*case_when(
            program == "Ed" ~ .15, 
            program == "CE1" ~ -.15, 
            program == "CE2" ~ 0
            ), 
        lng = ~longitude + 
          scl*case_when(
            program == "Ed" ~ -.15, 
            program == "CE1" ~ -.15, 
            program == "CE2" ~ 0.15
          ), 
        radius = 15*scl,
        fill = NA,
        color = ~case_when(
          program == "Ed" ~ unname(cols()["Ed"]),
          program == "CE1" ~ unname(cols()["CE1"]), 
          program == "CE2" ~ unname(cols()["CE2"])
          ),
        data = some_programs(),
        opacity = 1
      ) 
    
  })
  
  mobj_bounded <- reactive({
    input$reset
    
    bounds <- 
      bind_rows(
        some_sites(), 
        some_programs()
        ) %>% 
      st_bbox()
    
    mobj() %>% 
      fitBounds(
        bounds[["xmin"]]-0.5, bounds[["ymin"]]-0.5, 
        bounds[["xmax"]]+0.5, bounds[["ymax"]]+0.5
      )
  })
  
  output$map <- renderLeaflet(
    mobj_bounded()
  )

  output$map_r <- renderUI({
    leafletOutput(
      "map", 
      width = min(input$sz, 2000), 
      height = min(input$sz*0.7, 2000)
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
      mapshot(
        mobj_bounded(), file = file, 
        vwidth = input$sz, vheight = input$sz*0.7
        )
    }
  )
  
}


