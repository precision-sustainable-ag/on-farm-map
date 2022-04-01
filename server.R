library(leaflet)
library(leaflet.minicharts)
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
  left_join(abb, by = "name") %>% 
  filter(abb != "AK", name != "Puerto Rico")

invert_names <- function(x) {
  set_names(names(x), x)
}

server <- function(input, output, session) {


  
  cols <- reactive({
    if (input$col %in% LETTERS[1:4]) {
      x <- scales::viridis_pal(
        option = input$col, begin = .1, end = 0.9
        )(length(input$exps)) %>% 
        stringr::str_remove("FF$")
    } else {
      x <- scales::brewer_pal(palette = input$col)(length(input$exps))
    }

    if (input$shuffle %% 2 != 0) {
      x <- sample(x)
    }
    
    set_names(x, input$exps)
  })

  
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
    filter(affiliation != "DV", affiliation != "DM") %>% 
    filter(
      !is.na(lat), !is.na(lon),
      lat != -999, lon != -999
      ) %>% 
    filter(lat > 0, lat < 90, lon < 0, lon > -180) %>% 
    group_by(code, year, affiliation) %>% 
    summarise(
      lat = mean(lat, na.rm = T), 
      lon = mean(lon, na.rm = T),
      .groups = "drop"
      ) 


  
  output$affiliations <- renderUI({
    selectInput(
      "affiliations", 
      "Show teams (backspace to remove, type to add)", 
      choices = sort(unique(all_sites$affiliation)),
      selected = sort(unique(all_sites$affiliation)),
      multiple = T
    )
  })  
  
  output$yrs <- renderUI({
    selectInput(
      "yrs", "Show years",
      choices = sort(unique(all_sites$year)),
      selected = sort(unique(all_sites$year)),
      multiple = T
    )
  }) 
  

  

  
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
    mutate(program = "onfarm", onfarm = 1) %>% 
    filter(program %in% input$exps)
  })
  
  some_programs <- reactive({
    program_locations %>% 
    filter(program %in% input$exps) %>% 
      mutate(n = 1) %>% 
      tidyr::pivot_wider(
        names_from = "program",
        values_from = n
      ) %>% 
      mutate_all(~replace(., is.na(.), 0)) %>% 
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
    
    outline_states <- st_filter(
      states, 
      bind_rows(some_sites(), some_programs()) %>% st_union(),
      .predicate = function(x, y) quietly(st_intersects)(x, y)[["result"]]
    )
    

    m <- leaflet(
      options = leafletOptions(
        attributionControl = F#,
        # zoomDelta = 0.5,
        # dragging = T
        )
      ) %>% 
      addProviderTiles(
        input$prov,
        options = providerTileOptions(opacity = input$background)
        ) %>% 
      addPolygons(
        data = outline_states,
        color = "#111111",
        opacity = input$state_outline,
        fillOpacity = 0,
        weight = 2
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
      ) 
    
    if (nrow(some_sites())) {
      m <- m %>% 
      addCircleMarkers(
        data = some_sites(),
        opacity = 1,
        radius = 5*scl,
        weight = 2*scl,
        color = unname(cols()["onfarm"])
      )
      } 

    if (nrow(some_programs())) {
      m <- m %>% 

      addMinicharts(
        some_programs()$longitude, some_programs()$latitude,
        type = input$chart_type,
        chartdata = some_programs() %>% 
          select(any_of(input$exps)) %>% 
          st_drop_geometry(), 
        colorPalette = unname(cols()[names(cols()) != "onfarm"]),
        fillColor = unname(cols()[names(cols()) != "onfarm"])[1],
        legend = F,
        transitionTime = 250
      )    
      }      

    if (length(input$exps)) {
      m <- m %>% 
        addLegend(
        "bottomright",
        colors = cols(),
        labels = c(
          "On-farm" = "onfarm",
          "CE1" = "CE1",
          "CE2" = "CE2",
          "Education" = "Ed"
        ) %>% invert_names() %>% .[input$exps],
        opacity = 1
      )
    }
    
    m
      
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
  
  if (is.null(suppressMessages(webshot:::find_phantom()))) { 
    webshot::install_phantomjs() 
    }
  
  
  output$savebtn <- downloadHandler(
    filename = function() {
      paste0(
        "PSA_map_", 
        format(Sys.time(), "%F %H%M%S"), 
        ".png"
        )
      },
    
    content = function(file) {

      waiter::waiter_show(
        html = div(
          div(waiter::spin_rotating_plane()),
          "Rendering...", style = "transform: scale(4, 4)"
        ),
        color = "#FFA50055"
        )

      on.exit( waiter::waiter_hide() )
      
      mapshot(
        mobj_bounded(), file = file, 
        vwidth = input$sz, vheight = input$sz*0.7
        )

    }
  )
  
}


