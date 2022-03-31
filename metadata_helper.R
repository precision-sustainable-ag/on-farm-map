library(dplyr)
# https://docs.google.com/spreadsheets/d/1rp_KDgETOa8-F1dM3BzTc9JEt98OVYFDdFN_jprjmFU/edit#gid=854608412
program_states <- list(
  CE1 = c("DE", "FL", "IL", "IN", "KS", "KY", "LA", "MD", 
          "NC", "NE", "NY", "PA", "SC", "VT", "WI"),
  CE2 = c("DE", "FL", "IL", "IA", "KS", "KY", "MD", 
          "NC", "NE", "NY", "OH", "PA", "TX", "VT", "WI"),
  Ed =  c("KY", "MD", "MI", "NE", "NH", "NY", "SC")
) %>% 
  tibble::enframe("program", "state") %>% 
  tidyr::unnest(state) #%>% 
  #mutate(program = stringr::str_remove_all(program, "[0-9]")) %>% 
  #distinct(program, state)

univs_raw <- httr::GET(
  "https://raw.githubusercontent.com/gboeing/data-visualization/main/ncaa-football-stadiums/data/stadiums-geocoded.csv"
)

univs_ll <- httr::content(univs_raw, as = "text") %>% 
  readr::read_csv() %>% 
  filter(state %in% program_states$state ) %>% # misses DE, VT, NY, NH
  filter(
    team %in% c(
      "Florida",
      "Iowa State",
      "Illinois",
      "Purdue",
      "Kansas State",
      "Kentucky",
      "LSU",
      "Maryland",
      "Michigan State",
      "NC State",
      "Nebraska",
      "Ohio State",
      "Penn State",
      "Clemson",
      "Texas A&M",
      "Wisconsin"
    )
  )

univs_addl <- tribble(
  ~city,        ~state, ~team,           ~latitude,         ~longitude,
  "Newark",     "DE",   "Delaware",      39.68591618717565, -75.7458982561899,
  "Durham",     "NH",   "New Hampshire", 43.13907326077061, -70.93483651750785,
  "Ithaca",     "NY",   "Cornell",       42.44572342548429, -76.4832318739652,
  "Burlington", "VT",   "Vermont",       44.4783580236247, -73.19643151529881
)

library(leaflet)
library(ggplot2)
univs_ll %>%
  bind_rows(univs_addl) %>% 
  left_join(
    as_tibble(state.center) %>% 
      mutate(state = state.abb)
  ) %>% 
  ggplot() +
  borders("state") +
  geom_segment(
    aes(x = longitude, y = latitude, xend = x, yend = y)
  ) +
  coord_map()


univs_ll %>%
  bind_rows(univs_addl) %>% 
  full_join(program_states) %>% 
  ggplot() +
  borders("state") +
  geom_point(
    aes(x = longitude, y = latitude, size = program),
    shape = 1
  ) +
  coord_map()


univs_ll %>%
  bind_rows(univs_addl) %>% 
  full_join(program_states) %>% 
  select(city, state, team, latitude, longitude, program) %>% 
  # st_as_sf(coords = c("longitude", "latitude"), remove = F) %>% 
  readr::write_csv("programs.csv")

readr::read_csv("programs.csv") %>% 
  leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addCircleMarkers(
    radius = ~ifelse(program == "CE1", 5, 15),
    fill = NA,
    color = ~ifelse(program == "CE1", "#03F", "#F30")
    )

readr::read_csv("programs.csv") %>% 
  leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addAwesomeMarkers(
    icon = 
      awesomeIcons(spin = ~ifelse(program == "CE1", 0, 180))
      
  )



library(magick)
css_pos <- readr::read_lines(
  "https://raw.githubusercontent.com/sigma-geosistemas/Leaflet.awesome-markers/master/dist/leaflet.awesome-markers.css"
  ) %>% 
  stringr::str_c(collapse = " ") %>% 
  stringr::str_extract_all(
    "\\.awesome-marker-icon-[a-z]+ \\{   background-position: [-0-9px]+ 0; \\}",
    simplify = T
    ) %>% 
  stringr::str_extract_all("-[a-z]+ |[-0-9px]+ 0", simplify = T) %>% 
  as_tibble() %>% 
  mutate(
    color = stringr::str_extract(V1, "[a-z]+"),
    xpos = stringr::str_extract(V2, "^[-0-9]+") %>% as.numeric()
  )

css_awesome_cols <- css_pos %>% 
  mutate(
    col = purrr::map_chr(
      xpos,
      ~image_read("/Users/baits/Downloads/markers-soft.png") %>% 
      image_crop(geometry = geometry_area(1, 1, -.x+18, 15)) %>% 
      image_data() %>% 
      paste(collapse = "")
    ),
    col = paste0("#", col)
    ) 

readr::write_csv(css_awesome_cols, "css_awesome_cols.csv")


css_awesome_cols %>% 
  mutate(
    hcl = farver::decode_colour(col, to = "hcl"),
    h = hcl[,1],
    c = hcl[,2],
    l = hcl[,3]
    ) %>% 
  mutate(
    pal = purrr::pmap(
      list(h,c,l),
      ~scales::hue_pal(h = ..1 + c(0,360), c = ..2, l = ..3)(3)
    )
  ) %>% 
  select(color, pal) %>% 
  tidyr::unnest(pal) %>% 
  pull(pal) %>% 
  scales::show_col(ncol = 9)


# background mask and state outlines ----


library(sf)

download.file(
  "https://rstudio.github.io/leaflet/json/us-states.geojson",
  "us-states.geojson"
)

abb <- tibble(name = state.name, abb = state.abb)

states <- sf::read_sf(
  "https://rstudio.github.io/leaflet/json/us-states.geojson"
) %>% 
  left_join(abb)

sf_use_s2(F)
background_everything <- 
  st_point(c(-85, 39)) %>% 
  st_sfc() %>% 
  st_set_crs('WGS84') %>% 
  st_buffer(45) 



st_difference(
  background_everything, 
  st_union(st_make_valid(states))
  ) %>% 
  write_sf("background_mask.geojson")

background_mask <- read_sf("background_mask.geojson")
background_mask %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()
  


leaflet(background_everything) %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addPolygons() %>% 
  addPolygons(
    data = states %>% 
      filter(abb %in% c("NC", "MD"))
    )


leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addPolygons(
    data = background_mask,
    color = "#000000",
    opacity = 0.1,
    weight = 1,
    fillOpacity = 0.15
  ) %>% 
  addPolygons(
    data = states %>% 
      filter(!(abb %in% program_locations$state)),
    color = "#000000",
    opacity = 0.0,
    fillOpacity = 0.2
  ) %>% 
  addMarkers(data = program_locations) %>% 
  fitBounds(
    program_locations$longitude %>% min(),
    program_locations$latitude %>% min(),
    program_locations$longitude %>% max(),
    program_locations$latitude %>% max()
  )


active_states <- st_filter(
  states %>% filter(abb != "AK"), 
  program_locations %>% 
    filter(state %in% c("FL", "TX")) %>% 
    st_as_sf(
      coords = c("longitude", "latitude"), 
      remove = F,
      crs = 4326
      ) %>% 
    st_union(),
  .predicate = function(x, y) !quietly(st_intersects)(x, y)[["result"]]
  )
ggplot(active_states) + geom_sf()

program_locations %>% 
  filter(state %in% c("FL", "TX")) %>% 
  st_as_sf(
    coords = c("longitude", "latitude"), 
    remove = F,
    crs = 4326
  ) %>% 
  bind_rows(
    program_locations %>% 
      filter(state %in% c("NY")) %>% 
      st_as_sf(
        coords = c("longitude", "latitude"), 
        remove = F,
        crs = 4326
      )
  ) %>% 
  st_bbox()
