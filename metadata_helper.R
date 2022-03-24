library(dplyr)
ce_states <- list(
  CE1 = c("DE", "FL", "IL", "IN", "KS", "KY", "LA", "MD", 
          "NC", "NE", "NY", "PA", "SC", "VT", "WI"),
  CE2 = c("DE", "FL", "IL", "IA", "KS", "KY", "MD", 
          "NC", "NE", "NY", "OH", "PA", "TX", "VT", "WI")
) %>% 
    tibble::enframe("ce", "state") %>% 
    tidyr::unnest(state)

univs_raw <- httr::GET(
  "https://raw.githubusercontent.com/gboeing/data-visualization/main/ncaa-football-stadiums/data/stadiums-geocoded.csv"
)

univs_ll <- httr::content(univs_raw, as = "text") %>% 
  readr::read_csv() %>% 
  filter(state %in% ce_states$state ) %>% # misses DE, VT, NY
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
  ~city,        ~state, ~team,      ~latitude,         ~longitude,
  "Newark",     "DE",   "Delaware", 39.68591618717565, -75.7458982561899,
  "Ithaca",     "NY",   "Cornell",  42.44572342548429, -76.4832318739652,
  "Burlington", "VT",   "Vermont",  44.4783580236247, -73.19643151529881
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
  full_join(ce_states) %>% 
  ggplot() +
  borders("state") +
  geom_point(
    aes(x = longitude, y = latitude, size = ce),
    shape = 1
  ) +
  coord_map()


univs_ll %>%
  bind_rows(univs_addl) %>% 
  full_join(ce_states) %>% 
  select(city, state, team, latitude, longitude, ce) %>% 
  readr::write_csv("common_experiments.csv")

readr::read_csv("common_experiments.csv") %>% 
  leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addCircleMarkers(
    radius = ~ifelse(ce == "CE1", 5, 15),
    fill = NA,
    color = ~ifelse(ce == "CE1", "#03F", "#F30")
    )

readr::read_csv("common_experiments.csv") %>% 
  leaflet() %>% 
  addProviderTiles("OpenStreetMap") %>% 
  addAwesomeMarkers(
    icon = 
      awesomeIcons(spin = ~ifelse(ce == "CE1", 0, 180))
      
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
