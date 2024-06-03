## traffic proxy using OSM hithway tags


library(tidyverse)
library(sf)
road_network_clean = sf::st_read("outputdata/map1/road_network_clean.shp")
grid = sf::st_read("outputdata/map1/grid.geojson")
congested_tags = c("motorway", "motorway_link", "trunk", "trunk_link",
                   "primary", "primary_link", "secondary", "secondary_link",
                   "tertiary", "tertiary_link") # 

traffic_vector_grid = st_intersection(road_network_clean, grid)


# classify if a cell presents congestion (heavy or severe) in more than 10% of total length
traffic_vector = traffic_vector_grid |>
  st_drop_geometry() |>
  filter(highway %in% congested_tags) |>
  distinct(ID)
  
traffic_cells = grid |> 
  mutate(classification = ifelse(ID %in% traffic_vector$ID, "congested", "free"))
# traffic_cells$classification = factor(traffic_cells$classification, levels = c("free", "congested")) 

# vizualise
road_network_clean = road_network_clean |> 
  mutate(congested_tags = ifelse(highway %in% congested_tags, "primary, secondary, tertiary", "other"))
road_network_clean$congested_tags = factor(road_network_clean$congested_tags, levels = c("primary, secondary, tertiary", "other")) # for viz purpouse only

mapview::mapview(traffic_cells, zcol="classification", alpha.regions = 0.2) +
  mapview::mapview(road_network_clean, zcol="congested_tags")
