# aim: get road network centrality measures and attach them to grid layer

library(sf)
library(dplyr)
library(ggplot2)
library(sfnetworks)
library(tidygraph)
library(scales)
road_network = st_read("database/lisbon_network.gpkg") #or https://github.com/U-Shift/SiteSelection/releases/download/0.1/lisbon_network.gpkg
road_network = st_transform(road_network, 3857) # Project


# create a road network graph ------------------------------------------------------------

edges = road_network %>%
  mutate(edgeID = c(1:n()))

nodes = edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))


# Denominate each node with an unique ID
nodes = nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy) # ignore warnings!

# Combine the node ID with the edge ID. 
source_nodes = nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes = nodes %>%
  filter(start_end == 'end') %>%
  pull(nodeID)

edges = edges %>%
  mutate(from = source_nodes, to = target_nodes)


# Remove duplicate nodes. It may happen that more than one edge starts or finishes at a node. 
nodes <- nodes %>%
  distinct(nodeID, .keep_all = TRUE) %>%
  select(-c(edgeID, start_end)) %>%
  st_as_sf(coords = c('X', 'Y')) %>%
  st_set_crs(st_crs(edges))


# Convert Sticky Geometry (sf) and Linestring geometries into a tbl_graph
graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

graph = graph %>%
  activate(edges) %>%
  mutate(length = st_length(geom))



# closeness, betweenness and centrality degree ------------------------------

# only needed for the nodes
graph = graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>% 
  mutate(closeness = centrality_closeness(weights = length)) #not as expected

# check if makes sence -------------------------------------------------------

# Plot betweenness of nodes
ggplot() +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = betweenness, size = betweenness)) + 
  scale_colour_viridis_c(option = 'inferno') + scale_size_continuous(range = c(0,4)) + theme_classic()

# Plot closeness of nodes - sounds BAD
ggplot() +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = closeness, size = closeness)) + 
  scale_colour_viridis_c(option = 'inferno') + scale_size_continuous(range = c(0,4)) + theme_classic()

# Plot centrality degree of nodes
ggplot() +
  geom_sf(data = graph %>% activate(nodes) %>% as_tibble() %>% st_as_sf(), aes(col = degree, size = degree)) + 
  scale_colour_viridis_c(option = 'inferno') + scale_size_continuous(range = c(0,4)) + theme_classic()


# export as sf ------------------------------------------------------------

centrality_nodes = graph %>%
  activate(nodes) %>%
  as_tibble() %>%
  st_as_sf()

st_write(centrality_nodes, "database/centrality_nodes_lisbon.gpkg", delete_dsn = TRUE)
rm(nodes, edges)

# match with grid ---------------------------------------------------------

centrality_grid = st_join(centrality_nodes, st_transform(GRID, 3857), join = st_intersects) %>% 
  mutate(betweenness = rescale(betweenness),
         closeness = rescale(closeness),
         degree = rescale(degree))

summary(centrality_grid$degree)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.2000  0.4000  0.3212  0.4000  1.0000 

summary(centrality_grid$betweenness)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.000000 0.001445 0.010110 0.056437 0.051371 1.000000  

summary(centrality_grid$closeness) #não parece estar bem!
#   Min.  1st Qu.   Median     Mean    3rd Qu.     Max.     NA's 
# 0.000000 0.000000 0.000000 0.000634 0.000000 1.000000       27

centrality_grid = centrality_grid %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarise(degree = mean(degree),
            betweenness = mean(betweenness),
            closeness = mean(closeness))
