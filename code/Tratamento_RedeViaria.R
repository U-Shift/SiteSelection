# Spatial network analysis - Paper 3
# Betweenness and closeness measures

# Install packages

remotes::install_github("ITSLeeds/osmextract")

# Import Libraries

library(osmextract)
library(sf)
library(tidyverse)
library(dplyr)
library(igraph)
library(mapview)
library(tidygraph)
library(dplyr)
library(tibble)
library(ggplot2)

# Update the stplanr

devtools::install_github("ropensci/stplanr")

library(stplanr)

# Set working directory
setwd("G:/O meu disco/Thesis/Artigo Site selection")

# Import open street map of Portugal

portugal_osm = oe_get("Portugal", provider = "geofabrik", stringsAsFactors = FALSE, quiet = FALSE,
                      force_download = TRUE, force_vectortranslate = TRUE)

# Take a look at how "highway" is classified in OSM. 
table(portugal_osm$highway)
table(portugal_osm$man_made)
# Filter the network

portugal_filtered = portugal_osm %>%
  filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",'secondary',"secondary_link", "trunk", 'trunk_link') | man_made %in% c("bridge")) 
#portugal_filtered = portugal_osm %>%
  #filter(highway %in% c('primary', "primary_link", 'secondary',"secondary_link", 'tertiary', "tertiary_link", "trunk", "trunk_link", "residential", "living_street", "unclassified"))

# Choose only Lisbon

## Import the shapefile of the "conselhos"
Concelhos = st_read("ConcelhosPT.gpkg")

## Take a look at the existing "concelhos"

Concelhos$Concelho  

# Choose Lisbon

#LisboaLimite = Concelhos %>% filter(Concelho == "LISBOA") 

amlLimite = Concelhos %>% filter(Concelho %in% c("LOURES", "LISBOA","ODIVELAS","AMADORA","OEIRAS","CASCAIS","SINTRA","MAFRA","VILA FRANCA DE XIRA","ALMADA", "SEIXAL", "SESIMBRA", "SET?BAL","PALMELA","MONTIJO","ALCOCHETE","MOITA","BARREIRO"))

# Cortar o limite de Lisboa com um buffer de 100m para as vias não ficarem cortadas

# Cortar nos bounding boxes, para não ficar tão pesado na operação seguinte
osm_lines_Concelho = st_crop(portugal_filtered, amlLimite) 

RedeOSM_aml = st_intersection(osm_lines_Concelho, geo_buffer(amlLimite, dist=100)) #clip com um buffer de 100m
st_write(RedeOSM_aml, "R and QGIS/RedeViariateste3_osm.shp")

# Network treatment

# Detect unconnected links
RedeOSM_aml$group =stplanr::rnet_group(RedeOSM_aml)

table(RedeOSM_aml$group)
plot(RedeOSM_aml["group"])

# Take out the unconnected links

RedeOSM_aml_clean = RedeOSM_aml %>% filter(group %in% c(1,2)) #o que tem mais segmentos

#Importar Rede vi?ria. 
st_write(RedeOSM_aml_clean, "R and QGIS/RedeViariateste_osm.shp")
mapview(RedeOSM_aml_clean) #verifify



# All the steps transform segments into MULTILINESTRING. We want to transform back to LINESTRING. 

st_geometry(RedeOSM_Loures_clean)
Redeviaria = portugal_filtered %>% filter(osm_id %in% RedeOSM_Loures_clean$osm_id) #ficar apenas os segmentos da rede limpa
st_geometry(Redeviaria) #verificar se são LINESTRING

# Solve intersections: 
# We do not use the v.clean of QGIS due to it breaks brunels (bridges and tunnels). 
# The function rnet_breakup_vertices breaks the segments internally, conserving the brunels.

nrow(Redeviaria)

Redeviaria = stplanr::rnet_breakup_vertices(Redeviaria)

nrow(Redeviaria)

#transformar em shapefile
st_write(Redeviaria, "R and QGIS/RedeViariaLoures_osm.shp")

# Take out columns that will not be used.

drop <- c("osm_id","name", "waterway", "aerialway", "barrier", "man_made", "z_order", "other_tags")
Redeviaria = Redeviaria[,!(names(Redeviaria) %in% drop)]

# Transform to projected geographical system
Redeviaria = st_transform(Redeviaria, 3857)

Redeviaria

# Give each edge a unique ID. The edges are the linestrings. 
edges <- Redeviaria %>%
  mutate(edgeID = c(1:n()))

##edges

# Create nodes at the start and end of each edge

nodes <- edges %>%
  st_coordinates() %>%
  as_tibble() %>%
  rename(edgeID = L1) %>%
  group_by(edgeID) %>%
  slice(c(1, n())) %>%
  ungroup() %>%
  mutate(start_end = rep(c('start', 'end'), times = n()/2))

# Denominate each node with an unique ID

nodes <- nodes %>%
  mutate(xy = paste(.$X, .$Y)) %>% 
  mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
  select(-xy)

##nodes

# Combine the node ID with the edge ID. 
#Therefore, we should specify for each edge, which node it starts and which node it ends.

source_nodes <- nodes %>%
  filter(start_end == 'start') %>%
  pull(nodeID)

target_nodes <- nodes %>%
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

nodes

# Convert Sticky Geometry (sf) and Linestring geometries into a tbl_graph

graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)

# Putting it all together for calculating betweenness and closeness

sf_to_tidygraph = function(x, directed = TRUE) {
  
  edges <- x %>%
    mutate(edgeID = c(1:n()))
  
  nodes <- edges %>%
    st_coordinates() %>%
    as_tibble() %>%
    rename(edgeID = L1) %>%
    group_by(edgeID) %>%
    slice(c(1, n())) %>%
    ungroup() %>%
    mutate(start_end = rep(c('start', 'end'), times = n()/2)) %>%
    mutate(xy = paste(.$X, .$Y)) %>% 
    mutate(nodeID = group_indices(., factor(xy, levels = unique(xy)))) %>%
    select(-xy)
  
  source_nodes <- nodes %>%
    filter(start_end == 'start') %>%
    pull(nodeID)
  
  target_nodes <- nodes %>%
    filter(start_end == 'end') %>%
    pull(nodeID)
  
  edges = edges %>%
    mutate(from = source_nodes, to = target_nodes)
  
  nodes <- nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = directed)
  
}

sf_to_tidygraph(Redeviaria, directed = FALSE)

graph <- graph %>%
  activate(edges) %>%
  mutate(length = st_length(geometry))

graph
# Calculate betweenness centrality. It calculates the number of shortest paths that pass through an edge.

graph <- graph %>%
  activate(nodes) %>%
  mutate(degree = centrality_degree()) %>%
  mutate(betweenness = centrality_betweenness(weights = length)) %>%
  activate(edges) %>%
  mutate(betweenness = centrality_edge_betweenness(weights = length))

graph

####################################################################################

# Transform in shapefiles
#edgesSF = graph %>%
  #activate(edges) %>%
  #as_tibble() %>%
  #st_as_sf()

#nodesSF = graph %>%
  #activate(nodes) %>%
  #as_tibble() %>%
  #st_as_sf()

#st_write(edgesSF, "edges_between_lisboa.shp")
#st_write(nodesSF, "nodes_between_lisboa.shp")






