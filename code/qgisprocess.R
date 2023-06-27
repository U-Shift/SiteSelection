# qgisprocess

road_osm = st_read("database/geofabrik_portugal-latest.gpkg")
road_network = road_osm %>% 
  dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                               'secondary',"secondary_link", "trunk", 'trunk_link',
                               "tertiary", "tertiary_link", "pedestrian",
                               "residential", "living_street", "unclassified", "service"))
#remove: cycleway, pedestrian, service, 'motorway',"motorway_link"

# crop to city limits, with a buffer of 100m
road_network = st_intersection(road_network, geo_buffer(CITYlimit, dist=100)) 

road_network$group = stplanr::rnet_group(road_network, d = 10) # 10m tolerance
# plot(lisbon_network["group"])
road_network_groups = road_network %>% filter(group == 1) #the network with more connected segments

road_network = road_osm %>% filter(osm_id %in% road_network_groups$osm_id) # get only the segments from the clean network

# st_geometry(road_network) # Should be "LINESTRING"
# road_network = st_cast(road_network, "LINESTRING") #if you don't wnat to use the previous filter
# road_network = stplanr::rnet_breakup_vertices(road_network) # break the segments internally, conserving the brunels.

road_network = road_network %>% select(osm_id, highway, geometry) # keep some variables

st_write(road_network, "database/lisbon_network.gpkg", delete_dsn = TRUE)

options(qgisprocess.path="/usr/bin/qgis_process.bin")
library(qgisprocess)
qgis_configure()
# qgis_plugins() #não tem o disconnected islands

# algorithms = qgis_algorithms()
# algorithms %>% filter(grepl(pattern = "clean", x = algorithm, ignore.case = TRUE))

#delete exiting outputs
file.remove("database/road_network_clean.shp")

qgis_show_help("grass7:v.clean")

input = road_network %>% 
  mutate(fid_2 = as.integer(1:nrow(road_network))) %>% 
  st_write("database/road_network.shp", delete_dsn = TRUE)

input = st_read("database/road_network.shp") #because of the fid column

output = qgis_run_algorithm(
  algorithm = "grass7:v.clean",
  input = input, 
  type = c(0, 1, 2, 3, 4, 5, 6), 
  tool = c(0, 1, 2, 6, 8), #break, snap, rmdangle, rmdupl, bpol
  threshold = c("0", "0.00000100", "0.00000100", "0", "0"), 
  output = "database/road_network_clean.shp", # need to be defined otherwise it saves in tmp.gpkg and makes the error with fid (# ERROR 1: failed to execute insert : UNIQUE constraint failed: outpute935bd152d284569afb314c88e8fce09.fid)
  error = qgis_tmp_vector(),
  GRASS_OUTPUT_TYPE_PARAMETER = "auto"
  # 'GRASS_REGION_PARAMETER':None, 
  # 'GRASS_SNAP_TOLERANCE_PARAMETER':-1, 
  # 'GRASS_MIN_AREA_PARAMETER':0.0001, 
  # 'GRASS_VECTOR_DSCO':'', 
  # 'GRASS_VECTOR_LCO':'', 
  # 'GRASS_VECTOR_EXPORT_NOCAT':False
)

road_network_clean = sf::st_read(output[["output"]][1]) %>% select(-c(fid, fid_2))
st_write(road_network_clean, "database/road_network_clean.gpkg", delete_dsn = TRUE)

# see trafficcalmr::osm_consolidate as an option!
# https://saferactive.github.io/trafficalmr/reference/osm_consolidate.html
# remotes::install_github("saferactive/trafficalmr")
# road_network_clean_consolidate = road_network_clean %>% st_transform(3857) %>% trafficalmr::osm_consolidate(200)
# osm_tags missing here, not working!

qgis_show_help("grass7:v.net.centrality")

file.remove("database/centrality_nodes.gpkg")
input = st_read("database/road_network_clean.gpkg")

output_centrality = qgis_run_algorithm(
  algorithm = "grass7:v.net.centrality",
  input = input, 
  degree = "degree",
  closeness = "closeness",
  betweenness = "betweenness",
  '-a' = TRUE,
  # output = qgis_tmp_vector(),
  output = "database/centrality_nodes.gpkg", # in this case it cannot be shp otherwise we need to make variables names shorter than 10chr
  error = qgis_tmp_vector(),
  iterations = 1000,
  error= 0.1,
  '-g' = FALSE,
  GRASS_SNAP_TOLERANCE_PARAMETER = -1,
  GRASS_MIN_AREA_PARAMETER = 0.0001,
  GRASS_OUTPUT_TYPE_PARAMETER = 0,
  GRASS_VECTOR_EXPORT_NOCAT = FALSE
)

centrality_nodes = sf::st_read(output_centrality[["output"]][1]) %>% select(-eigenvector)

