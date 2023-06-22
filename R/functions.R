# functions all in one

get_citylimit = function(CITY) {
  MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg", quiet = TRUE) # Portugal
  CITYlimit = MUNICIPIOSgeo %>% filter(Concelho == CITY)
}

make_grid = function(CITYlimit)  {
  
  CITYlimit_meters = st_transform(CITYlimit, 3857) #projected
  cellsize = c(200, 200) #200x200m
  
  grid = st_make_grid(CITYlimit_meters,
                      cellsize = cellsize, 
                      square = TRUE #FALSE = hexagons
  ) %>% 
    st_sf() %>% #convert sfc to sf %>% 
    st_join(CITYlimit_meters, left = FALSE) %>% 
    mutate(ID = seq(1:nrow(.))) %>% # give an ID to each cell
    select(-c(1,2)) %>% 
    st_transform(st_crs(CITYlimit)) # go back to WGS48 if needed
  
  # mapgrid = mapview::mapview(grid, alpha.regions = 0.2)
  
}


get_osm = function(CITYlimit) {
  
  BBOX = st_as_sfc(st_bbox(CITYlimit))
  
  road_osm = st_read("database/geofabrik_portugal-latest.gpkg", quiet = TRUE)
  road_network = road_osm %>% 
    dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                                 'secondary',"secondary_link", "trunk", 'trunk_link',
                                 "tertiary", "tertiary_link", "pedestrian",
                                 "residential", "living_street", "unclassified", "service")
                  | man_made %in% c("bridge")) 
  road_network = st_intersection(road_network, geo_buffer(CITYlimit, dist=100)) 
  
  road_network$group = stplanr::rnet_group(road_network, d = 10) # 10m tolerance
  # plot(lisbon_network["group"])
  
  road_network_groups = road_network %>% filter(group == 1) #the network with more connected segments
  
  road_network = road_osm %>% filter(osm_id %in% road_network_groups$osm_id) # get only the segments from the clean network
  
  # st_geometry(road_network) # Should be "LINESTRING"
  # road_network = st_cast(road_network, "LINESTRING") #if you don't wnat to use the previous filter
  road_network = stplanr::rnet_breakup_vertices(road_network) # break the segments internally, conserving the brunels.
  
  road_network = road_network %>% select(osm_id, highway, geometry) # keep some variables
  
}

get_centrality = function(road_network) {
  road_network = st_transform(road_network, 3857)
  
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
  
  # This may be unecessary, already done in qgis?
  # Remove duplicate nodes. It may happen that more than one edge starts or finishes at a node. 
  nodes = nodes %>%
    distinct(nodeID, .keep_all = TRUE) %>%
    select(-c(edgeID, start_end)) %>%
    st_as_sf(coords = c('X', 'Y')) %>%
    st_set_crs(st_crs(edges))
  
  
  # Convert Sticky Geometry (sf) and Linestring geometries into a tbl_graph
  graph = tbl_graph(nodes = nodes, edges = as_tibble(edges), directed = FALSE)
  
  graph = graph %>%
    activate(edges) %>%
    mutate(length = st_length(geometry)) #no shp do gabriel tem outro nome
    # mutate(length = st_length(geom))
  
  # get values  - takes some time!
  graph = graph %>%
    activate(nodes) %>%
    mutate(degree = centrality_degree()) %>%
    mutate(betweenness = centrality_betweenness(weights = length)) %>% 
    mutate(closeness = centrality_closeness(weights = length))
  
  centrality_nodes = graph %>%
    activate(nodes) %>%
    as_tibble() %>%
    st_as_sf()
  
  
  st_write(centrality_nodes, "database/centrality_nodes_lisbon.gpkg", delete_dsn = TRUE)
  
}

get_centrality_grid = function(centrality_nodes, grid) {
  
  centrality_grid = 
    st_join(centrality_nodes,
    st_transform(grid, 3857),
    join = st_intersects) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>% 
    summarise(degree = mean(degree),
              betweenness = mean(betweenness),
              closeness = mean(closeness)) %>% 
    mutate(degree = rescale(degree),
           betweenness = rescale(betweenness),
           closeness = rescale(closeness)
    )
  
  # saveRDS(grid_centrality, "database/centrality_grid.Rds")
  
}


find_candidates = function(grid, centrality_grid) {
  
  candidates_centrality = left_join(st_transform(grid, 3857), centrality_grid) 
  # Filter in thresholds #for Lisbon. Adjust for other places?
  candidates_centrality = candidates_centrality %>%
    filter(degree >= summary(centrality_grid$degree)[[4]], #1088 média
           betweenness >= quantile(centrality_grid$betweenness, 0.40, na.rm = TRUE) & betweenness <= quantile(centrality_grid$betweenness, 0.60, na.rm = TRUE), #207 #too high
           # closeness >= quantile(centrality_grid$closeness, 0.10, na.rm = TRUE) & closeness <= quantile(centrality_grid$closeness, 0.7, na.rm = TRUE) #2135 #too high
    )

  # map_candidates = mapview::mapview(candidates_centrality)
  
}

