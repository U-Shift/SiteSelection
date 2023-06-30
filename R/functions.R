# functions all in one

select_city = function(CITY){
  CITY = CITY
}


get_citylimit = function(CITY) {
  MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg", quiet = TRUE) # Portugal
  CITYlimit = MUNICIPIOSgeo %>%
    filter(Concelho == CITY) %>% 
    st_collection_extract(type = "POLYGON") %>% # when the mixes lines with polygons
    sfheaders::sf_remove_holes(close = TRUE) # when it has holes in topology
  
  output_dir = file.path("database", CITY)
    if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  } else {
    print("Dir already exists!")
  }
  
  st_write(CITYlimit, paste0(output_dir, "/CITYlimit.geojson"), delete_dsn = TRUE)
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


get_osm = function(CITYlimit, CITY) {
  
  CITYlimit = st_read(paste0("database/", CITY, "/CITYlimit.geojson"), quiet = TRUE)
  BBOX = st_as_sfc(st_bbox(CITYlimit))
  
  # road_osm = st_read("database/geofabrik_portugal-latest.gpkg", quiet = TRUE) #old version with osmextract
      
  road_osm = opq(BBOX) %>% # uses osmdata package, to extract only with BB
    add_osm_feature(key = "highway") %>% 
    osmdata_sf() %>% 
    osm_poly2line() # makes roundabouts into lines
  road_osm = road_osm$osm_lines %>%
    select(osm_id, name, highway, geometry)
  
  road_network = road_osm %>%
    dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                                 'secondary',"secondary_link", "trunk", 'trunk_link',
                                 "tertiary", "tertiary_link", "pedestrian",
                                 "residential", "living_street", "unclassified", "service"))
  
  road_network = st_intersection(road_network, stplanr::geo_buffer(CITYlimit, dist=100)) 
  
  road_network$group = stplanr::rnet_group(road_network, d = 10) # 10m tolerance
  # plot(lisbon_network["group"])
  
  road_network_groups = road_network %>% filter(group == 1) #the network with more connected segments
  
  road_network = road_osm %>% filter(osm_id %in% road_network_groups$osm_id) # get only the segments from the clean network
  
  # st_geometry(road_network) # Should be "LINESTRING"
  # road_network = st_cast(road_network, "LINESTRING") #if you don't wnat to use the previous filter
  # road_network = stplanr::rnet_breakup_vertices(road_network) # break the segments internally, conserving the brunels.
  
  road_network = road_network %>% select(osm_id, highway, geometry) # keep some variables
  
  st_write(road_network, paste0("database/", CITY, "/road_network.gpkg"), delete_dsn = TRUE)
  
}


clean_osm = function(road_network, CITY) {
  
  options(qgisprocess.path="/usr/bin/qgis_process.bin")
  qgis_configure()
  
  # qgis_plugins() #não tem o disconnected islands
  
  # algorithms = qgis_algorithms()
  # algorithms %>% filter(grepl(pattern = "clean", x = algorithm, ignore.case = TRUE))
  # qgis_show_help("grass7:v.clean")
  
  input = road_network %>% 
    # mutate(fid_2 = as.integer(1:nrow(road_network))) %>% 
    st_write(paste0("database/", CITY, "/road_network.shp"), delete_dsn = TRUE)
  
  input = st_read(paste0("database/", CITY, "/road_network.shp")) #because of the fid column
  
  #delete exiting outputs
  if (file.exists(paste0("database/", CITY, "/road_network.shp"))){
    file.remove(paste0("database/", CITY, "/road_network.shp"))
  }
  
  output_path = paste0("database/", CITY, "/road_network_clean.shp")
  
  output = qgis_run_algorithm(
    algorithm = "grass7:v.clean",
    input = input, 
    type = c(0, 1, 2, 3, 4, 5, 6), 
    tool = c(0, 1, 2, 6, 8), #break, snap, rmdangle, rmdupl, bpol
    threshold = c("0", "0.00000100", "0.00000100", "0", "0"), 
    output = output_path, # need to be defined otherwise it saves in tmp.gpkg and makes the error with fid (# ERROR 1: failed to execute insert : UNIQUE constraint failed: outpute935bd152d284569afb314c88e8fce09.fid)
    error = qgis_tmp_vector(),
    GRASS_OUTPUT_TYPE_PARAMETER = "auto"
    # 'GRASS_REGION_PARAMETER':None, 
    # 'GRASS_SNAP_TOLERANCE_PARAMETER':-1, 
    # 'GRASS_MIN_AREA_PARAMETER':0.0001, 
    # 'GRASS_VECTOR_DSCO':'', 
    # 'GRASS_VECTOR_LCO':'', 
    # 'GRASS_VECTOR_EXPORT_NOCAT':False
  )
  
  road_network_clean = sf::st_read(output[["output"]][1])
  # %>% select(-fid_2)
  
  st_write(road_network_clean, output_path, delete_dsn = TRUE)
  
  # see trafficcalmr::osm_consolidate as an option!
  # https://saferactive.github.io/trafficalmr/reference/osm_consolidate.html
  # remotes::install_github("saferactive/trafficalmr")
  # road_network_clean_consolidate = road_network_clean %>% st_transform(3857) %>% trafficalmr::osm_consolidate(200)
  # osm_tags missing here, not working!
  
}


get_centrality = function(road_network_clean, CITY) {
  # road_network_clean = st_transform(road_network_clean, 3857)

  # qgis_show_help("grass7:v.net.centrality")
  
  if (file.exists(paste0("database/", CITY, "/centrality_nodes.gpkg"))){
    file.remove(paste0("database/", CITY, "/centrality_nodes.gpkg"))
  }
  
  # input = st_read(paste0("database/", CITY, "/road_network_clean.shp"))
  input = road_network_clean
  
  # remove previous results
  output_path = paste0("database/", CITY, "/centrality_nodes.gpkg")
  
  output_centrality = qgis_run_algorithm(
    algorithm = "grass7:v.net.centrality",
    input = input, 
    degree = "degree",
    closeness = "closeness",
    betweenness = "betweenness",
    '-a' = TRUE,
    # output = qgis_tmp_vector(),
    output = output_path, # in this case it cannot be shp otherwise we need to make variables names shorter than 10chr
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
  
  st_write(centrality_nodes, output_path, delete_dsn = TRUE)
  
}

get_centrality_grid = function(centrality_nodes, grid) {
  
  centrality_grid = 
    st_join(centrality_nodes,
            grid,
    # st_transform(grid, 3857),
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
  
  # saveRDS(grid_centrality, paste0("database/", CITY, "/centrality_grid".Rds"))
  
}


find_candidates = function(grid, centrality_grid, CITY) {
  
  candidates_centrality = left_join(st_transform(grid, 3857), centrality_grid) 
  # Filter in thresholds #for Lisbon. Adjust for other places?
  candidates_centrality = candidates_centrality %>%
    filter(degree >= summary(centrality_grid$degree)[[4]], #1088 média
           betweenness >= quantile(centrality_grid$betweenness, 0.40, na.rm = TRUE) &
             betweenness <= quantile(centrality_grid$betweenness, 0.60, na.rm = TRUE), 
           closeness >= 0.25 & closeness <= 0.75
           # closeness >= quantile(centrality_grid$closeness, 0.25, na.rm = TRUE) & closeness <= quantile(centrality_grid$closeness, 0.75, na.rm = TRUE) #2135 #too high
    )
  
  st_write(candidates_centrality, paste0("database/", CITY, "/candidates_centrality.gpkg"), delete_dsn = TRUE)

  # map_candidates = mapview::mapview(candidates_centrality)
  
}

