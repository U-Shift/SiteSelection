#' @import sf
#' @import ggplot2
#' @import leaflet
#' @import lubridate
#' @import qgisprocess
#' @import readr
#' @import scales
#' @import stringr
#' @import targets
#' @import tibble
#' @import tidyr
#' @import dplyr
#' @import h3jsr
#' @importFrom stats frequency median quantile sd
#' @importFrom utils methods

# functions all in one


# select_city -------------------------------------------------------------

select_city = function(CITY, GEOJSON, GEOJSON_name){
  
  if (GEOJSON == FALSE) {
    
    CITY = CITY
    
  } else {
    
    CITY = GEOJSON_name
    
  }
  

}



# get_citylimit -----------------------------------------------------------

get_citylimit = function(CITY, GEOJSON, GEOJSON_name) {
  
  if(GEOJSON == TRUE){
    
    CITYlimit = st_read(paste0("inputdata/", GEOJSON_name, ".geojson"), quiet = TRUE)
    
    output_dir_gj = file.path("outputdata", GEOJSON_name)
    if (!dir.exists(output_dir_gj)) {
      dir.create(output_dir_gj)
    } else {
      print("Dir already exists!")
    }
    
    st_write(CITYlimit, paste0(output_dir_gj, "/CITYlimit.geojson"), delete_dsn = TRUE)
    
    
    }else{
      
  
    if(file.exists(paste0("outputdata/", CITY, "/CITYlimit.geojson"))){
      
      CITYlimit = st_read(paste0("outputdata/", CITY, "/CITYlimit.geojson"), quiet = TRUE)
      
    } else {
    
    
    MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg", quiet = TRUE) # Portugal
    CITYlimit = MUNICIPIOSgeo %>%
      filter(Concelho == CITY) %>% 
      st_collection_extract(type = "POLYGON") %>% # when the mixes lines with polygons
      sfheaders::sf_remove_holes(close = TRUE) # when it has holes in topology
    
    output_dir = file.path("outputdata", CITY)
      if (!dir.exists(output_dir)) {
      dir.create(output_dir)
    } else {
      print("Dir already exists!")
    }
    
    st_write(CITYlimit, paste0(output_dir, "/CITYlimit.geojson"), delete_dsn = TRUE)
   
    }
  }
}



# make_grid ---------------------------------------------------------------

make_grid = function(CITYlimit, CITY, cellsize_input, square_input, use_h3, h3_res)  {
  
  if (use_h3 == TRUE){
    
    grid = CITYlimit |>  
      polygon_to_cells(res = h3_res, simple = FALSE)  # res = 9 is 500m
    grid = grid$h3_addresses |>
      cell_to_polygon(simple = FALSE)
    # mutate(ID = seq(1:nrow(.))) # give an ID to each cell
    grid = grid |>
      mutate(ID = seq(1:nrow(grid)))  # give an ID to each cell
    h3_index = grid |> st_drop_geometry() # save h3_address for later
    grid = grid |>
      select(-h3_address)
    
    saveRDS(h3_index, paste0("outputdata/", CITY, "/h3_index.Rds"))
    
  } else {
  CITYlimit_meters = st_transform(CITYlimit, 3857) #projected
  # cellsize = c(200, 200) #200x200m

  grid = st_make_grid(CITYlimit_meters,
                      cellsize = cellsize_input, 
                      square = square_input 
  ) %>% 
    st_sf() %>% #convert sfc to sf %>% 
    st_join(CITYlimit_meters, left = FALSE) %>% 
    mutate(ID = seq(1:nrow(.))) %>% # give an ID to each cell
    select(ID, geometry) %>% 
    st_transform(st_crs(CITYlimit)) # go back to WGS48 if needed

  # mapgrid = mapview::mapview(grid, alpha.regions = 0.2)
  }
 
  st_write(grid, paste0("outputdata/", CITY, "/grid.geojson"), delete_dsn = TRUE)
  
}



# get_osm -----------------------------------------------------------------

get_osm = function(CITYlimit, CITY) {
  
 if(file.exists(paste0("outputdata/", CITY, "/road_network.gpkg"))){
    
    road_network = st_read(paste0("outputdata/", CITY, "/road_network.gpkg"), quiet = TRUE)
  
  }
    
 else {
  
   
  CITYlimit = st_read(paste0("outputdata/", CITY, "/CITYlimit.geojson"), quiet = TRUE)
   
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
  
 
  st_write(road_network, paste0("outputdata/", CITY, "/road_network.gpkg"), delete_dsn = TRUE)
  
  }
  
}



# clean_osm ---------------------------------------------------------------

clean_osm = function(road_network, CITY, build_osm) {
  
  if(build_osm == FALSE &
     file.exists(paste0("outputdata/", CITY, "/road_network_clean.shp"))){
    
    road_network_clean = sf::st_read(paste0("outputdata/", CITY, "/road_network_clean.shp"), quiet = TRUE)
    
  } else {
  
  options(qgisprocess.path="/usr/bin/qgis_process.bin")
  qgis_configure()
  
  # qgis_plugins() #não tem o disconnected islands
  
  # algorithms = qgis_algorithms()
  # algorithms %>% filter(grepl(pattern = "clean", x = algorithm, ignore.case = TRUE))
  # qgis_show_help("grass7:v.clean")
  
  input = road_network %>% 
    # mutate(fid_2 = as.integer(1:nrow(road_network))) %>% 
    st_write(paste0("outputdata/", CITY, "/road_network.shp"), delete_dsn = TRUE)
  
  input = st_read(paste0("outputdata/", CITY, "/road_network.shp"), quiet = TRUE) #because of the fid column
  
  #delete exiting outputs
  if (file.exists(paste0("outputdata/", CITY, "/road_network.shp"))){
    file.remove(paste0("outputdata/", CITY, "/road_network.shp"))
  }
  
  output_path = paste0("outputdata/", CITY, "/road_network_clean.shp")
  
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

  # cleaning the unnecessary nodes, using tidygraph and sfnetworks
  road_network_clean = as_sfnetwork(road_network_clean)
  
  road_network_clean = convert(road_network_clean, to_spatial_smooth) |> 
    activate(edges) |> 
    as_tibble() |>
    select(cat, osm_id, highway, geometry) |>
    mutate(edgeID = c(1:n())) |> 
    st_as_sf()
  
  st_write(road_network_clean, output_path, delete_dsn = TRUE)
  
  # see trafficcalmr::osm_consolidate as an option!
  # https://saferactive.github.io/trafficalmr/reference/osm_consolidate.html
  # remotes::install_github("saferactive/trafficalmr")
  # road_network_clean_consolidate = road_network_clean %>% st_transform(3857) %>% trafficalmr::osm_consolidate(200)
  # osm_tags missing here, not working!
 
  } 
}



# get_centrality ----------------------------------------------------------

get_centrality = function(road_network_clean, CITY) {
  # road_network_clean = st_transform(road_network_clean, 3857)

  
  if(file.exists(paste0("outputdata/", CITY, "/centrality_nodes.gpkg"))){
    
    centrality_nodes = sf::st_read(paste0("outputdata/", CITY, "/centrality_nodes.gpkg"), quiet = TRUE)
    
  } else {
  
    # if (file.exists(paste0("outputdata/", CITY, "/centrality_nodes.gpkg"))){
    # file.remove(paste0("outputdata/", CITY, "/centrality_nodes.gpkg"))
    # }
    
    
  # qgis_show_help("grass7:v.net.centrality")
  
  # input = st_read(paste0("outputdata/", CITY, "/road_network_clean.shp"))
  input = road_network_clean
  
  # remove previous results
  output_path = paste0("outputdata/", CITY, "/centrality_nodes.gpkg")
  
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
}



# get_centrality_grid -----------------------------------------------------

get_centrality_grid = function(centrality_nodes, grid) {
  
  centrality_grid = grid |> 
    st_join(centrality_nodes, join = st_intersects) %>%
    st_drop_geometry() %>%
    group_by(ID) %>%
    summarise(
      degree = mean(degree),
      betweenness = mean(betweenness),
      closeness = mean(closeness)
    ) %>%
    mutate(
      degree = rescale(degree),
      betweenness = rescale(betweenness),
      closeness = rescale(closeness)
    )
  
}



# get_census --------------------------------------------------------------

get_census = function(CITYlimit) {
  
  CENSUSpoint = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CENSUSpoint.gpkg", quiet = TRUE)
  CITYcensus = CENSUSpoint[CITYlimit,]

}


# density_grid ------------------------------------------------------------

get_density_grid = function(grid, CITYcensus) {
  
  # CITYcensus = readRDS(paste0("outputdata/", CITY, "/CITYcensus.Rds"))
  
  density_grid = 
    st_join(CITYcensus |> select(BGRI2021, N_INDIVIDUOS, geom),
            grid,
            join = st_intersects) %>% 
    st_drop_geometry() %>% 
    group_by(ID) %>% 
    summarise(population = sum(N_INDIVIDUOS)) |> 
    ungroup()
  
}


# get_landuse -------------------------------------------------------------

get_landuse = function(grid, CITYcensus) {
  
  # get OSM POIs with 6 categories
  points_poi = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/osm_poi_landuse.gpkg", quiet = TRUE)
  points_poi = points_poi[grid,] |>
    st_join(grid, join = st_intersects) |>
    st_drop_geometry() |> 
    group_by(ID, group) |>
    summarise(n = n()) |>
    ungroup()

  # get census buildings
  points_residential = CITYcensus |>
    select(BGRI2021, N_EDIFICIOS_EXCLUSIV_RESID, Concelho, geom) |> 
    rename(buildings = N_EDIFICIOS_EXCLUSIV_RESID)
  points_residential = points_residential[grid,] |>
    st_join(grid, join = st_intersects) |>
    st_drop_geometry() |>    
    group_by(ID) |>
    summarise(n = sum(buildings)) |>
    ungroup() |>
    mutate(group = "residential") |> 
    filter(n > 0)
  
  # join residential and other 6 categories
  categories = c("amenity", "healthcare", "leisure", "shop", "sport", "tourism", "residential")
  n_categories = length(categories)
  
  landuse_entropy = bind_rows(points_poi, points_residential) |> 
    group_by(ID) |> 
    summarise(entropy = -(sum((n/sum(n)) * log(n/sum(n))))/log(n_categories)) |>
    ungroup() |> 
    mutate(entropy = round(entropy, digits = 3)) |> 
    as.data.frame()
  
  landuse_grid = landuse_entropy
  
  return(landuse_grid)
  
  # saveRDS(landuse_entropy,"outputdata/test_landuse_entropy.Rds")

}
  

# get_transit --------------------------------------------------------------

get_transit = function(CITYlimit) {
  
  points_transit = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/bus_stop_freq.gpkg", quiet = TRUE)
  points_transit = points_transit[CITYlimit, ]
}


# get_transit_grid --------------------------------------------------------

get_transit_grid = function(grid, points_transit) {
  
  if (nrow(points_transit) == 0){ # empty - no bus stops
    
    transit_grid = grid |>
      st_drop_geometry() |>
      mutate(frequency = 0)
    
  } else {
    
  transit_grid = points_transit |>
    st_join(grid, join = st_intersects) |>
    st_drop_geometry() |>
    group_by(ID, hour) |>
    summarise(frequency = sum(frequency)) |>
    ungroup() |>
    group_by(ID) |>
    summarise(frequency = max(frequency)) |>
    ungroup()
  
  }

}

  # saveRDS(transit_grid, paste0("outputdata/", CITY, "/transit_grid.Rds"))
  


# classify_candidates ---------------------------------------------------------

# centrality
find_centrality_candidates = function(centrality_grid, degree_min, betweeness_range, closeness_range) {
  
  centrality_candidates = centrality_grid |> 
    mutate(degree_candidate = ifelse(degree >= degree_min(centrality_grid$degree, na.rm = TRUE), 1, 0),
           betweenness_candidate = ifelse(betweenness >= betweeness_range & betweenness <= 1-betweeness_range,
                                        1, 0),
           # betweenness_candidate = ifelse(betweenness >= quantile(betweenness, betweeness_range, na.rm = TRUE) &
           #                                  betweenness <= quantile(betweenness, 1-betweeness_range, na.rm = TRUE),
           #                                1, 0),
           closeness_candidate = ifelse(closeness >= closeness_range & closeness <= 1-closeness_range,
                                        1, 0)) |> 
    mutate(degree = round(degree, digits = 3),
           betweenness = round(betweenness, digits = 3),
           closeness = round(closeness, digits = 3)
     )
  
  return(centrality_candidates)
  
}


# density
find_density_candidates = function(density_grid, population_min) {
  
  density_candidates = density_grid |>
    mutate(population_candidate =
             ifelse(population >= population_min(density_grid$population), 1, 0))
  
  return(density_candidates)
  
}


# landuse
find_landuse_candidates = function(landuse_grid, entropy_min) {
  
  # landuse_entropy = readRDS("outputdata/test_landuse_entropy.Rds") # WHY does not work without this??
  landuse_candidates = landuse_grid |>
    mutate(entropy_candidate =
             ifelse(entropy >= entropy_min, 1, 0))
  
  return(landuse_candidates)
  
}

# transit
find_transit_candidates = function(transit_grid, freq_bus) {

  if (max(transit_grid$frequency, na.rm = TRUE) == 0){
    
    transit_candidates = transit_grid |> 
      mutate(transit = 0,
             transit_candidate = 0)
             
  } else {
    
    transit_candidates = transit_grid |> 
      mutate(transit = case_when(
      frequency <= freq_bus[1] ~ 1,
        frequency > freq_bus[1] & frequency <= freq_bus[2] ~ 2,
        frequency > freq_bus[2] & frequency <= freq_bus[3] ~ 3,
        frequency > freq_bus[3] ~ 4
     )) |> 
      mutate(transit_candidate = ifelse(transit %in% c(3,4), 1, 0))
  }
  
  return(transit_candidates)
  
}


# grid_all ----------------------------------------------------------------

make_grid_all = function(grid, CITY, GEOJSON, GEOJSON_name,
                         centrality_candidates, density_candidates,
                         transit_candidates, landuse_candidates,
                         use_h3) {
  
  grid_all = grid |> 
      left_join(centrality_candidates |> st_drop_geometry(), by = "ID") |>
      left_join(density_candidates |> st_drop_geometry(), by = "ID") |> 
      left_join(transit_candidates |>
                  # select(-frequency) |> TO-DO tidy this
                  st_drop_geometry(), by = "ID") |>
      left_join(landuse_candidates, by = "ID") 
    
  # if there is no transit stops, all_candidate does not sum the transit_candidate
  # if (max(grid_all$transit_candidate, na.rm = TRUE) == 0){
  
  if (use_h3 == TRUE){
    
    h3_index = readRDS(paste0("outputdata/", CITY, "/h3_index.Rds"))
    
    grid_all = grid_all |> 
      left_join(h3_index, by = "ID")
  }
    
    grid_all = grid_all |> 
    mutate(all_candidate = ifelse(degree_candidate == 1 & betweenness_candidate == 1 &
                                      closeness_candidate == 1 & population_candidate == 1 &
                                      entropy_candidate == 1, 1, 0)) |> 
    mutate(all_candidate = as.numeric(all_candidate)) |> 
    mutate(all_candidate = ifelse(is.na(all_candidate), 0, all_candidate)) 
    
  #   # if there is transit stops, all_candidate sums the transit_candidate
  # } else {
  #   
  #   grid_all = grid_all |> 
  #   mutate(all_candidate = ifelse(degree_candidate == 1 & betweenness_candidate == 1 &
  #                                   closeness_candidate == 1 & population_candidate == 1 &
  #                                   entropy_candidate == 1 & transit_candidate == 1,
  #                                 1, 0)) |> 
  #   mutate(all_candidate = as.numeric(all_candidate)) |> 
  #   mutate(all_candidate = ifelse(is.na(all_candidate), 0, all_candidate))
  #   
  # }
  
  # for map legend purposes
  grid_all = grid_all |> 
    rowwise() |> # make sure the operator occurs on each row
    mutate(score = sum(degree_candidate, betweenness_candidate, closeness_candidate, 
                       population_candidate, entropy_candidate, transit_candidate, na.rm = TRUE)) |> 
    mutate(all_candidate = factor(all_candidate, levels = c(0,1), labels = c(0,1)),
           score = factor(score, levels = c(0:6), labels = c(0:6)))
  
  
    ## DEAL WITH TRANSIT BEFORE ##
    
  
  if (GEOJSON == TRUE){
    
    st_write(grid_all, dsn = paste0("outputdata/", GEOJSON_name, "/grid_all.gpkg"), delete_dsn = TRUE)  
    
  }
  else {
    
    st_write(grid_all, dsn = paste0("outputdata/", CITY, "/grid_all.gpkg"), delete_dsn = TRUE)  
    
  }
  
  
    return(grid_all)

}


# filter_candidates -------------------------------------------------------

get_site_selection = function(grid_all, CITY, GEOJSON, GEOJSON_name) {
  

  grid_selection = grid_all |>
  dplyr::filter(all_candidate == 1) |>
    mutate(complexity = "complex")


  
  # transit
  
  if (max(grid_all$transit_candidate, na.rm = TRUE) == 0){
    
    print("No transit complexity !")
    
    grid_selection = grid_selection |>
      select(-transit, -frequency)
    
  } else {
    
    print("Including transit complexity")
    
    #classify complexity as "very complex" if transit is 3 or 4
    grid_selection = grid_selection |>
      mutate(complexity = ifelse(transit %in% c(3,4), "very complex", "complex"))
    
  }
 
  
  # tidy df
  
  grid_selection = grid_selection |>
    select(-transit_candidate, -degree_candidate, -betweenness_candidate,
           -closeness_candidate, -population_candidate, -entropy_candidate, -all_candidate)
  
  
  # export candidates
  
  if (GEOJSON == TRUE){
    
    st_write(grid_selection, dsn = paste0("outputdata/", GEOJSON_name, "/site_selection.gpkg"), delete_dsn = TRUE)  
    
  }
  else {
    
  st_write(grid_selection, dsn = paste0("outputdata/", CITY, "/site_selection.gpkg"), delete_dsn = TRUE)
  }
  
  return(grid_selection)
   
}


# export analysis and variable inputs ------------------------------------------------

export_analysis = function(grid_all, grid_selection, CITY_input, GEOJSON, GEOJSON_input, analysis,
                           cellsize_input, square_input, use_h3, h3_res, build_osm, degree_min, betweeness_range,
                           closeness_range, population_min, entropy_min, freq_bus) {
  
  if (analysis == TRUE){
    
    print("Exporting analysis...")
    
    analysis_row = data.frame(timestamp = Sys.time()) |> 
      mutate(CITY = ifelse(GEOJSON == FALSE, CITY_input, NA),
             GEOJSON_name = ifelse(GEOJSON == TRUE, GEOJSON_input, NA),
             h3_res = ifelse(use_h3 == TRUE, h3_res, NA),
             cellsize_a = ifelse(use_h3 == FALSE, cellsize_input[1], NA),
             cellsize_b = ifelse(use_h3 == FALSE, cellsize_input[2], NA),
             square = ifelse(use_h3 == FALSE, square_input, "hex h3"),
             build_osm = build_osm,
             degree_min = ifelse(methods(degree_min)[1] == "mean.Date", "mean", "median"),
             betweeness_range = betweeness_range,
             closeness_range = closeness_range,
             population_min = ifelse(methods(population_min)[1] == "mean.Date", "mean", "median"),
             entropy_min = entropy_min,
             freq_bus_1 = freq_bus[1],
             freq_bus_2 = freq_bus[2],
             freq_bus_3 = freq_bus[3]) |>
      mutate(n_candidates = nrow(grid_all),
             n_degree = sum(grid_all$degree_candidate, na.rm = TRUE),
             n_betweenness = sum(grid_all$betweenness_candidate, na.rm = TRUE),
             n_closeness = sum(grid_all$closeness_candidate, na.rm = TRUE),
             n_population = sum(grid_all$population_candidate, na.rm = TRUE),
             n_entropy = sum(grid_all$entropy_candidate, na.rm = TRUE),
             n_transit = sum(grid_all$transit_candidate, na.rm = TRUE),
             n_transit_3 = sum(grid_all$transit == 3, na.rm = TRUE),
             n_transit_4 = sum(grid_all$transit == 4, na.rm = TRUE),
             n_selected = nrow(grid_selection),
             n_complex = sum(grid_selection$complexity == "complex"),
             n_very_complex = sum(grid_selection$complexity == "very complex")) |> 
      mutate(mean_degree = mean(grid_all$degree, na.rm = TRUE),
             sd_degree = sd(grid_all$degree, na.rm = TRUE),
             min_degree = min(grid_all$degree, na.rm = TRUE),
             q1_degree = quantile(grid_all$degree, 0.25, na.rm = TRUE),
             median_degree = median(grid_all$degree, na.rm = TRUE),
             q3_degree = quantile(grid_all$degree, 0.75, na.rm = TRUE),
             max_degree = max(grid_all$degree, na.rm = TRUE)) |> 
      mutate(mean_betweenness = mean(grid_all$betweenness, na.rm = TRUE),
             sd_betweenness = sd(grid_all$betweenness, na.rm = TRUE),
             min_betweenness = min(grid_all$betweenness, na.rm = TRUE),
             q1_betweenness = quantile(grid_all$betweenness, 0.25, na.rm = TRUE),
             median_betweenness = median(grid_all$betweenness, na.rm = TRUE),
             q3_betweenness = quantile(grid_all$betweenness, 0.75, na.rm = TRUE),
             max_betweenness = max(grid_all$betweenness, na.rm = TRUE)) |>
      mutate(mean_closeness = mean(grid_all$closeness, na.rm = TRUE),
             sd_closeness = sd(grid_all$closeness, na.rm = TRUE),
             min_closeness = min(grid_all$closeness, na.rm = TRUE),
             q1_closeness = quantile(grid_all$closeness, 0.25, na.rm = TRUE),
             median_closeness = median(grid_all$closeness, na.rm = TRUE),
             q3_closeness = quantile(grid_all$closeness, 0.75, na.rm = TRUE),
             max_closeness = max(grid_all$closeness, na.rm = TRUE)) |>
      mutate(mean_population = mean(grid_all$population, na.rm = TRUE),
             sd_population = sd(grid_all$population, na.rm = TRUE),
             min_population = min(grid_all$population, na.rm = TRUE),
             q1_population = quantile(grid_all$population, 0.25, na.rm = TRUE),
             median_population = median(grid_all$population, na.rm = TRUE),
             q3_population = quantile(grid_all$population, 0.75, na.rm = TRUE),
             max_population = max(grid_all$population, na.rm = TRUE)) |>
      mutate(mean_entropy = mean(grid_all$entropy, na.rm = TRUE),
             sd_entropy = sd(grid_all$entropy, na.rm = TRUE),
             min_entropy = min(grid_all$entropy, na.rm = TRUE),
             q1_entropy = quantile(grid_all$entropy, 0.25, na.rm = TRUE),
             median_entropy = median(grid_all$entropy, na.rm = TRUE),
             q3_entropy = quantile(grid_all$entropy, 0.75, na.rm = TRUE),
             max_entropy = max(grid_all$entropy, na.rm = TRUE)) |>
      mutate(mean_frequency = mean(grid_all$frequency, na.rm = TRUE),
             sd_frequency = sd(grid_all$frequency, na.rm = TRUE),
             min_frequency = min(grid_all$frequency, na.rm = TRUE),
             q1_frequency = quantile(grid_all$frequency, 0.25, na.rm = TRUE),
             median_frequency = median(grid_all$frequency, na.rm = TRUE),
             q3_frequency = quantile(grid_all$frequency, 0.75, na.rm = TRUE),
             max_frequency = max(grid_all$frequency, na.rm = TRUE)) |>
      mutate(mean_score = mean(as.numeric(grid_all$score), na.rm = TRUE),
             sd_score = sd(as.numeric(grid_all$score), na.rm = TRUE),
             min_score = min(as.numeric(grid_all$score), na.rm = TRUE),
             q1_score = quantile(as.numeric(grid_all$score), 0.25, na.rm = TRUE),
             median_score = median(as.numeric(grid_all$score), na.rm = TRUE),
             q3_score = quantile(as.numeric(grid_all$score), 0.75, na.rm = TRUE),
             max_score = max(as.numeric(grid_all$score), na.rm = TRUE))
    
    if (!dir.exists("analysis")) {
      dir.create("analysis")}
    
    analysis_table = readRDS("analysis/analysis_table.Rds")  
    # analysis_table = analysis_row # first run
    analysis_table = bind_rows(analysis_table, analysis_row)
    saveRDS(analysis_table, "analysis/analysis_table.Rds")
    
    openxlsx::write.xlsx(analysis_table, file = "analysis/analysis_table.xlsx", overwrite = TRUE)
    
  }

}

