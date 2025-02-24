#' clean_osm
#' 
#' @import sf
#' @import qgisprocess
#' @import sfnetworks
#' @import tibble
#' 
#' @noRd
clean_osm = function(road_network, CITY, build_osm) {
  
  if(build_osm == FALSE &
     file.exists(paste0("outputdata/", CITY, "/road_network_clean.shp"))){
    
    road_network_clean = sf::st_read(paste0("outputdata/", CITY, "/road_network_clean.shp"), quiet = TRUE)
    
  } else {
    
    # options(qgisprocess.path="/usr/bin/qgis_process.bin") # if not defined
    
    # qgis_configure() # to enable plugguins. we only need to use Grass
    # qgis_plugins() #não tem o disconnected islands
    
    # algorithms = qgis_algorithms()
    # algorithms |> filter(grepl(pattern = "clean", x = algorithm, ignore.case = TRUE))
    # qgis_show_help("grass7:v.clean")
    
    input = road_network |> 
      # mutate(fid_2 = as.integer(1:nrow(road_network))) |> 
      st_write(paste0("outputdata/", CITY, "/road_network.shp"), delete_dsn = TRUE, quiet = TRUE)
    
    input = st_read(paste0("outputdata/", CITY, "/road_network.shp"), quiet = TRUE) #because of the fid column
    
    # # delete existing outputs
    # if (file.exists(paste0("outputdata/", CITY, "/road_network.shp"))){
    #   file.remove(paste0("outputdata/", CITY, "/road_network.shp"))
    # }
    
    output_path = paste0("outputdata/", CITY, "/road_network_clean.shp")
    
    output = qgis_run_algorithm(
      algorithm = "grass7:v.clean",
      input = input, 
      type = c(0, 1, 2, 3, 4, 5, 6), 
      tool = c(0, 1, 2, 6, 8), #break, snap, rmdangle, rmdupl, bpol
      threshold = c("0", "0.00000100", "0.00000100", "0", "0"), 
      output = output_path, # need to be defined otherwise it saves in tmp.gpkg and makes the error with fid (# ERROR 1: failed to execute insert : UNIQUE constraint failed: outpute935bd152d284569afb314c88e8fce09.fid)
      error = qgis_tmp_vector(),
      GRASS_OUTPUT_TYPE_PARAMETER = "auto",
      # 'GRASS_REGION_PARAMETER':None, 
      # 'GRASS_SNAP_TOLERANCE_PARAMETER':-1, 
      # 'GRASS_MIN_AREA_PARAMETER':0.0001, 
      # 'GRASS_VECTOR_DSCO':'', 
      # 'GRASS_VECTOR_LCO':'', 
      # 'GRASS_VECTOR_EXPORT_NOCAT':False
      .quiet = TRUE
    )
    
    road_network_clean = sf::st_read(output[["output"]][1], quiet = TRUE)
    # |> select(-fid_2)
    
    # cleaning the unnecessary nodes, using tidygraph and sfnetworks
    road_network_clean = as_sfnetwork(road_network_clean)
    
    
    road_network_clean = convert(road_network_clean, to_spatial_smooth) |> 
      activate(edges) |> 
      as_tibble() |>
      select(cat, osm_id, highway, geometry) |>
      mutate(edgeID = c(1:n())) |> 
      st_as_sf()
    
    st_write(road_network_clean, output_path, delete_dsn = TRUE, quiet = TRUE)
    
    # see trafficcalmr::osm_consolidate as an option!
    # https://saferactive.github.io/trafficalmr/reference/osm_consolidate.html
    # remotes::install_github("saferactive/trafficalmr")
    # road_network_clean_consolidate = road_network_clean |> st_transform(3857) |> trafficalmr::osm_consolidate(200)
    # osm_tags missing here, not working!
    
  } 
}


