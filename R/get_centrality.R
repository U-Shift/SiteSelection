#' get_centrality
#' 
#' @import sf
#' @import qgisprocess
#' 
#' @noRd
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
    
    centrality_nodes = sf::st_read(output_centrality[["output"]][1], quiet = TRUE) |> select(-eigenvector)
    
    st_write(centrality_nodes, output_path, delete_dsn = TRUE, quiet = TRUE)
    
  }
}

