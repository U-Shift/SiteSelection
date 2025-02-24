#' make_grid_all
#' 
#' @import tidygraph
#' @import dplyr
#' @import sf
#' 
#' @noRd
make_grid_all = function(grid, CITY, GEOJSON_name, GEOJSON, use_h3,
                         transit_candidates, landuse_candidates,
                         centrality_candidates, density_candidates
) {
  
  grid_all = grid |> 
    left_join(centrality_candidates |> st_drop_geometry(), by = "ID") |>
    left_join(density_candidates |> st_drop_geometry(), by = "ID") |> 
    left_join(transit_candidates |>
                # select(-frequency) |> TO-DO tidy this
                st_drop_geometry(), by = "ID") |>
    left_join(landuse_candidates |> st_drop_geometry(), by = "ID") 
  
  ## DEBUG  
  # st_write(grid_all, dsn = paste0("outputdata/", CITY, "/grid_all_DEBUG.gpkg"), delete_dsn = TRUE)
  
  # if there is no transit stops, all_candidate does not sum the transit_candidate
  # if (max(grid_all$transit_candidate, na.rm = TRUE) == 0){
  
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
  
  # add h3 info 
  if (use_h3 == TRUE){
    
    h3_index = readRDS(paste0("outputdata/", CITY, "/h3_index.Rds"))
    
    grid_all = grid_all |> 
      left_join(h3_index, by = "ID") # add hex codes
  }
  
  # for map legend purposes
  grid_all = grid_all |> 
    rowwise() |> # make sure the operator occurs on each row
    mutate(score = sum(degree_candidate, betweenness_candidate, closeness_candidate, 
                       population_candidate, entropy_candidate, transit_candidate, na.rm = TRUE)) |> 
    mutate(all_candidate = factor(all_candidate, levels = c(0,1), labels = c(0,1)),
           score = factor(score, levels = c(0:6), labels = c(0:6)))
  
  
  ## DEAL WITH TRANSIT BEFORE ##
  
  
  if (GEOJSON == TRUE){
    
    st_write(grid_all, dsn = paste0("outputdata/", GEOJSON_name, "/grid_all.gpkg"), delete_dsn = TRUE, quiet = TRUE)  
    
  }
  else {
    
    st_write(grid_all, dsn = paste0("outputdata/", CITY, "/grid_all.gpkg"), delete_dsn = TRUE, quiet = TRUE)  
    
  }
  
  
  return(grid_all)
  
}