#' get_site_selection
#' 
#' @import dplyr
#' @import tidygraph
#' @import sf
#' 
#' @noRd
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
    
    print("Including transit complexity !")
    
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
    
    st_write(grid_selection, dsn = paste0("outputdata/", GEOJSON_name, "/site_selection.gpkg"), delete_dsn = TRUE, quiet = TRUE)  
    
  }
  else {
    
    st_write(grid_selection, dsn = paste0("outputdata/", CITY, "/site_selection.gpkg"), delete_dsn = TRUE, quiet = TRUE)
  }
  
  return(grid_selection)
  
}