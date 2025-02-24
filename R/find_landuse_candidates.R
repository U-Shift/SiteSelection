#' find_landuse_candidates
#' 
#' @import tidygraph
#' 
#' @noRd
find_landuse_candidates = function(landuse_grid, entropy_min) {
  
  # landuse_entropy = readRDS("outputdata/test_landuse_entropy.Rds") # WHY does not work without this??
  landuse_candidates = landuse_grid |>
    mutate(entropy_candidate =
             ifelse(entropy >= entropy_min, 1, 0))
  
  return(landuse_candidates)
  
}