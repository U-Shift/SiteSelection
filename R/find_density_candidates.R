#' find_density_candidates
#' 
#' @noRd
find_density_candidates = function(density_grid, population_min) {
  
  density_candidates = density_grid |>
    mutate(population_candidate =
             ifelse(population >= population_min(density_grid$population), 1, 0))
  
  return(density_candidates)
  
}