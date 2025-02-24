#' find_centrality_candidates
#' 
#' @noRd
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
