#' get_centrality_grid
#' 
#' @import sf
#' 
#' @noRd
get_centrality_grid = function(centrality_nodes, grid) {
  
  centrality_grid = grid |> 
    st_join(centrality_nodes, join = st_intersects) |>
    st_drop_geometry() |>
    group_by(ID) |>
    summarise(
      degree = mean(degree),
      betweenness = mean(betweenness),
      closeness = mean(closeness)
    ) |>
    ungroup() |> 
    mutate(
      degree = rescale(degree),
      betweenness = rescale(betweenness),
      closeness = rescale(closeness)
    )
  
}
