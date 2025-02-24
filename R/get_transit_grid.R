#' get_transit_grid
#' 
#' @import sf
#' 
#' @noRd
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


