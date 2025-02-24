#' get_density_grid
#' 
#' @import sf
#' 
#' @noRd
get_density_grid = function(grid, CITYcensus) {
  
  # CITYcensus = readRDS(paste0("outputdata/", CITY, "/CITYcensus.Rds"))
  
  density_grid = 
    st_join(CITYcensus |> select(BGRI2021, N_INDIVIDUOS, geom),
            grid,
            join = st_intersects) |> 
    st_drop_geometry() |> 
    group_by(ID) |> 
    summarise(population = sum(N_INDIVIDUOS)) |> 
    ungroup()
  
  # ## DEBUG
  # saveRDS(density_grid, paste0("outputdata/", "Chaves", "/density_grid.Rds"))
  
}
