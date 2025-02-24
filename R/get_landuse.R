#' get_landuse
#' 
#' @import sf
#' @import dplyr
#' 
#' @noRd
get_landuse = function(grid, CITYcensus) {
  
  options(dplyr.summarise.inform = FALSE) # suppress annoying warning
  
  # get OSM POIs with 6 categories
  points_poi = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/osm_poi_landuse.gpkg", quiet = TRUE)
  points_poi = points_poi[grid,] |>
    st_join(grid, join = st_intersects) |>
    st_drop_geometry() |> 
    group_by(ID, group) |>
    summarise(n = n()) |>
    ungroup()
  
  # get census buildings
  points_residential = CITYcensus |>
    select(BGRI2021, N_EDIFICIOS_EXCLUSIV_RESID, Concelho, geom) |> 
    rename(buildings = N_EDIFICIOS_EXCLUSIV_RESID)
  points_residential = points_residential[grid,] |>
    st_join(grid, join = st_intersects) |>
    st_drop_geometry() |>    
    group_by(ID) |>
    summarise(n = sum(buildings)) |>
    ungroup() |>
    mutate(group = "residential") |> 
    filter(n > 0)
  
  # join residential and other 6 categories
  categories = c("amenity", "healthcare", "leisure", "shop", "sport", "tourism", "residential")
  n_categories = length(categories)
  
  landuse_entropy = bind_rows(points_poi, points_residential) |> 
    group_by(ID) |> 
    summarise(entropy = -(sum((n/sum(n)) * log(n/sum(n))))/log(n_categories)) |>
    ungroup() |> 
    mutate(entropy = round(entropy, digits = 3)) |> 
    as.data.frame()
  
  landuse_grid = landuse_entropy
  
  return(landuse_grid)
  
  # saveRDS(landuse_entropy,"outputdata/test_landuse_entropy.Rds")
  
}
