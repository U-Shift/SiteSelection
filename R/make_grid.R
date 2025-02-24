#' make_grid
#' 
#' @import h3jsr
#' @import tibble
#' @import sf
#' @import tidygraph
#' 
#' @noRd
make_grid = function(CITYlimit, CITY, cellsize_input, square_input, use_h3, h3_res)  {
  
  if (use_h3 == TRUE){
    
    grid = CITYlimit |>  
      polygon_to_cells(res = h3_res, simple = FALSE)  # res = 9 is 500m
    grid = grid$h3_addresses |>
      cell_to_polygon(simple = FALSE) |> 
      rowid_to_column(var = "ID") # rowname as ID
    
    h3_index = grid |> st_drop_geometry() # save h3_address for later
    grid = grid |>
      select(-h3_address)
    
    saveRDS(h3_index, paste0("outputdata/", CITY, "/h3_index.Rds"))
    
  } else {
    CITYlimit_meters = st_transform(CITYlimit, 3857) #projected
    # cellsize = c(200, 200) #200x200m
    
    grid = st_make_grid(CITYlimit_meters,
                        cellsize = cellsize_input,
                        square = square_input) |>
      st_sf() |> #convert sfc to sf |>
      st_join(CITYlimit_meters, left = FALSE) |> 
      rowid_to_column(var = "ID") |> 
      select(ID) |> # and geometry also comes
      st_transform(st_crs(CITYlimit)) # go back to WGS48 if needed
    
    # mapgrid = mapview::mapview(grid, alpha.regions = 0.2)
  }
  
  st_write(grid, paste0("outputdata/", CITY, "/grid.geojson"), delete_dsn = TRUE, quiet = TRUE)
  
}