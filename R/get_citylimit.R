#' get_citylimit
#' 
#' @import sf
#' @import sfheaders
#' 
#' @noRd
get_citylimit = function(CITY, GEOJSON, GEOJSON_name) {
  
  if(GEOJSON == TRUE){
    
    CITYlimit = st_read(paste0("inputdata/", GEOJSON_name, ".geojson"), quiet = TRUE)
    
    output_dir_gj = file.path("outputdata", GEOJSON_name)
    if (!dir.exists(output_dir_gj)) {
      dir.create(output_dir_gj)
    } else {
      print("Dir already exists!")
    }
    
    st_write(CITYlimit, paste0(output_dir_gj, "/CITYlimit.geojson"), delete_dsn = TRUE, quiet = TRUE)
    
    
  }else{
    
    
    if(file.exists(paste0("outputdata/", CITY, "/CITYlimit.geojson"))){
      
      CITYlimit = st_read(paste0("outputdata/", CITY, "/CITYlimit.geojson"), quiet = TRUE)
      
    } else {
      
      
      MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg", quiet = TRUE) # Portugal
      CITYlimit = MUNICIPIOSgeo |>
        filter(Concelho == CITY) |> 
        st_collection_extract(type = "POLYGON") |> # when the mixes lines with polygons
        sfheaders::sf_remove_holes(close = TRUE) # when it has holes in topology
      
      output_dir = file.path("outputdata", CITY)
      if (!dir.exists(output_dir)) {
        dir.create(output_dir)
      } else {
        print("Dir already exists!")
      }
      
      st_write(CITYlimit, paste0(output_dir, "/CITYlimit.geojson"), delete_dsn = TRUE, quiet = TRUE)
      
    }
  }
}


