#' get_transit
#' 
#' @import sf
#' 
#' @noRd
get_transit = function(CITYlimit) {
  
  points_transit = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/bus_stop_freq.gpkg", quiet = TRUE)
  points_transit = points_transit[CITYlimit, ]
}
