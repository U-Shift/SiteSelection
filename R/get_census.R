#' get_census
#' 
#' @import sf
#' 
#' @noRd
get_census = function(CITYlimit) {
  
  CENSUSpoint = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CENSUSpoint.gpkg", quiet = TRUE)
  CITYcensus = CENSUSpoint[CITYlimit,]
  
}