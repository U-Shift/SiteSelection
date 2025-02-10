library(tidytransit, include.only = c("read_gtfs", "write_gtfs"))

source("R/gtfs_create_shapes.R")

#' Download GTFS file, fixing any irregularities. Returns zip location on local storage.
#' @param gtfs_url The url of the GTFS zip file
#' @param area String with area name
#' @param validateAndFix If true, GTFS file is validated and fixes applied if does not comply with standards
gtfs_download <- function(gtfs_url, area, validateAndFix=TRUE) {
  
  print(sprintf("Downloading GTFS file for %s, at %s...", area, gtfs_url))
        
  # DOWNLOAD GTFS and store it locally
  if (!dir.exists("database/transit")) {
    dir.create("database/transit", recursive = TRUE)
  }
  
  destfile <- sprintf("database/transit/%s_gtfs.zip", area)
  download.file(gtfs_url, destfile = destfile)
  print(sprintf("> GTFS downloaded and stored at %s!", destfile))
  
  # Validate if any fixes required
  if (validateAndFix) {
    
    gtfs <- tidytransit::read_gtfs(destfile)
    
    ## Clean empty stop_times arrival/departure (happened with Cascais GTFS) which raises an error at filter_feed_by_date method
    stopsNPrev <- length(gtfs$stop_times$trip_id)
    gtfs$stop_times <- gtfs$stop_times[!is.na(gtfs$stop_times$arrival_time), ] 
    stopsNAfter <- length(gtfs$stop_times$trip_id)
    if (stopsNPrev != stopsNAfter) {
      filtered_location = sprintf("%s/%s_stopTimesCleaned.zip", dirname(destfile), tools::file_path_sans_ext(basename(destfile)))
      print(sprintf("> !! FIXED GTFS, there were %d stop times without arrival time! Generated new GTFS at %s...", stopsNPrev-stopsNAfter, filtered_location))
      tidytransit::write_gtfs(gtfs, filtered_location)
      destfile <- filtered_location
    }
    
    ## If no shapes.txt, create them automatically with GTFSwizard
    if (!("shapes" %in% names(gtfs))) {
      print("> !! shapes.txt missing, using GTFSwizard to build it...") 
      destfile_new <- gtfs_create_shapes(destfile)
      print(sprintf("> !! CREATED shapes.txt. Generated new GTFS ZIP with it at %s, proceeding analysis...", destfile_new)) 
      gtfs <- tidytransit::read_gtfs(destfile_new)
      destfile <- destfile_new
    }
    
  }
  
  return(destfile)
}