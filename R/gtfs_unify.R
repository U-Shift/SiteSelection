library(GTFSwizard)
library(gtfsrouter)

source("R/gtfs_download.R")

convert_seconds_to_hms <- function(seconds) {
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}

#' Download and merge several GTFS files
#' @param urls of GTFS files to download
#' @param outputName Name of the GTFS to generate, without extension
#' @param generateTransfers If true, generates transfers.txt
gtfs_unify <- function(urls, outputName, generateTransfers=TRUE) {
  
  # Download each GTFS and compile list of their locations on host 
  print(sprintf("1. Starting download of %d GTFS files...", length(urls)))
  locals <- list()
  counter <- 1
  for (url in urls) {
    locals <- append(locals, gtfs_download(url, sprintf("merging_area%d", counter)))
    counter <- counter + 1 
  }
  
  # Open them with GTFSWizard and merge them
  print(sprintf("2. Starting merge process..."))
  merged = NULL
  for (local in locals) {
    print(sprintf("> %s", local))
    gtfs <- GTFSwizard::read_gtfs(local)
    if (is.null(merged)) {
      merged <- gtfs
    } else {
      merged <- GTFSwizard::merge_gtfs(merged, gtfs) # https://r-packages.io/packages/GTFSwizard/merge_gtfs
    }
    print(sprintf("> Merged with %d routes, %d trips and %d stops, removing file...", length(merged$routes$route_id), length(merged$trips$trip_id), length(merged$stops$stop_id)))
    file.remove(local)
  }
  
  # Store merged GTFS and return its location
  destfile <- sprintf("database/transit/%s.zip", outputName)
  print(sprintf("3. Done! Storing result to %s...", destfile))
  GTFSwizard::write_gtfs(merged, destfile)
  
  # Generate transfers.txt
  if (generateTransfers) {
    print(sprintf("4. Generating transfers..."))
    merged_router <- gtfsrouter::extract_gtfs(destfile)
    # Use default parameters:
    # 200 meters distance, 120 sec time, routing through the road network (?)
    merged_router <- gtfsrouter::gtfs_transfer_table(merged_router)
    
    # gtfsrouter::extract_gtfs converts stop times to seconds, lets get it back to the format HH:mm before storing it...
    merged_router$stop_times$arrival_time <- sapply(merged_router$stop_times$arrival_time, convert_seconds_to_hms)
    merged_router$stop_times$departure_time <- sapply(merged_router$stop_times$departure_time, convert_seconds_to_hms)
    
    destfile <- sprintf("database/transit/%s_withTransfers.zip", outputName)
    print(sprintf("> Done! Storing result to %s...", destfile))
    GTFSwizard::write_gtfs(merged_router, destfile)
  }
  
  print(sprintf("FINISHED :)"))
  return(destfile)
}

# main()
request <- read.csv("database/gtfs/gtfs_sources.csv") |>
  subset(Ignore!=1)

gtfs_unify(request$URL, "GTFS_merged_Algarve")

