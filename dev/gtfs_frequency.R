library(SiteSelection)
library(tidytransit)

request <- read.csv("inst/extdata/gtfs_sources_pt.csv") |>
  subset(ID == "metroLisboa")

output_file <- "database/transit/bus_stop_frequency_metro.gpkg"

aggregated_frequencies <- data.frame()

for (area in request$ID) {
  
  # DOWNLOAD GTFS and store it locally
  destfile = gtfs_download(request$URL[request$ID == area], sprintf("database/transit/%s_gtfs.zip", area))
  
  # Open GTFS with tidytransit library
  message(sprintf("> Openning it for processing (%s)...", destfile))
  gtfs <- tidytransit::read_gtfs(destfile)
  message(sprintf("> Openned GTFS for %s (ID %s)!", gtfs$agency$agency_name, gtfs$agency$agency_id))
  
  # Perform frequency analysis
  frequencies <- gtfs_frequency(gtfs, route_types=list(1))
  
  assign(sprintf("frequencies_%s", area), frequencies)
  
  aggregated_frequencies <- rbind(aggregated_frequencies, frequencies)
}

print(sprintf("Finished processing! Storing output to %s...", output_file))
st_write(aggregated_frequencies, output_file, append=FALSE) # append=FALSE for overwrite

print("Done! :)")
