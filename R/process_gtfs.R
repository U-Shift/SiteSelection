# imports 
library(sf)
library(tidyverse)
library(lubridate)
library(tidytransit)

source("R/gtfs_create_shapes.R")

# methods

#' Process GTFS file
#' @param gtfs_url The url of the GTFS zip file
#' @param city String with zity name
#' @param date Reference date to consider when analysing the GTFS file
process_gtfs <- function(gtfs_url, area, date) {
  print(sprintf("Analysing GTFS for %s...", area))

  # Download GTFS and store it locally
  if (!dir.exists("database/transit")) {
    dir.create("database/transit", recursive = TRUE)
  }
  print(sprintf("> Downloading GTFS file..."))
  destfile <- sprintf("database/transit/%s_gtfs.zip", area)
  download.file(gtfs_url, destfile = destfile)
  print(sprintf("> GTFS downloaded and stored at %s!", destfile))

  # Open GTFS with tidytransit library and filter by date
  print("> Openning it for processing...")
  gtfs <- tidytransit::read_gtfs(destfile)
  print(sprintf("> Openned GTFS for %s (ID %s)!", gtfs$agency$agency_name, gtfs$agency$agency_id))
  
  # Fix GTFS 
  ## If no shapes.txt, create them automatically with GTFSwizard
  if (!("shapes" %in% names(gtfs))) {
    print("> !! shapes.txt missing, using GTFSwizard to build it...") 
    destfile_new <- gtfs_create_shapes(destfile)
    print(sprintf("> !! shapes.txt created, created new GTFS ZIP with it at %s, proceeding analysis...", destfile_new)) 
    gtfs <- tidytransit::read_gtfs(destfile_new)
  }
  
  ## Clean empty stop_times arrival/departure (happened with Cascais GTFS) which raises an error at filter_feed_by_date method
  stopsNPrev <- length(gtfs$stop_times$trip_id)
  gtfs$stop_times <- gtfs$stop_times[!is.na(gtfs$stop_times$arrival_time), ] 
  stopsNAfter <- length(gtfs$stop_times$trip_id)
  if (stopsNPrev != stopsNAfter) {print(sprintf("> !! FIXED GTFS, there were %d stop times without arrival time!", stopsNPrev-stopsNAfter))}
  
  print(sprintf("> Analysing reference date %s...", date))
  gtfs_date <- filter_feed_by_date(
    gtfs, extract_date = date
  )
  print(sprintf("> There are %d routes operating %d trips on %d stops...", 
    length(gtfs_date$trips$trip_id),
    length(gtfs_date$routes$route_id),
    length(gtfs_date$stops$stop_id)
  ))

  # Organize the table calculating the frequencies per bus stop

  ## Service pattern

  ### Building meta data on the service patterns 
  ### https://cran.r-project.org/web/packages/tidytransit/vignettes/servicepatterns.html
  ### Alternative docs: https://cran.r-project.org/web/packages/tidytransit/tidytransit.pdf, page 20
  ### Creates $.$servicepatterns with unique id per pattern
  ### Creates $.$dates_servicepatterns matching each individual date covered by the GTFS with the corresponding id
  pattern_gtfs <- set_servicepattern(gtfs_date)
  print(sprintf("> Identified %d service patterns matching date: %s", length(pattern_gtfs$.$servicepatterns$servicepattern_id), paste(pattern_gtfs$.$servicepatterns$service_id, collapse=", ")))
  ### WARNING: every time we run this, random ids will be generated for the service patterns

  ## Convert stops and shapes to simple features
  pattern_gtfs <- gtfs_as_sf(pattern_gtfs)
  pattern_gtfs$shapes$length <- st_length(pattern_gtfs$shapes) # Compute length for each shape

  shape_lengths <- pattern_gtfs$shapes |> 
    as.data.frame() |>
    select(shape_id, length, -geometry)

  
  ## Get statistics: for each service pattern, get nr of trips, routes, total and avg distance and number of stops covered
  service_pattern_summary <- pattern_gtfs$trips |> # Join trips 
    left_join(pattern_gtfs$.$servicepatterns, by="service_id") |> # with service pattern
    left_join(shape_lengths, by="shape_id") |> # with shape length
    left_join(pattern_gtfs$stop_times, by="trip_id") |> # with planned route (stops and times)
    group_by(servicepattern_id) |> # group by service pattern
    summarise(
      trips = n(),
      routes = n_distinct(route_id),
      total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3, # divide by 1e3 to convert meters to kms
      route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
      stops=(n_distinct(stop_id)/2) # divided by two because usually there is one stop per direction
    )

  ## Add the number of days that each service is in operation (by join with $.$dates_servicepatterns)
  service_pattern_summary <- pattern_gtfs$.$dates_servicepatterns |>
    group_by(servicepattern_id) |>
    summarise(days_in_service = n()) |>
    left_join(service_pattern_summary, by = "servicepattern_id")  

  ## Get service patterns that run on the date selected
  service_pattern_ids = pattern_gtfs$.$dates_servicepatterns |>
      filter(date==date)
    
  service_ids = pattern_gtfs$.$servicepattern |> 
      filter(servicepattern_id %in% service_pattern_ids$servicepattern_id) |>
      pull(service_id)

  #### Filter by date    

  # Get stop frequency (missing data)

  frame = data.frame()

  for (i in 6:23) {
    stop_frequency <- get_stop_frequency(
      gtfs_date,
      start_time = sprintf("%.2d:00:00", i),
      end_time = sprintf("%.2d:59:59", i),
      service_ids = service_ids,
      by_route = TRUE
    )

    stop_frequency <- stop_frequency |>
      group_by(stop_id) |>
      summarise(frequency = sum(n_departures)) |>
      mutate(hour = i)
  
    frame <- rbind(frame, stop_frequency)
  }

  frequency <- frame |>
    ungroup() |>
    group_by(stop_id, hour) |>
    summarise(frequency = sum(frequency)) |>
    ungroup()

  table <- frequency |>
    left_join(gtfs_date$stops |>
    select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
    st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
  
  ## Prepend stop_id with GTFS.agency.agency_id to avoid duplicate stop ids from multiple GTFS merging
  table$stop_id <- paste0(sprintf("%s_", gtfs$agency$agency_id), table$stop_id)

  print("Finished GTFS analysis!")
  
  return(table)
}

# main()

request <- tribble(
  ~area, ~date, ~url,
  "barreiro", "2025-02-05", "https://www.tcbarreiro.pt/front/files/sample_gtfs/GTFS-TCB_24.zip", # VALIDATED against CityMapper
  "braga", "2025-02-05", "https://tub.pt/developer/gtfs/feed/tub.zip", # VALIDATED against previous version
  "lisboa", "2025-02-05", "https://gateway.carris.pt/gateway/gtfs/api/v2.8/GTFS", # VALIDATED against CityMapper
  "AML", "2025-02-05", "https://api.carrismetropolitana.pt/gtfs", # VALIDATED against CityMapper
  "cascais", "2025-02-05", "https://drive.google.com/u/0/uc?id=13ucYiAJRtu-gXsLa02qKJrGOgDjbnUWX&export=download", # VALIDATED against previous version
  "porto", "2025-02-05", "https://opendata.porto.digital/dataset/5275c986-592c-43f5-8f87-aabbd4e4f3a4/resource/1e0f4315-3694-42b0-a8ce-5218ad4742e5/download/horarios_gtfs_stcp_06_01_2025.zip" # VALIDATED against previous version
)
output_file <- "database/transit/bus_stop_frequency.gpkg"

aggregated_frequencies <- data.frame()

for (area in request$area) {
  frequencies <- process_gtfs(
    request$url[request$area == area],
    area,
    request$date[request$area == area]
  )
  
  assign(sprintf("frequencies_%s", area), frequencies)
  
  aggregated_frequencies <- rbind(aggregated_frequencies, frequencies)
}

print(sprintf("Finished processing! Storing output to %s...", output_file))
st_write(aggregated_frequencies, output_file, append=FALSE) # append=FALSE for overwrite

print("Done! :)")
