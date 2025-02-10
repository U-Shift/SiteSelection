# imports 
library(sf)
library(tidyverse)
library(lubridate)
library(tidytransit)

source("R/gtfs_download.R")
source("R/calendar_nextBusinessWednesday.R")

# methods

#' Process GTFS file
#' @param gtfs_url The url of the GTFS zip file
#' @param area String with area name
#' @param date Reference date to consider when analysing the GTFS file. Defaults to next business wednesday
#' @param route_types Restricts analysis to defined route_types, defaults to those that have conflicts on urban environments: tram and bus
process_gtfs <- function(gtfs_url, area, date=NULL, route_types=list(0,3,5,11)) {
  print(sprintf("Analysing GTFS for %s...", area))

  # DOWNLOAD GTFS and store it locally
  destfile = gtfs_download(gtfs_url, area)
  
  # Open GTFS with tidytransit library and filter by date
  print(sprintf("> Openning it for processing (%s)...", destfile))
  gtfs <- tidytransit::read_gtfs(destfile)
  print(sprintf("> Openned GTFS for %s (ID %s)!", gtfs$agency$agency_name, gtfs$agency$agency_id))
  

  # FILTER GTFS to focus on only 
  
  ## Consider transit data for one day only
  if (is.null(date)) {
    date = calendar_nextBusinessWednesday()
    print(sprintf("> Reference date not provided, considering next business wednesday: %s...", date))
  }
  print(sprintf("> Filtering by reference date %s...", date))
  gtfs_date <- filter_feed_by_date(
    gtfs, extract_date = date
  )
  print(sprintf("> There are %d routes operating %d trips on %d stops...", 
    length(gtfs_date$trips$trip_id),
    length(gtfs_date$routes$route_id),
    length(gtfs_date$stops$stop_id)
  ))
  
  # Consider trips for defined modes only
  if (!is.null(route_types)) {
    print(sprintf("> Filtering by route types %s...", toString(route_types)))
    routesNBefore <- length(gtfs_date$routes$route_id)
    tripsNBefore <- length(gtfs_date$trips$trip_id)
    
    routes_ids <- gtfs_date$routes[gtfs_date$routes$route_type %in% route_types, ]$route_id
    trips_ids <- gtfs_date$trips[gtfs_date$trips$route_id %in% routes_ids, ]$trip_id
    gtfs_date <- filter_feed_by_trips(gtfs_date, trips_ids)
        
    routesNAfter = length(gtfs_date$routes$route_id)
    tripsNAfter = length(gtfs_date$trips$trip_id)
    print(sprintf("> Removed %d routes, representing %d trips, proceding analysis...", routesNBefore-routesNAfter, tripsNBefore-tripsNAfter))
  }
  
  if (length(gtfs_date$trips$trip_id)==0) {
    stop("No trips found after filtering! Make sure you have a valid GTFS!")
  }
    
  # PROCESS GTFS, generating table calculating the frequencies per bus stop

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

request <- read.csv("database/gtfs/gtfs_sources.csv") |>
  filter((Type == "Urban" | Type == "Inter-urban") & Ignore!=1)

output_file <- "database/transit/bus_stop_frequency.gpkg"

aggregated_frequencies <- data.frame()

for (area in request$Area) {
  frequencies <- process_gtfs(
    request$URL[request$Area == area],
    area
  )
  
  assign(sprintf("frequencies_%s", area), frequencies)
  
  aggregated_frequencies <- rbind(aggregated_frequencies, frequencies)
}

print(sprintf("Finished processing! Storing output to %s...", output_file))
st_write(aggregated_frequencies, output_file, append=FALSE) # append=FALSE for overwrite

print("Done! :)")
