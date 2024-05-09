# aim: get daily frequency of transit modes in Portugal


# Automatically download GTFS data---------------------------------------------------

# create directory if does not exist already
if (!dir.exists("database/transit")) {
  dir.create("database/transit", recursive = TRUE)
}

# Braga - Operador: 4 Planning
braga_gtfs_url = "https://gtfs.pro/files/uran/improved-gtfs-braga.zip" # 1.6MB
download.file(braga_gtfs_url, destfile = "database/transit/braga_gtfs.zip")

# Lisbon - Operador: Carris
lisbon_gtfs_url = "https://gtfs.pro/files/uran/improved-gtfs-gateway.zip" # 15.8MB
download.file(lisbon_gtfs_url, destfile = "database/transit/lisbon_gtfs.zip")

# Área Metropolitana de Lisboa - Operador: Carris Metropolitana
aml_gtfs_url = "https://github.com/carrismetropolitana/gtfs/raw/live/CarrisMetropolitana.zip" #48MB
download.file(aml_gtfs_url, destfile = "database/transit/AML_gtfs.zip")

# Cascais - Operador: Mobi-Cascais
cascais_gtfs_url = "https://gtfs.pro/files/uran/improved-gtfs-mobi-cascais.zip" # 1.4MB
download.file(cascais_gtfs_url, destfile = "database/transit/cascais_gtfs.zip")

# Barreiro - Operador: Transporlis
barreiro_gtfs_url = "https://gtfs.pro/files/uran/improved-gtfs-transportes-colectivos-do-barreiro-pt.zip" # 0.33MB
download.file(barreiro_gtfs_url, destfile = "database/transit/barreiro_gtfs.zip")

# Agueda, Aveiro - Operador: Câmara Municipal de Aveiro (operador?)
agueda_gtfs_url = "https://gtfs.pro/files/uran/improved-gtfs-agueda.zip" # 0.25MB
download.file(agueda_gtfs_url, destfile = "database/transit/agueda_gtfs.zip")

# Porto - Operador: STCP
porto_gtfs_url = "https://gtfs.pro/files/uran/improved-gtfs-stcp-porto-pt.zip" # 7.4MB
download.file(porto_gtfs_url, destfile = "database/transit/porto_gtfs.zip")


# filter gtfs by a tipical working day ------------------------------------

library(sf)
library(tidyverse)
library(lubridate)
library(tidytransit)

# read gtfs files
braga_gtfs = read_gtfs("database/transit/braga_gtfs.zip")
lisbon_gtfs = read_gtfs("database/transit/lisbon_gtfs.zip")
aml_gtfs = read_gtfs("database/transit/AML_gtfs.zip")
cascais_gtfs = read_gtfs("database/transit/cascais_gtfs.zip")
barreiro_gtfs = read_gtfs("database/transit/barreiro_gtfs.zip")
agueda_gtfs = read_gtfs("database/transit/agueda_gtfs.zip")
porto_gtfs = read_gtfs("database/transit/porto_gtfs.zip")
      

# Select and filter databases by a representative date (Wednesday)

filter_dates = data.frame(
  mun = c("braga", "lisbon", "aml", "cascais", "barreiro", "agueda", "porto"),
  dates = c("2024-04-03", "2024-05-15", "2024-04-10", "2024-04-10", "2019-04-10", "2019-04-10", "2022-11-09")
)

for (mun in filter_dates$mun) {

  cena_date = filter_feed_by_date(
    get(paste0(mun, "_gtfs")),
    extract_date = filter_dates$dates[filter_dates$mun == mun]
  )
  assign(paste0(mun, "_date"), cena_date)
  print(mun)

}

#Organize the table calculating the frequencies per bus stop
    

# Braga -------------------------------------------------------------------

## Service pattern

#### Create a table on the gtfs feed that lets us filter by weekday/weekend service
     
      braga_pattern_gtfs = set_servicepattern(braga_gtfs) # WARNING: every time we run this, random numbers will be generated for the service patterns
      
#### Convert stops and shapes to simple features
      
      braga_pattern_gtfs = gtfs_as_sf(braga_pattern_gtfs)
      braga_pattern_gtfs$shapes$length = st_length(braga_pattern_gtfs$shapes)
      
      braga_shape_lengths = braga_pattern_gtfs$shapes |>
        as.data.frame() |>
        select(shape_id, length, -geometry)
      
#### Statistics up to services
      
      service_pattern_summary_braga = braga_pattern_gtfs$trips |> 
        left_join(braga_pattern_gtfs$.$servicepatterns, by="service_id") |>  
        left_join(braga_shape_lengths, by="shape_id") |> 
        left_join(braga_pattern_gtfs$stop_times, by="trip_id") |>  
        group_by(servicepattern_id) |>  
        summarise(
          trips = n(), 
          routes = n_distinct(route_id),
          total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
          route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
          stops=(n_distinct(stop_id)/2))  
       
#### Number of days that each service operates
      
      service_pattern_summary_braga = braga_pattern_gtfs$.$dates_servicepatterns |>
        group_by(servicepattern_id) |>
        summarise(days_in_service = n()) |>
        left_join(service_pattern_summary_braga, by = "servicepattern_id")  
        
#### convert service pattern to an excel file
     #library(writexl)
    #write_xlsx(service_pattern_summary, "database/transit/braga_service_pattern_summary.xlsx")

#### Filter to the most common service pattern id  
      
      service_id_braga = braga_pattern_gtfs$.$servicepattern |>
        filter(servicepattern_id == 's_6a7097c') |>  # my random generated service pattern
        pull(service_id)
      
      head(service_id_braga) |>
        knitr::kable()  
      
#### Filter by date    
    
    #Get stop frequency (missing data)
    
    braga_f = data.frame()
    
    for (i in 6:23) {
      braga = get_stop_frequency(
        braga_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = service_id_braga,
        by_route = TRUE
      )
      
      braga = braga |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      braga_f = rbind(braga_f, braga)
    }
    
    braga_frequency = braga_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    braga_table = braga_frequency |>
      left_join(braga_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(braga_table) 
    
    

# Lisbon ------------------------------------------------------------------

    #### Create a table on the gtfs feed that lets us filter by weekday/weekend service
    
    lisbon_pattern_gtfs = set_servicepattern(lisbon_gtfs)
    
    #### Convert stops and shapes to simple features
    
    lisbon_pattern_gtfs = gtfs_as_sf(lisbon_pattern_gtfs)
    lisbon_pattern_gtfs$shapes$length = st_length(lisbon_pattern_gtfs$shapes)
    
    lisbon_shape_lengths = lisbon_pattern_gtfs$shapes |> 
      as.data.frame() |> 
      select(shape_id, length, -geometry)
    
    #### Statistics up to services
    
    service_pattern_summary_lisbon = lisbon_pattern_gtfs$trips |> 
      left_join(lisbon_pattern_gtfs$.$servicepatterns, by="service_id") |>  
      left_join(lisbon_shape_lengths, by="shape_id") |> 
      left_join(lisbon_pattern_gtfs$stop_times, by="trip_id") |>  
      group_by(servicepattern_id) |>  
      summarise(
        trips = n(), 
        routes = n_distinct(route_id),
        total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
        route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
        stops=(n_distinct(stop_id)/2))  
    
    #### Number of days that each service operates
    
    service_pattern_summary_lisbon = lisbon_pattern_gtfs$.$dates_servicepatterns |>  
      group_by(servicepattern_id) |>  
      summarise(days_in_service = n()) |>  
      left_join(service_pattern_summary_lisbon, by="servicepattern_id")  
    
    #### Filter to the most common service pattern id  
    
    service_id_lisbon = lisbon_pattern_gtfs$.$servicepattern |>  
      filter(servicepattern_id == 's_840dfaf') |>  
      pull(service_id)
    
    head(service_id_lisbon) |>  
      knitr::kable()  
    
    
# Filter by date and get stop frequency
    
    lisbon_f = data.frame()
    
    for (i in 6:23) {
      lisbon = get_stop_frequency(
        lisbon_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = service_id_lisbon,
        by_route = TRUE
      )
      
      lisbon = lisbon |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      lisbon_f = rbind(lisbon_f, lisbon)
    }
    
    
    lisbon_frequency = lisbon_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    lisbon_table = lisbon_frequency |>
      left_join(lisbon_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(lisbon_table)     
        

# AML ---------------------------------------------------------------------

  # Setting the service patterns        
aml_pattern_gtfs = set_servicepattern(aml_gtfs)

  # Convert stops and shapes into simple features
aml_pattern_gtfs = gtfs_as_sf(aml_pattern_gtfs)
aml_pattern_gtfs$shapes$length = st_length(aml_pattern_gtfs$shapes)

aml_shape_lengths = aml_pattern_gtfs$shapes |>
  as.data.frame() |>
  select(shape_id, length, -geometry)

  # Get statistics up to services
service_pattern_summary_aml = aml_pattern_gtfs$trips |> 
  left_join(aml_pattern_gtfs$.$servicepatterns, by = "service_id") |> 
  left_join(aml_shape_lengths, by = "shape_id") |> 
  left_join(aml_pattern_gtfs$stop_times, by = "trip_id") |> 
  group_by(servicepattern_id) |> 
  summarise(
    trips = n(),
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm = TRUE) / 1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm = TRUE) / 1e3) / (trips * routes),
    stops = (n_distinct(stop_id) / 2)
  )

# Add the number of days that each service is in operation
service_pattern_summary_aml = aml_pattern_gtfs$.$dates_servicepatterns |> 
  group_by(servicepattern_id) |> 
  summarise(days_in_service = n()) |> 
  left_join(service_pattern_summary_aml, by = "servicepattern_id") 

# We tested the service patterns for the AML since the locations are very different. 
## Adopted service patterns (ranked by "days in service"): 

# 1. Service pattern #1: "s_d38ffee" (192 days)
# 2. Service pattern #2: "s_0973a74" (191 days)
# 3. Service pattern #3: "s_70dfe23" (188 days)
# 4. Service pattern #12: "s_fff1bcb" (39 days)
# 5. Service pattern #18: "s_bc376dc" (37 days)

 # Get the service_ids for the most common service patterns
service_ids_aml_1 = aml_pattern_gtfs$.$servicepattern |> 
  filter(servicepattern_id %in% "s_d38ffee") |> 
  pull(service_id)

service_ids_aml_2 = aml_pattern_gtfs$.$servicepattern |> 
  filter(servicepattern_id %in% "s_0973a74") |> 
  pull(service_id)

service_ids_aml_3 = aml_pattern_gtfs$.$servicepattern |> 
  filter(servicepattern_id %in% "s_70dfe23") |> 
  pull(service_id)

service_ids_aml_12 = aml_pattern_gtfs$.$servicepattern |>  
  filter(servicepattern_id %in% "s_fff1bcb") |>  
  pull(service_id)

service_ids_aml_18 = aml_pattern_gtfs$.$servicepattern |> 
  filter(servicepattern_id %in% "s_bc376dc") |> 
  pull(service_id)


# Get route geometries

aml_routes_pattern_1 = get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_1)
aml_routes_pattern_2 = get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_2)
aml_routes_pattern_3 = get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_3)
aml_routes_pattern_12 = get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_12)
aml_routes_pattern_18 = get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_18)


# get_route_geometry needs a gtfs object that includes shapes as simple feature data frames

routes_sf_1 = get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_1)
routes_sf_2 = get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_2)
routes_sf_3 = get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_3)
routes_sf_12 = get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_12)
routes_sf_18 = get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_18)

# join the geometries to the calculated frequencies

routes_sf_1 = routes_sf_1 |> inner_join(aml_routes_pattern_1, by = "route_id")
routes_sf_2 = routes_sf_2 |> inner_join(aml_routes_pattern_2, by = "route_id")
routes_sf_3 = routes_sf_3 |> inner_join(aml_routes_pattern_3, by = "route_id")
routes_sf_12 = routes_sf_12 |> inner_join(aml_routes_pattern_12, by = "route_id")
routes_sf_18 = routes_sf_18 |> inner_join(aml_routes_pattern_18, by = "route_id")


# visualize the routes

# mapview::mapview(routes_sf_1)
# mapview::mapview(routes_sf_2)
# mapview::mapview(routes_sf_3)
# mapview::mapview(routes_sf_12)
# mapview::mapview(routes_sf_18)


#get start and end days in operation for each service pattern

service_pattern_summary_aml = aml_pattern_gtfs$.$dates_servicepatterns |> 
  group_by(servicepattern_id) |> 
  summarise(start_date = min(date), end_date = max(date)) |> 
  left_join(service_pattern_summary_aml, by = "servicepattern_id")

#filter service_pattern_summary_aml to the service patterns 

service_pattern_summary_aml = service_pattern_summary_aml |> 
  filter(servicepattern_id %in% c("s_d38ffee", "s_0973a74", "s_70dfe23", "s_fff1bcb", "s_bc376dc"))

#join selected service patterns ids with the frequencies per bus stop
    
    aml_f = data.frame()
    
    for (i in 6:23) {
      aml = get_stop_frequency(
        aml_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = c(
          service_ids_aml_1,
          service_ids_aml_2,
          service_ids_aml_3,
          service_ids_aml_12,
          service_ids_aml_18
        ),
        by_route = TRUE
      )
      
      aml = aml |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      aml_f = rbind(aml_f, aml)
    }
    
    
    aml_frequency = aml_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    aml_table = aml_frequency |>
      left_join(aml_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(aml_table)         
    


# Cascais -----------------------------------------------------------------


    # Setting the service patterns
    cascais_pattern_gtfs = set_servicepattern(cascais_gtfs)
    
    # Convert stops and shapes into simple features
    cascais_pattern_gtfs = gtfs_as_sf(cascais_pattern_gtfs)
    cascais_pattern_gtfs$shapes$length = st_length(cascais_pattern_gtfs$shapes)
    
    cascais_shape_lengths = cascais_pattern_gtfs$shapes |>
      as.data.frame() |>
      select(shape_id, length, -geometry)
    
    # Get statistics up to services
    service_pattern_summary_cascais = cascais_pattern_gtfs$trips |> 
      left_join(cascais_pattern_gtfs$.$servicepatterns, by = "service_id") |> 
      left_join(cascais_shape_lengths, by = "shape_id") |> 
      left_join(cascais_pattern_gtfs$stop_times, by = "trip_id") |> 
      group_by(servicepattern_id) |> 
      summarise(
        trips = n(),
        routes = n_distinct(route_id),
        total_distance_per_day_km = sum(as.numeric(length), na.rm = TRUE) / 1e3,
        route_avg_distance_km = (sum(as.numeric(length), na.rm = TRUE) / 1e3) / (trips * routes),
        stops = (n_distinct(stop_id) / 2)
      )
    
    # Add the number of days that each service is in operation
    service_pattern_summary_cascais = cascais_pattern_gtfs$.$dates_servicepatterns |> 
      group_by(servicepattern_id) |> 
      summarise(days_in_service = n()) |> 
      left_join(service_pattern_summary_cascais, by = "servicepattern_id")
    
    # Get the service_ids for the most common service patterns
    service_ids_cascais = cascais_pattern_gtfs$.$servicepattern |> 
      filter(servicepattern_id %in% "s_ebf7135") |> 
      pull(service_id)
    
    
    # Filter by date and get stop frequency
    
    cascais_f = data.frame()
    
    for (i in 6:23) {
      cascais = get_stop_frequency(
        cascais_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = service_ids_cascais,
        by_route = TRUE
      )
      
      cascais = cascais |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      cascais_f = rbind(cascais_f, cascais)
    }
    
    
    cascais_frequency = cascais_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    cascais_table = cascais_frequency |>
      left_join(cascais_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(cascais_table)      
    


# Barreiro ----------------------------------------------------------------


    # Setting the service patterns        
    barreiro_pattern_gtfs = set_servicepattern(barreiro_gtfs)
    
    # Convert stops and shapes into simple features
    barreiro_pattern_gtfs = gtfs_as_sf(barreiro_pattern_gtfs)
    barreiro_pattern_gtfs$shapes$length = st_length(barreiro_pattern_gtfs$shapes)
    
    barreiro_shape_lengths = barreiro_pattern_gtfs$shapes |>
      as.data.frame() |>
      select(shape_id, length, -geometry)
    
    # Get statistics up to services
    service_pattern_summary_barreiro = barreiro_pattern_gtfs$trips |> 
      left_join(barreiro_pattern_gtfs$.$servicepatterns, by = "service_id") |> 
      left_join(barreiro_shape_lengths, by = "shape_id") |> 
      left_join(barreiro_pattern_gtfs$stop_times, by = "trip_id") |> 
      group_by(servicepattern_id) |> 
      summarise(
        trips = n(),
        routes = n_distinct(route_id),
        total_distance_per_day_km = sum(as.numeric(length), na.rm = TRUE) / 1e3,
        route_avg_distance_km = (sum(as.numeric(length), na.rm = TRUE) / 1e3) / (trips * routes),
        stops = (n_distinct(stop_id) / 2)
      )
    
    # Add the number of days that each service is in operation
    service_pattern_summary_barreiro = barreiro_pattern_gtfs$.$dates_servicepatterns |> 
      group_by(servicepattern_id) |> 
      summarise(days_in_service = n()) |> 
      left_join(service_pattern_summary_barreiro, by = "servicepattern_id") 
    
    
    # Get the service_ids for the most common service patterns
    service_ids_barreiro = barreiro_pattern_gtfs$.$servicepattern |> 
      filter(servicepattern_id %in% "s_22f0a7c") |> 
      pull(service_id)
    
# Filter by date and get stop frequency
    
    barreiro_f = data.frame()
    
    for (i in 6:23) {
      barreiro = get_stop_frequency(
        barreiro_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = service_ids_barreiro,
        by_route = TRUE
      )
      
      barreiro = barreiro |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      barreiro_f = rbind(barreiro_f, barreiro)
    }
    
    
    barreiro_frequency = barreiro_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    barreiro_table = barreiro_frequency |>
      left_join(barreiro_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(barreiro_table)      

    

# Agueda ------------------------------------------------------------------


    # Setting the service patterns        
    agueda_pattern_gtfs = set_servicepattern(agueda_gtfs)
    
    # Convert stops and shapes into simple features
    agueda_pattern_gtfs = gtfs_as_sf(agueda_pattern_gtfs)
    agueda_pattern_gtfs$shapes$length = st_length(agueda_pattern_gtfs$shapes)
    
    agueda_shape_lengths = agueda_pattern_gtfs$shapes |>
      as.data.frame() |>
      select(shape_id, length, -geometry)
    
    # Get statistics up to services
    service_pattern_summary_agueda = agueda_pattern_gtfs$trips |> 
      left_join(agueda_pattern_gtfs$.$servicepatterns, by = "service_id") |> 
      left_join(agueda_shape_lengths, by = "shape_id") |> 
      left_join(agueda_pattern_gtfs$stop_times, by = "trip_id") |> 
      group_by(servicepattern_id) |> 
      summarise(
        trips = n(),
        routes = n_distinct(route_id),
        total_distance_per_day_km = sum(as.numeric(length), na.rm = TRUE) / 1e3,
        route_avg_distance_km = (sum(as.numeric(length), na.rm = TRUE) / 1e3) / (trips * routes),
        stops = (n_distinct(stop_id) / 2)
      )
    
    # Add the number of days that each service is in operation
    service_pattern_summary_agueda = agueda_pattern_gtfs$.$dates_servicepatterns |> 
      group_by(servicepattern_id) |> 
      summarise(days_in_service = n()) |> 
      left_join(service_pattern_summary_agueda, by = "servicepattern_id") 
    
    # Get the service_ids for the most common service patterns
    service_ids_agueda = agueda_pattern_gtfs$.$servicepattern |> 
      filter(servicepattern_id %in% "s_3c9f481") |> 
      pull(service_id)
    
# Filter by date and get stop frequency
    
    agueda_f = data.frame()
    
    for (i in 6:23) {
      agueda = get_stop_frequency(
        agueda_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = service_ids_agueda,
        by_route = TRUE
      )
      
      agueda = agueda |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      agueda_f = rbind(agueda_f, agueda)
    }
    
    
    agueda_frequency = agueda_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    agueda_table = agueda_frequency |>
      left_join(agueda_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(agueda_table)      
    


# Porto -------------------------------------------------------------------


    # Setting the service patterns
    porto_pattern_gtfs = set_servicepattern(porto_gtfs)
    
    # Convert stops and shapes into simple features
    porto_pattern_gtfs = gtfs_as_sf(porto_pattern_gtfs)
    porto_pattern_gtfs$shapes$length = st_length(porto_pattern_gtfs$shapes)
    
    porto_shape_lengths = porto_pattern_gtfs$shapes |>
      as.data.frame() |>
      select(shape_id, length, -geometry)
    
    # Get statistics up to services
    service_pattern_summary_porto = porto_pattern_gtfs$trips |> 
      left_join(porto_pattern_gtfs$.$servicepatterns, by = "service_id") |> 
      left_join(porto_shape_lengths, by = "shape_id") |> 
      left_join(porto_pattern_gtfs$stop_times, by = "trip_id") |> 
      group_by(servicepattern_id) |> 
      summarise(
        trips = n(),
        routes = n_distinct(route_id),
        total_distance_per_day_km = sum(as.numeric(length), na.rm = TRUE) / 1e3,
        route_avg_distance_km = (sum(as.numeric(length), na.rm = TRUE) / 1e3) / (trips * routes),
        stops = (n_distinct(stop_id) / 2)
      )
    
    # Add the number of days that each service is in operation
    service_pattern_summary_porto = porto_pattern_gtfs$.$dates_servicepatterns |> 
      group_by(servicepattern_id) |> 
      summarise(days_in_service = n()) |> 
      left_join(service_pattern_summary_porto, by = "servicepattern_id")
    
    # Get the service_ids for the most common service patterns
    service_ids_porto = porto_pattern_gtfs$.$servicepattern |> 
      filter(servicepattern_id %in% "s_f70554e") |> 
      pull(service_id)
    

# Filter by date and get stop frequency
    
    porto_f = data.frame()
    
    for (i in 6:23) {
      porto = get_stop_frequency(
        porto_date,
        start_time = ifelse(i < 10, paste0(i, ":00:00"), paste0(i, ":00:00")),
        end_time = ifelse(i < 10, paste0("0", i, ":59:59"), paste0(i, ":59:59")),
        service_ids = service_ids_porto,
        by_route = TRUE
      )
      
      porto = porto |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      agueda_f = rbind(porto_f, porto)
    }
    
    
    porto_frequency = porto_f |>
      ungroup() |>
      group_by(stop_id, hour) |>
      summarise(frequency = sum(frequency)) |>
      ungroup()
    
    
    porto_table = porto_frequency |>
      left_join(porto_date$stops |>
                  select(stop_id, stop_lon, stop_lat), by = "stop_id") |>
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat"))
    
    #mapview::mapview(porto_table)      
    
    

# COMBINE ALL BUS STOPS ---------------------------------------------------

    transit_table_all = rbind(braga_table, lisbon_table, aml_table, cascais_table, barreiro_table, agueda_table, porto_table)
    
    #mapview::mapview(transit_table_all)
    
# Save the final table in gpkg format 
    
    st_write(transit_table_all, "database/transit/bus_stop_frequency.gpkg")
    
    piggyback::pb_upload("database/transit/bus_stop_freq.gpkg")
    # download.file("https://github.com/U-Shift/SiteSelection/releases/download/0.1/bus_stop_freq.gpkg", "database/transit/bus_stop_freq.gpkg")
 
       
# list municipalities with GTFS -------------------------------------------
municipios = list(
  c(
    "Lisboa",
    "Oeiras",
    "Amadora",
    "Sintra",
    "Cascais",
    "Barreiro",
    "Braga",
    "Agueda", # Águeda?
    "Porto"
  )
)

    
   