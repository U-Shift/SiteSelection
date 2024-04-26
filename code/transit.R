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
  dates = c("2024-04-03", "2024-04-10", "2024-04-10", "2024-04-10", "2019-04-10", "2019-04-10", "2022-11-09")
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
    
#### Braga 4-Planning
    
##Test Service pattern

#### Create a table on the gtfs feed that lets us filter by weekday/weekend service
     
      braga_pattern_gtfs <- set_servicepattern(braga_gtfs)
      
#### Convert stops and shapes to simple features
      
      braga_pattern_gtfs <- gtfs_as_sf(braga_pattern_gtfs)
      braga_pattern_gtfs$shapes$length <- st_length(braga_pattern_gtfs$shapes)
      
      braga_shape_lengths <- braga_pattern_gtfs$shapes |> 
        as.data.frame() |> 
        select(shape_id, length, -geometry)
      
#### Statistics up to services
      
      service_pattern_summary <- braga_pattern_gtfs$trips %>%
        left_join(braga_pattern_gtfs$.$servicepatterns, by="service_id") %>% 
        left_join(braga_shape_lengths, by="shape_id") %>%
        left_join(braga_pattern_gtfs$stop_times, by="trip_id") %>% 
        group_by(servicepattern_id) %>% 
        summarise(
          trips = n(), 
          routes = n_distinct(route_id),
          total_distance_per_day_km = sum(as.numeric(length), na.rm=TRUE)/1e3,
          route_avg_distance_km = (sum(as.numeric(length), na.rm=TRUE)/1e3)/(trips*routes),
          stops=(n_distinct(stop_id)/2))  
       
        
#### Number of days that each service operates
      
      service_pattern_summary <- braga_pattern_gtfs$.$dates_servicepatterns %>% 
        group_by(servicepattern_id) %>% 
        summarise(days_in_service = n()) %>% 
        left_join(service_pattern_summary, by="servicepattern_id")  
        
#### convert service pattern to an excel file
     #library(writexl)
    #write_xlsx(service_pattern_summary, "database/transit/braga_service_pattern_summary.xlsx")

#### Filter to the most common service pattern id  
      
      service_ids <- braga_pattern_gtfs$.$servicepattern %>% 
        filter(servicepattern_id == 's_6a7097c') %>% 
        pull(service_id)
      
      head(service_ids) %>% 
        knitr::kable()  
      
####  Analyze how trips fall under each of these service_ids, and how they relate to routes
      
      braga_pattern_gtfs$trips %>%
        filter(service_id %in% service_ids) %>%
        group_by(service_id, route_id) %>%
        summarise(trips = n()) %>%
        arrange(desc(trips)) %>%
        head() %>%
        knitr::kable()
      
#-----------------------------------------------------------      
      
#### FILTER BY WEDNESDAY    
    
      #Get stop frequency (missing data)
    
    braga_f = data.frame()
    
    for (i in 6:9){
      braga = get_stop_frequency(braga_date,
                                                    start_time = paste0("0",i,":00:00"),
                                                    end_time = paste0("0",i,":59:59"),
                                                    service_ids = NULL,
                                                    by_route = TRUE) 
      
      braga = braga |> 
             group_by(stop_id) |> 
              summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      braga_f = rbind(braga_f,braga)
    }
    
    for (i in 10:23){
      braga = get_stop_frequency(braga_date,
                                            start_time = paste0(i,":00:00"),
                                            end_time = paste0(i,":59:59"),
                                            service_ids = NULL,
                                            by_route = TRUE) 
      braga = braga |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      braga_f = rbind(braga_f,braga)
      }
      
      
    braga_frequency = braga_f |> 
        ungroup() |> 
        group_by(stop_id,hour) |> 
        summarise(frequency = sum(frequency)) |> 
        ungroup()
      
    
    braga_table = braga_frequency |> 
      left_join(braga_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
   
    mapview::mapview(braga_table) 
    
    
#### Lisbon
    
    #Get stop frequency (missing data)
    
    lisbon_f = data.frame()
    
    for (i in 6:9) {
      lisbon = get_stop_frequency(
        lisbon_date,
        start_time = paste0("0", i, ":00:00"),
        end_time = paste0("0", i, ":59:59"),
        service_ids = NULL,
        by_route = TRUE
      )
      
      lisbon = lisbon |>
        group_by(stop_id) |>
        summarise(frequency = sum(n_departures)) |>
        mutate(hour = i)
      lisbon_f = rbind(lisbon_f, lisbon)
    }
    
    for (i in 10:23) {
      lisbon = get_stop_frequency(
        lisbon_date,
        start_time = paste0(i, ":00:00"),
        end_time = paste0(i, ":59:59"),
        service_ids = NULL,
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
      st_as_sf(crs = 4326, coords = c("stop_lon", "stop_lat")
      )
    
    mapview::mapview(lisbon_table)     

    
        
#### AML

  # Setting the service patterns        
aml_pattern_gtfs <- set_servicepattern(aml_gtfs)

  # Convert stops and shapes into simple features
aml_pattern_gtfs <- gtfs_as_sf(aml_pattern_gtfs)
aml_pattern_gtfs$shapes$length <- st_length(aml_pattern_gtfs$shapes)

aml_shape_lengths <- aml_pattern_gtfs$shapes |>
  as.data.frame() |>
  select(shape_id, length, -geometry)

  # Get statistics up to services
service_pattern_summary_aml <- aml_pattern_gtfs$trips %>%
  left_join(aml_pattern_gtfs$.$servicepatterns, by = "service_id") %>%
  left_join(aml_shape_lengths, by = "shape_id") %>%
  left_join(aml_pattern_gtfs$stop_times, by = "trip_id") %>%
  group_by(servicepattern_id) %>%
  summarise(
    trips = n(),
    routes = n_distinct(route_id),
    total_distance_per_day_km = sum(as.numeric(length), na.rm = TRUE) / 1e3,
    route_avg_distance_km = (sum(as.numeric(length), na.rm = TRUE) / 1e3) / (trips * routes),
    stops = (n_distinct(stop_id) / 2)
  )

# Add the number of days that each service is in operation
service_pattern_summary_aml <- aml_pattern_gtfs$.$dates_servicepatterns %>%
  group_by(servicepattern_id) %>%
  summarise(days_in_service = n()) %>%
  left_join(service_pattern_summary_aml, by = "servicepattern_id") 

# The service patterns with most days in operation are: 
 # s_d38ffee (192 days)
 # s_0973a74 (191 days)
 # s_70dfe23 (188 days)

 # s_b00890f (47 days)
 # s_89ef482 (46 days)
 # s_49403b6 (45 days)
 # s_a6bf59d (44 days)
 
 # s_cd4a872 (43 days)
 # s_8415b1b (39 days)
 # s_ba5dcc4 (39 days)
 # s_f22b75f (39 days) 
 # s_fff1bcb (39 days)

 # s_8415b1b (39 days)
 # s_c614df3 (38 days)
 # s_757dfee (38 days)
 # s_6eb2a0d (38 days)
 # s_bc376dc (37 days)
 

 # IDENTIFY WHERE THESE SERVICE PATTERNS ARE OPERATING

 # Get the service_ids for the most common service patterns
service_ids_aml_1 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_d38ffee") %>%
  pull(service_id)

service_ids_aml_2 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_0973a74") %>%
  pull(service_id)

service_ids_aml_3 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_70dfe23") %>%
  pull(service_id)



service_ids_aml_4 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_b00890f") %>%
  pull(service_id)

service_ids_aml_5 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_89ef482") %>%
  pull(service_id)

service_ids_aml_6 <- aml_pattern_gtfs$.$servicepattern %>% 
  filter(servicepattern_id %in% "s_49403b6") %>% 
  pull(service_id)

service_ids_aml_7 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_a6bf59d") %>%
  pull(service_id)



service_ids_aml_8 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_cd4a872") %>%
  pull(service_id)

service_ids_aml_9 <- aml_pattern_gtfs$.$servicepattern %>%
  filter(servicepattern_id %in% "s_8415b1b") %>%
  pull(service_id)

service_ids_aml_10 <- aml_pattern_gtfs$.$servicepattern %>% 
  filter(servicepattern_id %in% "s_ba5dcc4") %>% 
  pull(service_id)

service_ids_aml_11 <- aml_pattern_gtfs$.$servicepattern %>% 
  filter(servicepattern_id %in% "s_f22b75f") %>% 
  pull(service_id)

service_ids_aml_12 <- aml_pattern_gtfs$.$servicepattern %>% 
  filter(servicepattern_id %in% "s_fff1bcb") %>% 
  pull(service_id)



# Get route geometries

aml_routes_pattern_1 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_1)
aml_routes_pattern_2 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_2)
aml_routes_pattern_3 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_3)

aml_routes_pattern_4 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_4)
aml_routes_pattern_5 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_5)
aml_routes_pattern_6 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_6)
aml_routes_pattern_7 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_7)

aml_routes_pattern_8 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_8)
aml_routes_pattern_9 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_9)
aml_routes_pattern_10 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_10)
aml_routes_pattern_11 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_11)
aml_routes_pattern_12 <- get_route_frequency(aml_pattern_gtfs, service_ids = service_ids_aml_12)


# get_route_geometry needs a gtfs object that includes shapes as simple feature data frames

routes_sf_1 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_1)
routes_sf_2 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_2)
routes_sf_3 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_3)

routes_sf_4 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_4)
routes_sf_5 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_5)
routes_sf_6 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_6)
routes_sf_7 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_7)

routes_sf_8 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_8)
routes_sf_9 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_9)
routes_sf_10 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_10)
routes_sf_11 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_11)
routes_sf_12 <- get_route_geometry(aml_pattern_gtfs, service_ids = service_ids_aml_12)


# join the geometries to the calculated frequencies

routes_sf_1 <- routes_sf_1 |> 
  inner_join(aml_routes_pattern_1, by = "route_id")

routes_sf_2 <- routes_sf_2 |> 
  inner_join(aml_routes_pattern_2, by = "route_id")

routes_sf_3 <- routes_sf_3 |> 
  inner_join(aml_routes_pattern_3, by = "route_id")


routes_sf_4 <- routes_sf_4 |>
  inner_join(aml_routes_pattern_4, by = "route_id")

routes_sf_5 <- routes_sf_5 |>
  inner_join(aml_routes_pattern_5, by = "route_id")

routes_sf_6 <- routes_sf_6 |>
  inner_join(aml_routes_pattern_6, by = "route_id")

routes_sf_7 <- routes_sf_7 |>
  inner_join(aml_routes_pattern_7, by = "route_id")


routes_sf_8 <- routes_sf_8 |> 
  inner_join(aml_routes_pattern_8, by = "route_id")

routes_sf_9 <- routes_sf_9 |>
  inner_join(aml_routes_pattern_9, by = "route_id")

routes_sf_10 <- routes_sf_10 |>
  inner_join(aml_routes_pattern_10, by = "route_id")

routes_sf_11 <- routes_sf_11 |>
  inner_join(aml_routes_pattern_11, by = "route_id")

routes_sf_12 <- routes_sf_12 |>
  inner_join(aml_routes_pattern_12, by = "route_id")


mapview::mapview(routes_sf_1)

mapview::mapview(routes_sf_2)

mapview::mapview(routes_sf_3)
    
mapview::mapview(routes_sf_4)        

mapview::mapview(routes_sf_5)

mapview::mapview(routes_sf_6)

mapview::mapview(routes_sf_7)

mapview::mapview(routes_sf_8)

mapview::mapview(routes_sf_9)

mapview::mapview(routes_sf_10)

mapview::mapview(routes_sf_11)

mapview::mapview(routes_sf_12)
#-------------------------------------------------------------    
    
    #Get stop frequency (missing data)
    
    aml_f = data.frame()
    
    for (i in 6:9){
      aml = get_stop_frequency(aml_date,
                                   start_time = paste0("0",i,":00:00"),
                                   end_time = paste0("0",i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE) 
      
      aml = aml |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      aml_f = rbind(aml_f, aml)
    }
    
    for (i in 10:23) {
      aml = get_stop_frequency(aml_date,
                                   start_time = paste0(i,":00:00"),
                                   end_time = paste0(i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE)
      
      aml = aml |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      aml_f =rbind(aml_f, aml)
    }
    
    
    aml_frequency = aml_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    aml_table = aml_frequency |> 
      left_join(aml_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(aml_table)         
    


#### Cascais
    
    #Get stop frequency (missing data)
    
    cascais_f = data.frame()
    
    for (i in 6:9){
      cascais = get_stop_frequency(cascais_date,
                                    start_time = paste0("0",i,":00:00"),
                                    end_time = paste0("0",i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE) 
      
      cascais = cascais |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      cascais_f = rbind(cascais_f, cascais)
    }
    
    for (i in 10:23) {
      cascais = get_stop_frequency(cascais_date,
                                    start_time = paste0(i,":00:00"),
                                    end_time = paste0(i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE)
      
      cascais = cascais |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      cascais_f =rbind(cascais_f, cascais)
    }
    
    
    cascais_frequency = cascais_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    cascais_table = cascais_frequency |> 
      left_join(cascais_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(cascais_table)      
    

#### Barreiro
    
    #Get stop frequency (missing data)
    
    barreiro_f = data.frame()
    
    for (i in 6:9){
      barreiro = get_stop_frequency(barreiro_date,
                                    start_time = paste0("0",i,":00:00"),
                                    end_time = paste0("0",i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE) 
      
      barreiro = barreiro |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      barreiro_f = rbind(barreiro_f, barreiro)
    }
    
    for (i in 10:23) {
      barreiro = get_stop_frequency(barreiro_date,
                                    start_time = paste0(i,":00:00"),
                                    end_time = paste0(i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE)
      
      barreiro = barreiro |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      barreiro_f =rbind(barreiro_f, barreiro)
    }
    
    
    barreiro_frequency = barreiro_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    barreiro_table = barreiro_frequency |> 
      left_join(barreiro_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(barreiro_table)      
    
#### Agueda
    
    #Get stop frequency (missing data)
    
    agueda_f = data.frame()
    
    for (i in 6:9){
      agueda = get_stop_frequency(agueda_date,
                                     start_time = paste0("0",i,":00:00"),
                                     end_time = paste0("0",i,":59:59"),
                                     service_ids = NULL,
                                     by_route = TRUE) 
      
      agueda = agueda |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      agueda_f = rbind(agueda_f, agueda)
    }
    
    for (i in 10:23) {
      agueda = get_stop_frequency(agueda_date,
                                     start_time = paste0(i,":00:00"),
                                     end_time = paste0(i,":59:59"),
                                     service_ids = NULL,
                                     by_route = TRUE)
      
      agueda = agueda |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      agueda_f =rbind(agueda_f, agueda)
    }
    
    
    agueda_frequency = agueda_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    agueda_table = agueda_frequency |> 
      left_join(agueda_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(agueda_table)      
    

#### Porto
    
    #Get stop frequency (missing data)
    
    porto_f = data.frame()
    
    for (i in 6:9){
      porto = get_stop_frequency(porto_date,
                                   start_time = paste0("0",i,":00:00"),
                                   end_time = paste0("0",i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE) 
      
      porto = porto |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      agueda_f = rbind(porto_f, porto)
    }
    
    for (i in 10:23) {
      porto = get_stop_frequency(porto_date,
                                   start_time = paste0(i,":00:00"),
                                   end_time = paste0(i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE)
      
      porto = porto |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      porto_f =rbind(porto_f, porto)
    }
    
    
    porto_frequency = porto_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    porto_table = porto_frequency |> 
      left_join(porto_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(porto_table)      
    
    
    

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

    
   