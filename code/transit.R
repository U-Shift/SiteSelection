# aim: get daily frequency of transit modes in the area


# Automatically download GTFS data---------------------------------------------------

  # Braga 
  
    #Operador: 4 Planning
  
    Braga_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-braga.zip" # 1.6MB 
    download.file(Braga_gtfs_zip, destfile = "database/transit/braga_gtfs_4planning.zip")

  # Lisbon
    
    #Operador: Carris
    
    Lisbon_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-gateway.zip" # 15.8MB
    download.file(Lisbon_gtfs_zip, destfile = "database/transit/lisbon_gtfs.zip")
    
  # Área Metropolitana de Lisboa
    
    #Operador: Carris Metropolitana
    
    AML_gtfs_zip = "https://github.com/carrismetropolitana/gtfs/raw/live/CarrisMetropolitana.zip" #48MB
    download.file(AML_gtfs_zip, destfile = "database/transit/AML_gtfs.zip")
  
  # Funchal
    
    #Operador: 
    
    funchal_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-horariosdofunchal.zip" # 3.8MB
    download.file(funchal_gtfs_zip, destfile = "database/transit/funchal_gtfs.zip")
    
  # Cascais: São Domingos de Rana
    
    #Operador: Mobi-Cascais
    
    cascais_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-mobi-cascais.zip" # 1.4MB
    download.file(cascais_gtfs_zip, destfile = "database/transit/cascais_gtfs.zip")

  # Barreiro

    #Operador: Transporlis
    
    barreiro_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-transportes-colectivos-do-barreiro-pt.zip" # 0.33MB
    download.file(barreiro_gtfs_zip, destfile = "database/transit/barreiro_gtfs.zip")

  # Agueda, Aveiro
    
    #Operador: Câmara Municipal de Aveiro (operador?)
    
    agueda_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-agueda.zip" # 0.25MB
    download.file(agueda_gtfs_zip, destfile = "database/transit/agueda_gtfs.zip")
    
  # Porto
    
    #Operador: 
    
    porto_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-stcp-porto-pt.zip" # 7.4MB
    download.file(porto_gtfs_zip, destfile = "database/transit/porto_gtfs.zip")
    
#Import libraries

    library(sf)
    library(readr)
    library(tidyverse)
    library(lubridate)
    library(tidytransit)
    
# read gtfs files
    
    braga_4Planning_gtfs <- read_gtfs("database/transit/braga_gtfs_4planning.zip")
    lisbon_gtfs <- read_gtfs("database/transit/lisbon_gtfs.zip")
    aml_gtfs <- read_gtfs("database/transit/AML_gtfs.zip")
    funchal_gtfs <- read_gtfs("database/transit/funchal_gtfs.zip")
    cascais_gtfs <- read_gtfs("database/transit/cascais_gtfs.zip")
    barreiro_gtfs <- read_gtfs("database/transit/barreiro_gtfs.zip")
    agueda_gtfs <- read_gtfs("database/transit/agueda_gtfs.zip")
    porto_gtfs <- read_gtfs("database/transit/porto_gtfs.zip")
    
# Organize the databases
    
    # Select and filter databases by a representative date (Wednesday)
    
    braga_4Planning_date <- filter_feed_by_date(braga_4Planning_gtfs,"2024-04-03")
    lisbon_date <- filter_feed_by_date(lisbon_gtfs, "2024-04-10")
    aml_date <- filter_feed_by_date(aml_gtfs, "2024-04-10")
    funchal_date <- filter_feed_by_date(funchal_gtfs, "2024-04-10")
    cascais_date <- filter_feed_by_date(cascais_gtfs, "2024-04-10")
    barreiro_date <- filter_feed_by_date(barreiro_gtfs, "2019-04-10")
    agueda_date <- filter_feed_by_date(agueda_gtfs, "2019-04-10")
    porto_date <- filter_feed_by_date(porto_gtfs, "2022-11-09")
    
    #Organize the table calculating the frequencies per bus stop
    
#### Braga 4-Planning
    
      #Get stop frequency (missing data)
    
    braga_4Planning_f = data.frame()
    
    for (i in 6:9){
      braga_4Planning <- get_stop_frequency(braga_4Planning_date,
                                                    start_time = paste0("0",i,":00:00"),
                                                    end_time = paste0("0",i,":59:59"),
                                                    service_ids = NULL,
                                                    by_route = TRUE) 
      
      braga_4Planning <- braga_4Planning |> 
             group_by(stop_id) |> 
              summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      braga_4Planning_f = rbind(braga_4Planning_f,braga_4Planning)
    }
    
    for (i in 10:23){
      braga_4Planning <- get_stop_frequency(braga_4Planning_date,
                                            start_time = paste0(i,":00:00"),
                                            end_time = paste0(i,":59:59"),
                                            service_ids = NULL,
                                            by_route = TRUE) 
      braga_4Planning <- braga_4Planning |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      braga_4Planning_f = rbind(braga_4Planning_f,braga_4Planning)
      }
      
      
    braga_4Planning_frequency <- braga_4Planning_f |> 
        ungroup() |> 
        group_by(stop_id,hour) |> 
        summarise(frequency = sum(frequency)) |> 
        ungroup()
      
    
    braga_4Planning_table <- braga_4Planning_frequency |> 
      left_join(braga_4Planning_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
   
    mapview::mapview(braga_4Planning_table) 
    
    
#### Lisbon
    
    #Get stop frequency (missing data)
    
    lisbon_f = data.frame()
    
    for (i in 6:9){
      lisbon <- get_stop_frequency(lisbon_date,
                                            start_time = paste0("0",i,":00:00"),
                                            end_time = paste0("0",i,":59:59"),
                                            service_ids = NULL,
                                            by_route = TRUE) 
      
      lisbon <- lisbon |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      lisbon_f = rbind(lisbon_f, lisbon)
    }
    
    for (i in 10:23) {
      lisbon <- get_stop_frequency(lisbon_date,
                                              start_time = paste0(i,":00:00"),
                                              end_time = paste0(i,":59:59"),
                                              service_ids = NULL,
                                              by_route = TRUE)
      
      lisbon <- lisbon |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      lisbon_f =rbind(lisbon_f, lisbon)
    }
    
    
    lisbon_frequency <- lisbon_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    lisbon_table <- lisbon_frequency |> 
      left_join(lisbon_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(lisbon_table)     

    
        
#### AML
    
    #Get stop frequency (missing data)
    
    aml_f = data.frame()
    
    for (i in 6:9){
      aml <- get_stop_frequency(aml_date,
                                   start_time = paste0("0",i,":00:00"),
                                   end_time = paste0("0",i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE) 
      
      aml <- aml |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      aml_f = rbind(aml_f, lisbon)
    }
    
    for (i in 10:23) {
      aml <- get_stop_frequency(aml_date,
                                   start_time = paste0(i,":00:00"),
                                   end_time = paste0(i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE)
      
      aml <- aml |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      aml_f =rbind(aml_f, aml)
    }
    
    
    aml_frequency <- aml_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    aml_table <- aml_frequency |> 
      left_join(aml_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(aml_table)         
    
#### Funchal
    
    #Get stop frequency (missing data)
    
    funchal_f = data.frame()
    
    for (i in 6:9){
      funchal <- get_stop_frequency(funchal_date,
                                start_time = paste0("0",i,":00:00"),
                                end_time = paste0("0",i,":59:59"),
                                service_ids = NULL,
                                by_route = TRUE) 
      
      funchal <- funchal |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      funchal_f = rbind(funchal_f, funchal)
    }
    
    for (i in 10:23) {
      funchal <- get_stop_frequency(funchal_date,
                                start_time = paste0(i,":00:00"),
                                end_time = paste0(i,":59:59"),
                                service_ids = NULL,
                                by_route = TRUE)
      
      funchal <- funchal |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      funchal_f =rbind(funchal_f, funchal)
    }
    
    
    funchal_frequency <- funchal_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    funchal_table <- funchal_frequency |> 
      left_join(funchal_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(funchal_table)      
    

#### Cascais
    
    #Get stop frequency (missing data)
    
    cascais_f = data.frame()
    
    for (i in 6:9){
      cascais <- get_stop_frequency(cascais_date,
                                    start_time = paste0("0",i,":00:00"),
                                    end_time = paste0("0",i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE) 
      
      cascais <- cascais |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      cascais_f = rbind(cascais_f, cascais)
    }
    
    for (i in 10:23) {
      cascais <- get_stop_frequency(cascais_date,
                                    start_time = paste0(i,":00:00"),
                                    end_time = paste0(i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE)
      
      cascais <- cascais |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      cascais_f =rbind(cascais_f, cascais)
    }
    
    
    cascais_frequency <- cascais_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    cascais_table <- cascais_frequency |> 
      left_join(cascais_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(cascais_table)      
    

#### Barreiro
    
    #Get stop frequency (missing data)
    
    barreiro_f = data.frame()
    
    for (i in 6:9){
      barreiro <- get_stop_frequency(barreiro_date,
                                    start_time = paste0("0",i,":00:00"),
                                    end_time = paste0("0",i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE) 
      
      barreiro <- barreiro |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      barreiro_f = rbind(barreiro_f, barreiro)
    }
    
    for (i in 10:23) {
      barreiro <- get_stop_frequency(barreiro_date,
                                    start_time = paste0(i,":00:00"),
                                    end_time = paste0(i,":59:59"),
                                    service_ids = NULL,
                                    by_route = TRUE)
      
      barreiro <- barreiro |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      barreiro_f =rbind(barreiro_f, barreiro)
    }
    
    
    barreiro_frequency <- barreiro_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    barreiro_table <- barreiro_frequency |> 
      left_join(barreiro_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(barreiro_table)      
    

    
#### Agueda
    
    #Get stop frequency (missing data)
    
    agueda_f = data.frame()
    
    for (i in 6:9){
      agueda <- get_stop_frequency(agueda_date,
                                     start_time = paste0("0",i,":00:00"),
                                     end_time = paste0("0",i,":59:59"),
                                     service_ids = NULL,
                                     by_route = TRUE) 
      
      agueda <- agueda |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      agueda_f = rbind(agueda_f, agueda)
    }
    
    for (i in 10:23) {
      agueda <- get_stop_frequency(agueda_date,
                                     start_time = paste0(i,":00:00"),
                                     end_time = paste0(i,":59:59"),
                                     service_ids = NULL,
                                     by_route = TRUE)
      
      agueda <- agueda |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      agueda_f =rbind(agueda_f, agueda)
    }
    
    
    agueda_frequency <- agueda_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    agueda_table <- agueda_frequency |> 
      left_join(agueda_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(agueda_table)      
    

#### Porto
    
    #Get stop frequency (missing data)
    
    porto_f = data.frame()
    
    for (i in 6:9){
      porto <- get_stop_frequency(porto_date,
                                   start_time = paste0("0",i,":00:00"),
                                   end_time = paste0("0",i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE) 
      
      porto <- porto |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      agueda_f = rbind(porto_f, porto)
    }
    
    for (i in 10:23) {
      porto <- get_stop_frequency(porto_date,
                                   start_time = paste0(i,":00:00"),
                                   end_time = paste0(i,":59:59"),
                                   service_ids = NULL,
                                   by_route = TRUE)
      
      porto <- porto |> 
        group_by(stop_id) |> 
        summarise(frequency = sum(n_departures)) |> 
        mutate(hour=i)
      porto_f =rbind(porto_f, porto)
    }
    
    
    porto_frequency <- porto_f |> 
      ungroup() |> 
      group_by(stop_id,hour) |> 
      summarise(frequency = sum(frequency)) |> 
      ungroup()
    
    
    porto_table <- porto_frequency |> 
      left_join(porto_date$stops |> 
                  select(stop_id,stop_lon,stop_lat), by = "stop_id") |> 
      st_as_sf(crs=4326, coords = c("stop_lon","stop_lat"))
    
    mapview::mapview(porto_table)      
    
    
updat 
    
    
    
    

    
   