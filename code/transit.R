# aim: get daily frequency of transit modes in the area


# Automatically download GTFS data---------------------------------------------------

  # Braga 
  
    #Operador: 4 Planning
  
    Braga_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-braga.zip" # 1.6MB 
    download.file(Braga_gtfs_zip, destfile = "database/transit/braga_gtfs_4planning.zip")
    
    #Operador: Transporlis
    
   # Braga_gtfs_2_zip = "https://gtfs.pro/files/uran/improved-gtfs-braga.zip" # 1.6MB 
   # download.file(Braga_gtfs_2_zip, destfile = "database/transit/braga_gtfs_transporlis.zip")
    

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
  #  braga_transporlis_gtfs <- read_gtfs("database/transit/braga_gtfs_transporlis.zip")
    lisbon_gtfs <- read_gtfs("database/transit/lisbon_gtfs.zip")
    aml_gtfs <- read_gtfs("database/transit/AML_gtfs.zip")
    funchal_gtfs <- read_gtfs("database/transit/funchal_gtfs.zip")
    cascais_gtfs <- read_gtfs("database/transit/cascais_gtfs.zip")
    barreiro_gtfs <- read_gtfs("database/transit/barreiro_gtfs.zip")
    agueda_gtfs <- read_gtfs("database/transit/agueda_gtfs.zip")
    porto_gtfs <- read_gtfs("database/transit/porto_gtfs.zip")
    
# Organize the databases
    
    # Select and filter databases by a representative date (Wednesday)

    #MODIFICAR DATAS
    
    braga_4Planning_date <- filter_feed_by_date(braga_4Planning_gtfs,"2024-04-03")
 #   braga_transporlis_date <- filter_feed_by_date(braga_transporlis_gtfs, "2024-03-10")
    lisbon_date <- filter_feed_by_date(lisbon_gtfs, "2024-04-10")
    
    
    aml_date <- filter_feed_by_date(aml_gtfs, "2024-03-10")
    funchal_date <- filter_feed_by_date(funchal_gtfs, "2024-03-10")
    cascais_date <- filter_feed_by_date(cascais_gtfs, "2024-03-10")
    barreiro_date <- filter_feed_by_date(barreiro_gtfs, "2024-03-10")
    agueda_date <- filter_feed_by_date(agueda_gtfs, "2019-03-13")
    porto_date <- filter_feed_by_date(porto_gtfs, "2022-03-09")
    
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
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    

    
   