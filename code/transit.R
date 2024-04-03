# aim: get daily frequency of transit modes in the area


# Automatically download GTFS data---------------------------------------------------

  # Braga 
  
    #Operador: 4 Planning
  
    Braga_gtfs_zip = "https://gtfs.pro/files/uran/improved-gtfs-braga.zip" # 1.6MB 
    download.file(Braga_gtfs_zip, destfile = "database/transit/braga_gtfs_4planning.zip")
    
    #Operador: Transporlis
    
    Braga_gtfs_2_zip = "https://gtfs.pro/files/uran/improved-gtfs-braga.zip" # 1.6MB 
    download.file(Braga_gtfs_2_zip, destfile = "database/transit/braga_gtfs_transporlis.zip")
    

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

    library(gtfstools)
    
# read gtfs files
    
    braga_4Planning_gtfs <- read_gtfs("database/transit/braga_gtfs_4planning.zip")
    braga_transporlis_gtfs <- read_gtfs("database/transit/braga_gtfs_transporlis.zip")
    lisbon_gtfs <- read_gtfs("database/transit/lisbon_gtfs.zip")
    aml_gtfs <- read_gtfs("database/transit/AML_gtfs.zip")
    funchal_gtfs <- read_gtfs("database/transit/funchal_gtfs.zip")
    cascais_gtfs <- read_gtfs("database/transit/cascais_gtfs.zip")
    barreiro_gtfs <- read_gtfs("database/transit/barreiro_gtfs.zip")
    agueda_gtfs <- read_gtfs("database/transit/agueda_gtfs.zip")
    porto_gtfs <- read_gtfs("database/transit/porto_gtfs.zip")
    
# Organize the databases 
    
    #Import Libraries
    library(sf)
    library(readr)
    library(tidyverse)
    library(lubridate)
    library(tidy)

    
    
    #Join stop_id with stop_times
    
    lisbon_service <- lisbon_gtfs$ %>%
      left_join(lisbon_gtfs$stop_times, by="stop_id")
    
    
    
    Braga_stops_4_planning <- braga_4Planning_gtfs$stops %>%
      left_join(braga_4Planning_gtfs$stop_times, by="stop_id")
    
    Braga_dates_4_planning <- Braga_stops_4_planning$trips %>%
      left_join(Braga_stops_4_planning$calendar_dates, by="service_id")
    
    
    Braga_agencys_transporlis <- braga_transporlis_gtfs$agency %>%
      left_join(braga_transporlis_gtfs$calendar_dates, by="dates")
    
    lisbon_stops <- lisbon_gtfs$stops %>%
      left_join(lisbon_gtfs$stop_times, by="stop_id")
    
    aml_stops <- aml_gtfs$stops %>%
      left_join(aml_gtfs$stop_times, by="stop_id")
    
    funchal_stops <- funchal_gtfs$stops %>%
      left_join(funchal_gtfs$stop_times, by="stop_id")
    
    cascais_stops <- cascais_gtfs$stops %>%
      left_join(cascais_gtfs$stop_times, by="stop_id")
    
    barreiro_stops <- barreiro_gtfs$stops %>%
      left_join(barreiro_gtfs$stop_times, by="stop_id")
    
    agueda_stops <- agueda_gtfs$stops %>%
      left_join(agueda_gtfs$stop_times, by="stop_id")
    
    porto_stops <- porto_gtfs$stops %>%
      left_join(porto_gtfs$stop_times, by="stop_id")
    
    # Join the frequencies
    
    
    
    
    
    
    
    
    
    


gtfs de todos as operadoras a nível nacional
filtrar as que são underground
validar
verificar data comum /(senão traspor)
gtfs merge


# calcular frequencia por hora por paragem --------------------------------

ficar com o valor máximo horário num dia típico


# procurar paragens no poligono -------------------------------------------

soma da média de frequencia de todas as paragens dentro do poligono


# classificacao -----------------------------------------------------------

ver o ranking do artigo (NACTO) e classificar a grid


