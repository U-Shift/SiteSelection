# aim: get traffic conditions along the day, with vector!



library(dplyr)
library(sf)
library(mapboxapi)


# Get data in vector format
# CITY = "Lisboa"
# MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg") # Portugal
# CITYlimit = MUNICIPIOSgeo %>% filter(Concelho == CITY)
# saveRDS(CITYlimit, "database/lisbon_limit.Rds")
CITYlimit = readRDS("database/lisbon_limit.Rds")

traffic_vector = get_vector_tiles(
  tileset_id = "mapbox.mapbox-traffic-v1",
  location = CITYlimit,
  zoom = 14 #14
  )
traffic_vector = traffic_vector$traffic

traffic_vector$congestion = factor(traffic_vector$congestion,
                                      levels = c("low", "moderate", "heavy", "severe"),
                                      ordered = TRUE)

#Visualise
mapview::mapview(traffic_vector, zcol="congestion")


# Export 
st_write(traffic_vector, 
            dsn = paste0("database/traffic/vector/", format(as.POSIXct(Sys.time()), format = "%y%d%m_%H%M"), ".gpkg"),
            delete_dsn = TRUE)



