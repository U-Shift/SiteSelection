# aim: get traffic conditions along the day, with vector!



library(dplyr)
library(sf)
library(mapboxapi)

Sys.getenv("MAPBOX_PUBLIC_TOKEN") # exists!
get_mb_access_token()

# Get data in vector format
# CITY = "Lisboa"
# MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg") # Portugal
# CITYlimit = MUNICIPIOSgeo %>% filter(Concelho == CITY)
# saveRDS(CITYlimit, "database/lisbon_limit.Rds")
CITYlimit = readRDS("database/lisbon_limit.Rds")

CELL_candidates = site_selection

traffic_vector = get_vector_tiles(
  tileset_id = "mapbox.mapbox-traffic-v1",
  location = CELL_candidates,
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


# with a loop -------------------------------------------------------------


CELL_candidates = site_selection
traffic_vector_total = traffic_vector_i[1,] # pre-run with 1 case

for (i in c(1:nrow(CELL_candidates))){
  traffic_vector = get_vector_tiles(
    tileset_id = "mapbox.mapbox-traffic-v1",
    location = CELL_candidates[i,],
    zoom = 17
  )
  
  traffic_vector_i = traffic_vector$traffic
  
    if("structure" %in% colnames(traffic_vector_i) == TRUE){
      traffic_vector_i = traffic_vector_i |> filter(!structure %in% c("tunnel", "bridge"))
    }else{
      traffic_vector_i = traffic_vector_i |> mutate(structure = NA)
          }
  
    traffic_vector_i = traffic_vector_i |>
      select(congestion, geometry)
  traffic_vector_total = bind_rows(traffic_vector_total, traffic_vector_i) |>
    unique()
  
}

traffic_vector_total = traffic_vector_total[-1,]
traffic_vector_total$congestion = factor(traffic_vector_total$congestion,
                                      levels = c("low", "moderate", "heavy", "severe"),
                                      ordered = TRUE)
  
  #Visualise
  mapview::mapview(traffic_vector_total, zcol="congestion") + mapview::mapview(CELL_candidates)
  
  
  # crop
  traffic_vector_total_cropped = st_intersection(traffic_vector_total, CELL_candidates |> select(ID))
  
  # How to classify if a cell presents congestion?
  traffic_vector_total_cropped = traffic_vector_total_cropped |> 
    mutate(length = st_length(geometry) |> units::drop_units())


  traffic_vector_total_cropped_cell_length = traffic_vector_total_cropped |> 
    st_drop_geometry() |>
    group_by(ID) |> 
    summarise(length = sum(length)) |> 
    ungroup()
  
  
  # classify if a cell presents congestion (heavy or severe) in more than 10% of total length
  traffic_vector_total_cropped_classified = traffic_vector_total_cropped |>
    st_drop_geometry() |>
    filter(congestion %in% c("heavy", "severe")) |>
    group_by(ID) |>
    summarise(length = sum(length)) |>
    left_join(traffic_vector_total_cropped_cell_length, by = "ID") |>
    mutate(classification = ifelse(length.x/length.y > 0.1, "congested", "free")) # 10% ?
  
  # get only the cells that are congested
  traffic_vector_total_cropped_congested = traffic_vector_total_cropped_classified |>
    select(ID, classification)
  
  CELL_candidates = CELL_candidates |>
    left_join(traffic_vector_total_cropped_congested, by = "ID")
    
  # vizualise
  mapview::mapview(CELL_candidates, zcol="classification", alpha.regions = 0.2) +
    mapview::mapview(traffic_vector_total, zcol="congestion")
 
