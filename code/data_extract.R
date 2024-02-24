
# download municipalities of Portugal, to use as example --------------------------------------

library(dplyr)
library(sf)

CAOP_zip = "https://geo2.dgterritorio.gov.pt/caop/CAOP_Continente_2022-gpkg.zip" # 103 MB
download.file(CAOP_zip, destfile = "database/caop.zip")
unzip(zipfile = "database/caop.zip", exdir = "database/")
file.remove("database/caop.zip")

CAOP = st_read(list.files("database", pattern = "CAOP", full.names = TRUE)[1])

CAOP_municipios = CAOP %>% 
  st_transform(4326) %>% #put in regular coordinate system (not the PT official)
  group_by(Distrito, Concelho) %>% 
  summarise(geom = st_union(geom)) %>% 
  ungroup()
st_write(CAOP_municipios, "database/CAOP_municipios.gpkg")

# CAOP_freguesias = CAOP %>% 
#   st_transform(4326) %>% 
#   group_by(Distrito, Concelho, Freguesia) %>% 
#   summarise(geom = st_union(geom)) %>% 
#   ungroup()

# upload to Assets
piggyback::pb_upload("database/CAOP_municipios.gpkg") # not working, upload manually



# download OSM street network Portugal, as example --------------------------------------------

library(dplyr)
library(sf)
library(osmextract)
library(stplanr)
library(osmdata)

CITY = "Lisboa"
BBOX = st_as_sfc(st_bbox(CITYlimit)) #see grid.R

# # Extract the OSM network from geofabrik
road_osm = oe_get("Portugal", # donwload the match (Lisbon will download entire Portugal)
                    # boundary = BBOX, # crop the results only to the city limit
                    provider = "geofabrik",
                    stringsAsFactors = FALSE,
                    quiet = FALSE,
                    force_download = TRUE,
                    force_vectortranslate = TRUE, # as shp
                    download_directory = "database/"
                    ) #643 MB! June2023
st_write(road_osm, "database/geofabrik_portugal-latest.gpkg", delete_dsn = TRUE)


## new way using osmdata ##
library(osmdata)
road_osm_test = opq(BBOX) %>% 
  add_osm_feature(key = "highway") %>% 
  osmdata_sf()
road_osm_test = road_osm_test %>% osm_poly2line()
road_osm_test = road_osm_test$osm_lines %>% select(osm_id, name, highway, geometry)

# filter some unwanted road links
road_osm = st_read("database/geofabrik_portugal-latest.gpkg")
road_network = road_osm %>% 
  dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                               'secondary',"secondary_link", "trunk", 'trunk_link',
                               "tertiary", "tertiary_link", "pedestrian",
                               "residential", "living_street", "unclassified", "service")
                | man_made %in% c("bridge")) #remove: cycleway,

# crop to city limits, with a buffer of 100m
road_network = st_intersection(road_network, geo_buffer(CITYlimit, dist=100)) 
plot(road_network["highway"])




# download census population and buildings --------------------------------


library(tidyverse)
library(sf)

# get census data
download.file("https://mapas.ine.pt/download/filesGPG/2021/BGRI21_CONT.zip", "database/BGRI_CONT.zip")
unzip("database/BGRI_CONT.zip", "BGRI21_CONT.gpkg")
file.copy(from = "BGRI21_CONT.gpkg", to = "database/BGRI21_CONT.gpkg")
file.remove("BGRI21_CONT.gpkg", "database/BGRI_CONT.zip")

CENSUSraw = st_read("database/BGRI21_CONT.gpkg")

# make centroids and select only resident population and buildings (total and exclusively residential)
CENSUSpoint = CENSUSraw |> 
  select(BGRI2021, DTMN21, N_INDIVIDUOS, N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_EXCLUSIV_RESID) |>
  st_as_sf(crs = 4326)

# mapview::mapview(CENSUSpoint)

# label the DTMN with municipalities, to make filtering faster (instead of filter by city_limit)



st_write(CENSUSpoint, "database/CENSUSpoint.gpkg")

# upload to Assets
piggyback::pb_upload("database/CENSUSpoint.gpkg") 