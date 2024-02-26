
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
CENSUSpolygons = CENSUSraw |> 
  select(BGRI2021, DTMN21, N_INDIVIDUOS, N_EDIFICIOS_CLASSICOS, N_EDIFICIOS_EXCLUSIV_RESID) |>
  st_transform(4326)
# CENSUSpoints = CENSUSpoligons |> st_centroid(of_largest_polygon = TRUE) # error! parse code 12

library(qgisprocess)
# algorithms = qgis_algorithms()
# algorithms %>% filter(grepl(pattern = "centroid", x = algorithm, ignore.case = TRUE))
# qgis_show_help("native:centroids")

CENSUSpolygons %>%
  st_write("database/CENSUSpolygons.gpkg", delete_dsn = TRUE)
input = st_read("database/CENSUSpolygons.gpkg") #because of the fid column
output_path = paste0("database/CENSUSpoint.gpkg")

output = qgis_run_algorithm(
  algorithm = "native:centroids",
  INPUT = input,
  OUTPUT = output_path, # need to be defined otherwise it saves in tmp.gpkg and makes the error with fid (# ERROR 1: failed to execute insert : UNIQUE constraint failed: outpute935bd152d284569afb314c88e8fce09.fid)
)

CENSUSpoint = sf::st_read(output_path)
                            
# mapview::mapview(CENSUSpoint)
plot(CENSUSpoint[1,])

# label the DTMN with municipalities, to make filtering faster (instead of filter by city_limit)
# get names BGRI DTMN21
download.file("https://www.dgterritorio.gov.pt/sites/default/files/ficheiros-cartografia/InfExtra_Municipios_Freguesias_CAOP2023.zip",
              "database/InfExtra_Municipios_Freguesias_CAOP2023.zip")
unzip("database/InfExtra_Municipios_Freguesias_CAOP2023.zip", "InfExtra_Municípios_Freguesias_CAOP2023.xls")
file.copy(from = "InfExtra_Municípios_Freguesias_CAOP2023.xls", to = "database/InfExtra_Municípios_Freguesias_CAOP2023.xls")
file.remove("InfExtra_Municípios_Freguesias_CAOP2023.xls", "database/BGRI_names.zip")
# not extracting well..

BGRI_names = readxl::read_xls("database/InfExtra_Municípios_Freguesias_CAOP2023.xls", sheet = 1)
BGRI_names = BGRI_names |>
  select(DICO, Designação_Município) |> 
  rename(DTMN21 = DICO,
         Concelho = Designação_Município) # in caps...

#join names - in CAPS
CENSUSpoint = CENSUSpoint |> left_join(BGRI_names)

st_write(CENSUSpoint, "database/CENSUSpoint.gpkg", delete_dsn = TRUE)

# upload to Assets
piggyback::pb_upload("database/CENSUSpoint.gpkg", repo = "U-Shift/SiteSelection", tag = "0.1") 
file.remove("database/CENSUSpolygons.gpkg")



# download OSM buildings and services -------------------------------------

library(osmdata)
library(sf)
CITY = "Lisboa"

## new way using osmdata ##
osm_points_amenity = opq(CITY) |> 
  add_osm_feature(key = "amenity", value = c("atm", "bank", "hospital", "pharmacy", "veterinary",
                                             "restaurant", "pub", "cafe", "bar",
                                             "college", "university", "kindergarten", "school",
                                             "library", "cinema", "theatre",
                                             "police", "fire_station", "courthouse", "post_office")) |>
  osmdata_sf()
osm_points_building = opq(CITY) |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()
osm_points_shop = opq(CITY) |> 
  add_osm_feature(key = "shop") |>
  osmdata_sf()

bulding_values = c("apartments", "detached", "house", "residential",
            "hotel", "commercial", "office", "retail", "supermarket", "warehouse",
            "religious", "cathedral", "chapel", "church", "synagogue", "temple",
            "government", "hospital", "firestation", "museum", "school", "transportation", "university",
            "stadium")
  
  # add_osm_feature(key = "helthcare") |> #nothing
  # add_osm_feature(key = "sport") |> 
  # add_osm_feature(key = "tourism") |>
  # add_osm_feature(key = "leisure", value = c("park", "playground", "garden", "dog_park")) |> 
  osmdata_sf()
table(osm_points$osm_points$building)
table(osm_points$osm_polygons$building)
table(osm_points$osm_points$tourism)
table(osm_points$osm_polygons$tourism)

mapview::mapview(osm_points_amenity$osm_polygons) +
  mapview::mapview(osm_points_building$osm_polygons, col.regions = "red") +
  mapview::mapview(osm_points_shop$osm_polygons, col.regions = "darkgreen")

############################## CONTINUAR AQUI ----------------------------------------------------------




road_osm_test = road_osm_test %>% osm_poly2line()
road_osm_test = road_osm_test$osm_lines %>% select(osm_id, name, highway, geometry)
