
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
CITY = "Almada"

## new way using osmdata ##
osm_points_amenity = opq(CITY) |> 
  add_osm_feature(key = "amenity", value = c("atm", "bank", "hospital", "pharmacy", "veterinary",
                                             "restaurant", "pub", "cafe", "bar",
                                             "college", "university", "kindergarten", "school",
                                             "library", "cinema", "theatre",
                                             "police", "fire_station", "courthouse", "post_office")) |>
  osmdata_sf()
nrow(osm_points_amenity$osm_points)
nrow(osm_points_amenity$osm_polygons)
point_amenity = osm_points_amenity$osm_points |> filter(amenity %in%  c("atm", "bank", "hospital", "pharmacy", "veterinary",
                                                                        "restaurant", "pub", "cafe", "bar",
                                                                        "college", "university", "kindergarten", "school",
                                                                        "library", "cinema", "theatre",
                                                                        "police", "fire_station", "courthouse", "post_office"))

centroid_amenity = osm_points_amenity$osm_polygons |> st_centroid()
point_amenity = point_amenity |> bind_rows(centroid_amenity)
mapview::mapview(point_amenity)
table(point_amenity$amenity)

# select few columns
point_amenity = point_amenity |>
  select(osm_id, amenity, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)

# remove the ones with the same address and housenumber and amenity
amenity_NA = point_amenity |> 
  filter(is.na(amenity) | is.na(address) | is.na(housenumber))
amenity_distinct = point_amenity |> 
  distinct(amenity, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(amenity) & !is.na(address) & !is.na(housenumber))
point_amenity_clean = rbind(amenity_NA, amenity_distinct)  
# verify
# point_amenity_toomuch = anti_join(point_amenity |> st_drop_geometry(), point_amenity_clean |> st_drop_geometry())
rm(amenity_NA, amenity_distinct)

## shops
osm_points_shop = opq(CITY) |> 
  add_osm_feature(key = "shop") |>
  osmdata_sf()
point_shop = osm_points_shop$osm_points
centroid_shop = osm_points_shop$osm_polygons |> st_centroid()
point_shop = point_shop |> bind_rows(centroid_shop)
mapview::mapview(point_shop)
table(point_shop$shop)
# select few columns
point_shop = point_shop |>
  select(osm_id, shop, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)
# remove the ones with the same address and housenumber and shop
shop_NA = point_shop |> 
  filter(is.na(shop) | is.na(address) | is.na(housenumber))
shop_distinct = point_shop |> 
  distinct(shop, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(shop) & !is.na(address) & !is.na(housenumber))
point_shop_clean = rbind(shop_NA, shop_distinct)  
point_shop_clean = point_shop_clean |> filter(!is.na(shop)) # remove the ones without healthcare
mapview::mapview(point_shop_clean)
rm(shop_NA, shop_distinct)

## healthcare
osm_points_healthcare = opq(CITY) |> 
  add_osm_feature(key = "healthcare") |>
  osmdata_sf()
point_healthcare = osm_points_healthcare$osm_points
centroid_healthcare = osm_points_healthcare$osm_polygons |> st_centroid()
mapview::mapview(point_healthcare) + mapview::mapview(centroid_healthcare, col.regions = "red")
point_healthcare = point_healthcare |> bind_rows(centroid_healthcare)
table(point_healthcare$healthcare)
# select few columns
point_healthcare = point_healthcare |>
  select(osm_id, healthcare, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)
# remove the ones with the same address and housenumber and healthcare
healthcare_NA = point_healthcare |> 
  filter(is.na(healthcare) | is.na(address) | is.na(housenumber))
healthcare_distinct = point_healthcare |> 
  distinct(healthcare, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(healthcare) & !is.na(address) & !is.na(housenumber))
point_healthcare_clean = rbind(healthcare_NA, healthcare_distinct)  
mapview::mapview(point_healthcare_clean, zcol = "healthcare")
rm(healthcare_NA, healthcare_distinct)
point_healthcare_clean = point_healthcare_clean |> filter(!is.na(healthcare)) # remove the ones without healthcare

## sport
osm_points_sport = opq(CITY) |> 
  add_osm_feature(key = "sport") |>
  osmdata_sf()
point_sport = osm_points_sport$osm_points
centroid_sport = osm_points_sport$osm_polygons |> st_centroid()
point_sport = point_sport |> bind_rows(centroid_sport)
mapview::mapview(point_sport[!is.na(point_sport$sport),], col.regions = "red") + mapview::mapview(point_sport[is.na(point_sport$sport),], col.regions = "blue")
table(point_sport$sport)
# select few columns
point_sport = point_sport |>
  select(osm_id, sport, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)
# remove the ones with the same address and housenumber and sport
sport_NA = point_sport |> 
  filter(is.na(sport) | is.na(address) | is.na(housenumber))
sport_distinct = point_sport |> 
  distinct(sport, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(sport) & !is.na(address) & !is.na(housenumber))
point_sport_clean = rbind(sport_NA, sport_distinct) |> filter(!is.na(sport)) # remove the ones without sport
mapview::mapview(point_sport_clean, zcol = "sport")
table(point_sport_clean$sport)
rm(sport_NA, sport_distinct)

## leisure
osm_points_leisure = opq(CITY) |> 
  add_osm_feature(key = "leisure", value = c("park", "playground", "dog_park")) |>
  osmdata_sf()
point_leisure = osm_points_leisure$osm_points
centroid_leisure = osm_points_leisure$osm_polygons |> st_centroid()
point_leisure = point_leisure |> bind_rows(centroid_leisure)
mapview::mapview(point_leisure[!is.na(point_leisure$leisure),], col.regions = "red") + mapview::mapview(point_leisure[is.na(point_leisure$leisure),], col.regions = "blue")
table(point_leisure$leisure)
# select few columns
point_leisure = point_leisure |>
  select(osm_id, leisure, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)
# remove the ones with the same address and housenumber and leisure
leisure_NA = point_leisure |> 
  filter(is.na(leisure) | is.na(address) | is.na(housenumber))
leisure_distinct = point_leisure |> 
  distinct(leisure, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(leisure) & !is.na(address) & !is.na(housenumber))
point_leisure_clean = rbind(leisure_NA, leisure_distinct) |> filter(!is.na(leisure)) # remove the ones without leisure
mapview::mapview(point_leisure_clean, zcol = "leisure")
table(point_leisure_clean$leisure)
rm(leisure_NA, leisure_distinct)

## building
bulding_values = c("hotel", "religious", "cathedral", "chapel", "church", "synagogue", "temple",
                   "government", "hospital", "firestation", "museum", "transportation", "university",
                   "stadium")
# remove university?
# remove transportation?


osm_points_building = opq(CITY) |> 
  add_osm_feature(key = "building", value = bulding_values) |>
  osmdata_sf()
point_building = osm_points_building$osm_points
centroid_building = osm_points_building$osm_polygons |> st_centroid()
point_building = point_building |> bind_rows(centroid_building)
mapview::mapview(point_building[!is.na(point_building$building),], col.regions = "red") + mapview::mapview(point_building[is.na(point_building$building),], col.regions = "blue")
table(point_building$building)
# select few columns
point_building = point_building |>
  select(osm_id, building, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)
# remove the ones with the same address and housenumber and building
building_NA = point_building |> 
  filter(is.na(building) | is.na(address) | is.na(housenumber))
building_distinct = point_building |> 
  distinct(building, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(building) & !is.na(address) & !is.na(housenumber))
point_building_clean = rbind(building_NA, building_distinct) |> filter(!is.na(building)) # remove the ones without building
mapview::mapview(point_building_clean, zcol = "building")
table(point_building_clean$building)
rm(building_NA, building_distinct)


############################## CONTINUAR AQUI ----------------------------------------------------------

# tourism?
# transportation hubs
  
table(osm_points$osm_points$tourism)
table(osm_points$osm_polygons$tourism)

mapview::mapview(osm_points_amenity$osm_polygons) +
  mapview::mapview(osm_points_building$osm_polygons, col.regions = "red") +
  mapview::mapview(osm_points_shop$osm_polygons, col.regions = "darkgreen")






road_osm_test = road_osm_test %>% osm_poly2line()
road_osm_test = road_osm_test$osm_lines %>% select(osm_id, name, highway, geometry)
