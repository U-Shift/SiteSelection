
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

CAOP_municipios = st_read("database/CAOP_municipios.gpkg")

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

library(tidyverse)
library(osmdata)
library(sf)
CITY = "Almada"
CITYlimit = CAOP_municipios |> filter(Concelho == CITY)

## new way using osmdata ##
osm_points_amenity = opq("Portugal") |> 
  add_osm_feature(key = "amenity", value = c("atm", "bank", "hospital", "pharmacy", "veterinary",
                                             "restaurant", "pub", "cafe", "bar",
                                             "college", "university", "kindergarten", "school",
                                             "library", "cinema", "theatre", "place_of_worship",
                                             "police", "fire_station", "courthouse", "post_office")) |>
  osmdata_sf()
# nrow(osm_points_amenity$osm_points)
# nrow(osm_points_amenity$osm_polygons)
point_amenity = osm_points_amenity$osm_points |> filter(amenity %in%  c("atm", "bank", "hospital", "pharmacy", "veterinary",
                                                                        "restaurant", "pub", "cafe", "bar",
                                                                        "college", "university", "kindergarten", "school",
                                                                        "library", "cinema", "theatre", "place_of_worship",
                                                                        "police", "fire_station", "courthouse", "post_office"))

centroid_amenity = osm_points_amenity$osm_polygons |> st_centroid()
point_amenity = point_amenity |> bind_rows(centroid_amenity)
# mapview::# mapview(point_amenity)
# table(point_amenity$amenity)

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
osm_points_shop = opq("Portugal") |> 
  add_osm_feature(key = "shop") |>
  osmdata_sf()
point_shop = osm_points_shop$osm_points
centroid_shop = osm_points_shop$osm_polygons |> st_centroid()
point_shop = point_shop |> bind_rows(centroid_shop)
# mapview::mapview(point_shop)
# table(point_shop$shop)
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
# mapview::mapview(point_shop_clean, zcol="shop")
# table(point_shop_clean$shop)
rm(shop_NA, shop_distinct)
rm(osm_points_shop)

## healthcare
osm_points_healthcare = opq("Portugal") |> 
  add_osm_feature(key = "healthcare") |>
  osmdata_sf()
point_healthcare = osm_points_healthcare$osm_points
centroid_healthcare = osm_points_healthcare$osm_polygons |> st_centroid()
# mapview::mapview(point_healthcare) + mapview::mapview(centroid_healthcare, col.regions = "red")
point_healthcare = point_healthcare |> bind_rows(centroid_healthcare)
# table(point_healthcare$healthcare)
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
# mapview::mapview(point_healthcare_clean, zcol = "healthcare")
rm(healthcare_NA, healthcare_distinct)
point_healthcare_clean = point_healthcare_clean |> filter(!is.na(healthcare)) # remove the ones without healthcare

## sport
osm_points_sport = opq("Portugal") |> 
  add_osm_feature(key = "sport") |>
  osmdata_sf()
point_sport = osm_points_sport$osm_points
centroid_sport = osm_points_sport$osm_polygons |> filter(!is.na(leisure)) |>  st_centroid()
point_sport = point_sport |> bind_rows(centroid_sport)
# mapview::mapview(point_sport[!is.na(point_sport$sport),], col.regions = "red") + mapview::mapview(point_sport[is.na(point_sport$sport),], col.regions = "blue")
# table(point_sport$sport)
# select few columns
point_sport = point_sport |>
  select(osm_id, leisure, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |>
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`,
         sport = leisure)
# remove the ones with the same address and housenumber and sport
sport_NA = point_sport |> 
  filter(is.na(sport) | is.na(address) | is.na(housenumber))
sport_distinct = point_sport |> 
  distinct(sport, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(sport) & !is.na(address) & !is.na(housenumber))
point_sport_clean = rbind(sport_NA, sport_distinct) |> filter(!is.na(sport)) # remove the ones without sport
# mapview::mapview(point_sport_clean, zcol = "sport")
# table(point_sport_clean$sport)
rm(sport_NA, sport_distinct)

## leisure
osm_points_leisure = opq("Portugal") |> 
  add_osm_feature(key = "leisure", value = c("park", "playground", "dog_park")) |>
  osmdata_sf()
point_leisure = osm_points_leisure$osm_points
centroid_leisure = osm_points_leisure$osm_polygons |> st_centroid()
point_leisure = point_leisure |> bind_rows(centroid_leisure)
# mapview::mapview(point_leisure[!is.na(point_leisure$leisure),], col.regions = "red") + mapview::mapview(point_leisure[is.na(point_leisure$leisure),], col.regions = "blue")
# table(point_leisure$leisure)
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
# mapview::mapview(point_leisure_clean, zcol = "leisure")
# table(point_leisure_clean$leisure)
rm(leisure_NA, leisure_distinct)


## tourism
tourism_values = c("motel", "viewpoint", "guest_house", "museum", "hotel", "hostel",
                   "attraction", "gallery")
osm_points_tourism = opq("Portugal") |> 
  add_osm_feature(key = "tourism", value = tourism_values) |>
  osmdata_sf()
point_tourism = osm_points_tourism$osm_points |> filter(!is.na(tourism) | !is.na(name))
centroid_tourism = osm_points_tourism$osm_polygons |> st_centroid()
point_tourism = point_tourism |> bind_rows(centroid_tourism)
# mapview::mapview(point_tourism[!is.na(point_tourism$tourism),], col.regions = "red") + mapview::mapview(point_tourism[is.na(point_tourism$tourism),], col.regions = "blue")
# table(point_tourism$tourism)
# select few columns
point_tourism = point_tourism |>
  select(osm_id, tourism, name, `addr:city`, `addr:street`, `addr:housenumber`, `addr:postcode`, geometry) |> 
  mutate(CITY = CITY) |> 
  rename(address = `addr:street`,
         housenumber = `addr:housenumber`,
         location = `addr:city`,
         postcode = `addr:postcode`)
# remove the ones with the same address and housenumber and tourism
tourism_NA = point_tourism |> 
  filter(is.na(tourism) | is.na(address) | is.na(housenumber))
tourism_distinct = point_tourism |> 
  distinct(tourism, address, housenumber, .keep_all = TRUE) |> 
  filter(!is.na(tourism) & !is.na(address) & !is.na(housenumber))
point_tourism_clean = rbind(tourism_NA, tourism_distinct) |> filter(!is.na(tourism)) # remove the ones without tourism
# mapview::mapview(point_tourism_clean, zcol = "tourism")
# table(point_tourism_clean$tourism)
rm(tourism_NA, tourism_distinct)


rm(osm_points_tourism, osm_points_sport, osm_points_leisure, osm_points_healthcare, osm_points_amenity)


points_all = rbind(point_amenity_clean |> rename(type = amenity) |> mutate(group = "amenity"),
                   point_shop_clean |> rename(type = shop) |> mutate(group = "shop"),
                   point_healthcare_clean |> rename(type = healthcare) |> mutate(group = "healthcare"),
                   point_sport_clean |> rename(type = sport) |> mutate(group = "sport"),
                   point_leisure_clean |> rename(type = leisure) |> mutate(group = "leisure"),
                   # point_building_clean |> rename(type = building),
                   point_tourism_clean |> rename(type = tourism) |> mutate(group = "tourism")
                   ) |> 
  select(-CITY) |> 
  distinct(osm_id, .keep_all = TRUE) 

points_all_portugal = points_all[CAOP_municipios,]

table(points_all_portugal$type)
points_all_tags = points_all_portugal |>
  st_drop_geometry() |> 
  group_by(group, type) |> 
  summarise(count = n())

write.table(points_all_tags, "database/osm_poi_tags.txt", sep = "\t", row.names = FALSE)
piggyback::pb_upload("database/osm_poi_tags.txt")


# Get for all country (Portugal), and filter with 
# points_all_city = points_all[CITYlimit,]
# ?
# It can be a good approach!


##### NEEDS TO RUN INDIVIDUALY FOR EACH CITY ### Too much information, won't pass
## building
building_values = c("government", "hospital", "firestation", "museum",
                    # "transportation", "university",
                     "stadium", "hotel")
# remove university?
# remove transportation?


osm_points_building = opq(CITY) |> 
  add_osm_feature(key = "building", value = building_values) |>
  osmdata_sf()
point_building = osm_points_building$osm_points |> filter(!is.na(name))
centroid_building = osm_points_building$osm_polygons |> st_centroid()
point_building = point_building |> bind_rows(centroid_building)
# mapview::mapview(point_building[!is.na(point_building$building),], col.regions = "red") + mapview::mapview(point_building[is.na(point_building$building),], col.regions = "blue")
# table(point_building$building)
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
# table(point_building_clean$building)
rm(building_NA, building_distinct)
rm(osm_points_building)

############################## CONTINUAR AQUI ----------------------------------------------------------

## transportation hubs
osm_points_public_transport = opq(CITY) |> 
  add_osm_feature(key = "public_transport") |>
  osmdata_sf()
point_public_transport = osm_points_public_transport$osm_points |> filter(!is.na(public_transport) | !is.na(name))
centroid_public_transport = osm_points_public_transport$osm_polygons |> st_centroid()
point_public_transport = point_public_transport |> bind_rows(centroid_public_transport)
mapview::mapview(point_public_transport, zcol = "public_transport")
table(point_public_transport$public_transport)
# select few columns
point_public_transport = point_public_transport |>
  select(osm_id, public_transport, network, name, highway, railway, bus, ferry, train, subway, light_rail, tram, geometry) |> 
  mutate(CITY = CITY)
point_public_transport_clean = point_public_transport |>  #bus != "yes"? |> 
  mutate(remover = ifelse(public_transport == "stop_position" & bus == "yes", "sim", "nao"))
point_public_transport_clean$remover[is.na(point_public_transport_clean$remover)] = "nao"
mapview::mapview(point_public_transport_clean, zcol = "public_transport")
mapview::mapview(point_public_transport_clean, zcol = "network")
mapview::mapview(point_public_transport_clean, zcol = "remover")
point_public_transport_clean = point_public_transport_clean |> filter(remover == "nao")
point_public_transport_clean_mode = point_public_transport_clean |> 
  pivot_longer(cols = c("bus", "ferry", "train", "subway", "light_rail", "tram"), names_to = "mode", values_to = "sure") |> 
  filter(sure == "yes") |> 
  select(osm_id, mode, public_transport, highway, railway, network, name, geometry, CITY)
mapview::mapview(point_public_transport_clean_mode, zcol = "mode")
table(point_public_transport_clean_mode$mode)
rm(osm_points_public_transport)
# ainda não está perfeito, há alguns duplicados com o stop_position e station. Mas no caso do comboio da praia não.
