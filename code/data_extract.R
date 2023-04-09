
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

CITY = "Lisboa"
BBOX = st_as_sfc(st_bbox(CITYlimit)) #see grid.R

# Extract the OSM network from geofabrik
lisbon_osm = oe_get(CITY, # donwload the match (Lisbon will dwonload wtinre Portugal)
                    boundary = BBOX, # crop the results only to the city limit
                    provider = "geofabrik",
                    stringsAsFactors = FALSE,
                    quiet = FALSE,
                    force_download = TRUE,
                    force_vectortranslate = TRUE, # as shp
                    download_directory = "database/"
                    ) #218 MB! Apr2022

# filter some unwanted road links
lisbon_network = lisbon_osm %>% 
  dplyr::filter(highway %in% c('primary', "primary_link", 'secondary',"secondary_link", 'tertiary', "tertiary_link", 
                               "trunk", "trunk_link", "residential", "cycleway", "living_street", "unclassified", 
                               "motorway", "motorway_link", "pedestrian", "steps", "track")) #remove: "service",

# crop to city limits, with a buffer of 100m
lisbon_network = st_intersection(lisbon_network, geo_buffer(CITYlimit, dist=100)) 

plot(lisbon_network["highway"])

# # Clean the road network ? maybe not necessary
# library(stplanr)
# lisbon_network$group = stplanr::rnet_group(lisbon_network)
# plot(lisbon_network["group"])
# lisbon_network_clean = lisbon_network %>% filter(group == 1) #the network with more connected segments

st_write(lisbon_network, "database/lisbon_network.gpkg")

# upload to Assets
piggyback::pb_upload("database/lisbon_network.gpkg")# not working, upload manually




# download GTFS -------------------------------------------------------------------------------


