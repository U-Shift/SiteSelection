
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

CITY = "Lisboa"
BBOX = st_as_sfc(st_bbox(CITYlimit)) #see grid.R

# Extract the OSM network from geofabrik
road_osm = oe_get(CITY, # donwload the match (Lisbon will dwonload wtinre Portugal)
                    boundary = BBOX, # crop the results only to the city limit
                    provider = "geofabrik",
                    stringsAsFactors = FALSE,
                    quiet = FALSE,
                    force_download = TRUE,
                    force_vectortranslate = TRUE, # as shp
                    download_directory = "database/"
                    ) #218 MB! May2023
st_write(road_osm, "database/geofabrik_portugal-latest.gpkg")

# filter some unwanted road links
road_osm = st_read("database/geofabrik_portugal-latest.gpkg")
road_network = road_osm %>% 
  dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                               'secondary',"secondary_link", "trunk", 'trunk_link',
                               "tertiary", "tertiary_link", "pedestrian",
                               "residential", "living_street", "unclassified", "service")
                | man_made %in% c("bridge")) #remove: cycleway

# crop to city limits, with a buffer of 100m
road_network = st_intersection(road_network, geo_buffer(CITYlimit, dist=100)) 
plot(road_network["highway"])

# Clean the road network
library(stplanr)
road_network$group = stplanr::rnet_group(road_network, d = 10) # 10m tolerance
# plot(lisbon_network["group"])
road_network_groups = road_network %>% filter(group == 1) #the network with more connected segments

road_network = road_osm %>% filter(osm_id %in% road_network_groups$osm_id) # get only the segments from the clean network

st_geometry(road_network) # Should be "LINESTRING"
# road_network = st_cast(road_network, "LINESTRING") #if you don't wnat to use the previous filter
road_network = stplanr::rnet_breakup_vertices(road_network) # break the segments internally, conserving the brunels.

road_network = road_network %>% select(osm_id, highway, geometry) # keep some variables

st_write(road_network, "database/lisbon_network.gpkg", delete_dsn = TRUE)



# CONTINUE IN QGIS ----------------------------------------------------------------------------

# READ VERY CAREFULLY
# open QGIS with Grass

# run plugin
# http://plugins.qgis.org/plugins/disconnected-islands/
#  
# select all that connect (select by attributes all  "networkGrp" =0) and eport selected features
# the trick is, when expoerting, do not export field "fid", and rename fid bellow to "fid_2",
# otherwise the clean prossess cannot save the output
# export as "network_groups-gpkg"
#
# v.clean
# select other hidden options: break, snap, rmdangle, rmdupl
#
# tolerance for each
# 0,0.00000100, 0.00000100, 0
#
# save output as "network_vclean.gpkg"

road_network = st_read("database/lisbon_network_vclean.gpkg")

# upload to Assets
piggyback::pb_upload("database/lisbon_network_vclean.gpkg")# not working, upload manually






# download GTFS -------------------------------------------------------------------------------


