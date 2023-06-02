# OSM WITHOUT pedonal and service and motorway 
# v.clean + v.net.centrality

# remove bpol ??

library(dplyr)
library(sf)
library(osmextract)
library(stplanr)
library(scales)


road_osm = st_read("database/geofabrik_portugal-latest.gpkg")
road_network = road_osm %>% 
  dplyr::filter(highway %in% c('primary', "primary_link",
                               'secondary',"secondary_link", "trunk", 'trunk_link',
                               "tertiary", "tertiary_link", 
                               "residential", "living_street", "unclassified"))
#remove: cycleway, pedestrian, service, 'motorway',"motorway_link"

# crop to city limits, with a buffer of 100m
road_network = st_intersection(road_network, geo_buffer(CITYlimit, dist=100)) 

st_write(road_network, "database/road_network_dirty_nopeds.gpkg",  delete_dsn = TRUE)

# CONTINUE IN QGIS ----------------------------------------------------------------------------

# READ VERY CAREFULLY
# open QGIS with Grass

# run plugin
# http://plugins.qgis.org/plugins/disconnected-islands/
#  
# select all that connect (select by attributes all  "networkGrp" =0) and eport selected features
# the trick is, when exporting, do not export field "fid", and rename fid bellow to "fid_2",
# otherwise the clean process cannot save the output
# export as "network_groups-gpkg"
#
# v.clean
# select other hidden options: break, snap, rmdangle, rmdupl, bpol
#
# tolerance for each
# 0, 0.00000100, 0.00000100, 0, 0
#
# save output as "network_vclean.gpkg"

# THEN proceed to
# v.net.centrality
# (accept all defaluts)



# BACK TO R ---------------------------------------------------------------

centrality_nodes = st_read("database/centrality_nodes_lisbon_vclean_vcentrality.gpkg")
# centrality_nodes = centrality_nodes %>% 
#   mutate(degree = rescale(degree),
#          betweenness = rescale(betweenness),
#          closeness = rescale(closeness))
         
centrality_grid = st_join(centrality_nodes, #WGS84
                          GRID,
                          join = st_intersects) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarise(degree = mean(degree),
            betweenness = mean(betweenness),
            closeness = mean(closeness)) %>% 
  mutate(degree = rescale(degree),
         betweenness = rescale(betweenness),
         closeness = rescale(closeness)
  )



summary(centrality_grid$degree)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.1250  0.2500  0.2176  0.2500  1.0000 
# 0.0000  0.3333  0.4286  0.4089  0.5000  1.0000 
# 0.0000  0.3333  0.4167  0.4020  0.5000  1.0000  bpol
# 0.0000  0.3438  0.4500  0.4222  0.5000  1.0000 gabriel
# 0.0000  0.3511  0.4586  0.4278  0.5000  1.0000 qgis

summary(centrality_grid$betweenness)
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0000000 0.0008968 0.0062977 0.0443808 0.0402014 1.0000000 
# 0.000000 0.006586 0.026042 0.067994 0.081303 1.000000 
# 0.00000 0.00680 0.02646 0.06641 0.08072 1.00000 bpol
# 0.00000 0.00876 0.03295 0.07747 0.10250 1.00000  gabriel
# 0.00000 0.01025 0.03387 0.06895 0.09223 1.00000 gabriel qgis
# 0.00000 0.01720 0.05171 0.09938 0.13307 1.00000 qgis

summary(centrality_grid$closeness) 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.4256  0.5759  0.5850  0.7531  1.0000 
# 0.0000  0.3748  0.5200  0.5338  0.7105  1.0000 
# 0.0000  0.3774  0.5291  0.5379  0.7093  1.0000 bpol
# 0.0000  0.4793  0.5508  0.5750  0.6543  1.0000 gabriel qgis <<<<----
# 0.0000  0.1310  0.2462  0.2839  0.4028  1.0000 qgis

saveRDS(centrality_grid, "database/centrality_grid.Rds")

# Algo não está bem, e acho que tem a ver com a normalização.