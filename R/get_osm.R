#' get_osm
#' 
#' @import sf
#' @import osmdata
#' @import dplyr
#' 
#' @noRd
get_osm = function(CITYlimit, CITY, build_osm) {
  
  if(
    # build_osm == FALSE & # ISTO PARECE ESTAR A FAZER COM QUE SEJA SEMPRE DESCARREGADO O OSM
    file.exists(paste0("outputdata/", CITY, "/road_network.shp"))){
    
    road_network = st_read(paste0("outputdata/", CITY, "/road_network.shp"), quiet = TRUE)
    
  }
  
  else {
    
    
    CITYlimit = st_read(paste0("outputdata/", CITY, "/CITYlimit.geojson"), quiet = TRUE)
    
    BBOX = st_as_sfc(st_bbox(CITYlimit))
    
    # road_osm = st_read("database/geofabrik_portugal-latest.gpkg", quiet = TRUE) #old version with osmextract
    
    road_osm = opq(BBOX) |> # uses osmdata package, to extract only with BB
      add_osm_feature(key = "highway") |> 
      osmdata_sf() |> 
      osm_poly2line() # makes roundabouts into lines
    road_osm = road_osm$osm_lines |>
      select(osm_id, name, highway, geometry)
    
    road_network = road_osm |>
      dplyr::filter(highway %in% c('motorway',"motorway_link",'primary', "primary_link",
                                   'secondary',"secondary_link", "trunk", 'trunk_link',
                                   "tertiary", "tertiary_link", "pedestrian",
                                   "residential", "living_street", "unclassified", "service"))
    
    road_network = st_intersection(road_network, stplanr::geo_buffer(CITYlimit, dist=100)) 
    
    road_network$group = stplanr::rnet_group(road_network, d = 10) # 10m tolerance
    # plot(lisbon_network["group"])
    
    road_network_groups = road_network |> filter(group == 1) #the network with more connected segments
    
    road_network = road_osm |> filter(osm_id %in% road_network_groups$osm_id) # get only the segments from the clean network
    
    # st_geometry(road_network) # Should be "LINESTRING"
    # road_network = st_cast(road_network, "LINESTRING") #if you don't wnat to use the previous filter
    # road_network = stplanr::rnet_breakup_vertices(road_network) # break the segments internally, conserving the brunels.
    
    road_network = road_network |> select(osm_id, highway, geometry) # keep some variables
    
    
    st_write(road_network, paste0("outputdata/", CITY, "/road_network.shp"), delete_dsn = TRUE, quiet = TRUE)
    
  }
  
}