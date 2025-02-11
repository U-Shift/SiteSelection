library(gtfsrouter)

gtfs_routing <- function(gtfs_data, from_stop_id, to_stop_id) {
  gtfsrouter::gtfs_route(gtfs_data, from_stop_id, to_stop_id)
}

gtfs <- gtfsrouter::extract_gtfs("database/transit/GTFS_merged_Algarve_withTransfers.zip")
gtfs <- gtfsrouter::gtfs_timetable(gtfs, day="Wed")

gtfs_routing(gtfs, "VRS António (Bombeiros)", "Praia Dona Ana") 
gtfs_routing(gtfs, "VRS António (Bombeiros)", "Monte Gordo (Pq. Campismo)") 
gtfs_routing(gtfs, "VRS António (Bombeiros)", "Faro (Moto Clube)") 
gtfs_routing(gtfs, "VRS António (X Castro Marim)", "Faro (Moto Clube)") 

