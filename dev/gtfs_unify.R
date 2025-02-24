library(GTFSwizard)
library(SiteSelection)

request <- read.csv("inst/extdata/gtfs_sources_pt.csv") |>
  subset(ID %in% list("barreiro", "cascais"))


gtfs_list <- lapply(request$ID, function(area) {
  local <- gtfs_download(request$URL[request$ID == area], sprintf("database/transit/%s_gtfs.zip", area))
  return(GTFSwizard::read_gtfs(local))
})


gtfs_unify(gtfs_list, "database/transit/gtfs_unified.zip")
