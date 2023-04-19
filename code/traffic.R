# aim: get traffic conditions along the day

options(echo = TRUE)

library(dplyr)
library(raster)
library(googletraffic)

rmarkdown::find_pandoc(dir = "/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/pandoc")
Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio/resources/app/bin/quarto/bin/tools/pandoc")
# rmarkdown::pandoc_exec()
rmarkdown::find_pandoc()


# Get data in raster format
CITYlimit = readRDS("/media/rosa/Dados/GIS/Streets4All/SiteSelection/database/lisbon_limit.Rds")
traffic_raster = gt_make_raster_from_polygon(polygon = CITYlimit,
                    zoom       = 16, #12 - 1.6MB, 13 - 6.2MB, 14 - 24MB, 15 - 98MB, 16 -349MB
                    google_key = Sys.getenv("GOOGLE_KEY"))

# Vizualize
# traffic_pal = leaflet::colorNumeric(c("green", "orange", "red", "#660000"), 
#                             1:4,
#                             na.color = "transparent")
# leaflet::leaflet(width = "100%") %>%
#   leaflet::addProviderTiles("Esri.WorldGrayCanvas") %>%
#   leaflet::addRasterImage(rtest, colors = traffic_pal, opacity = 1, method = "ngb") 

# Convert raster to points
traffic_points = rasterToPoints(traffic_raster, spatial = TRUE) %>% 
  as.data.frame() %>% 
  rename(traffic = layer)

# Export - 68MB - is this necessary?
write.table(traffic_points, 
            file = paste0("/media/rosa/Dados/GIS/Streets4All/SiteSelection/database/traffic/", format(as.POSIXct(Sys.time()), format = "%y%d%m_%H%M"), ".txt"),
            row.names = FALSE,
            sep = "\t")
