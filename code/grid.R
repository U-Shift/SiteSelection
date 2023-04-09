# Aim: create a grid with 500m in Lisbon (or in other city)


library(tidyverse)
library(sf)



# Get a polygon with the city boundaries ------------------------------------------------------

MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/SiteSelection/releases/download/0.1/MUNICIPIOSgeo.Rds")) # 

CITY = "Lisboa" # change here the city 
CITYlimit = MUNICIPIOSgeo %>% filter(Concelho == CITY)

plot(CITYlimit)


# Make a grid that covers the city polygon ----------------------------------------------------

CITYlimit_meters = st_transform(CITYlimit, 3857) #projected
cellsize = c(500, 500) #500x500m

GRID = st_make_grid(CITYlimit_meters,
                    cellsize = cellsize,
                    square = TRUE #FALSE = hexagons
                    ) %>% 
  st_intersection(CITYlimit_meters) %>%
  st_sf() %>% #convert sfc to sf
  st_cast() %>% 
  mutate(ID = seq(1:nrow(.))) %>% # give an ID to each cell
  st_transform(st_crs(CITYlimit)) # go back to WGS48 if needed

mapview::mapview(GRID, alpha.regions = 0.1)
