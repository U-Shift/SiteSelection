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
                    offset = st_bbox(CITYlimit_meters)[c("xmin", "ymin")], #bb ist not created correctly! rxplore how to cover properly
                    # what = "polygons",
                    # n = c(1, 1),
                    square = TRUE #FALSE = hexagons
                    ) 
plot(CITYlimit_meters)
plot(GRID, add = TRUE)

#bbox ist not created correctly! rxplore how to cover properly