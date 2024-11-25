# Aim: create a grid with 500m in Lisbon (or in other city)


library(tidyverse)
library(sf)
library(h3jsr)


# Get a polygon with the city boundaries ------------------------------------------------------

CITY = "Lisboa" # change here the city 

# MUNICIPIOSgeo = readRDS(url("https://github.com/U-Shift/SiteSelection/releases/download/0.1/MUNICIPIOSgeo.Rds")) # Lisbon Metro
MUNICIPIOSgeo = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CAOP_municipios.gpkg") # Portugal
CITYlimit = MUNICIPIOSgeo %>% filter(Concelho == CITY)


plot(CITYlimit)


# Make a grid that covers the city polygon ----------------------------------------------------

CITYlimit_meters = st_transform(CITYlimit, 3857) #projected
cellsize = c(200, 200) #200x200m

GRID = st_make_grid(CITYlimit_meters,
                    cellsize = cellsize,
                    square = TRUE #FALSE = hexagons
                    ) %>% 
  st_intersection(CITYlimit_meters) %>%
  st_sf() %>% #convert sfc to sf
  st_cast() %>% 
  mutate(ID = seq(1:nrow(.))) %>% # give an ID to each cell
  st_transform(st_crs(CITYlimit)) # go back to WGS48 if needed

mapgrid = mapview::mapview(GRID, alpha.regions = 0.2)



# use h3 pkg --------------------------------------------------------------
# Resolution: https://h3geo.org/docs/core-library/restable/ 
# h3_res = 10 # 150m diameter
h3_res = 9 # 400m diameter
# h3_res = 8 # 1060m diameter

GRID_h3 = CITYlimit |>  
  polygon_to_cells(res = h3_res, simple = FALSE)  # res = 9 is 500m
GRID_h3 = GRID_h3$h3_addresses |>
  cell_to_polygon(simple = FALSE)
  mutate(ID = seq(1:nrow(.))) # give an ID to each cell
GRID_h3 = GRID_h3 |>
  mutate(ID = seq(1:nrow(GRID_h3)))  # give an ID to each cell
h3_index = GRID_h3 |> st_drop_geometry() # save h3_address for later
GRID_h3 = GRID_h3 |>
  select(-h3_address)

mapgrid_h3 = mapview::mapview(GRID_h3, alpha.regions = 0.2)
mapgrid_h3
