# aim: get population for each cell and then include information in the grid

library(tidyverse)
library(sf)

# example data --------------------------------------------------------------

CITY = "Almada"
CITYlimit = st_read(paste0("outputdata/", CITY, "/CITYlimit.geojson"))
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

# density --------------------------------------------------------------

CENSUSpoint = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CENSUSpoint.gpkg")
# CENSUSpoint = CENSUSpoint |> st_intersection(GRID) # takes too long
CENSUScity = CENSUSpoint |> filter(Concelho == toupper(CITY))


# back to grid -------------------------------------------------------------

population_grid = 
  st_join(CENSUScity |> select(BGRI2021, N_INDIVIDUOS, geom),
          GRID,
          # st_transform(grid, 3857),
          join = st_intersects) %>% 
  st_drop_geometry() %>% 
  group_by(ID) %>% 
  summarise(population = sum(N_INDIVIDUOS)) |> 
  mutate(population = scales::rescale(population)) #?

# table(population_grid$population > 0.5)
# FALSE  TRUE 
# 1044    25 

population_grid_geo = GRID |> left_join(population_grid)
mapview::mapview(population_grid_geo, zcol="population") + mapview::mapview(CENSUScity)

# selection - above mean (should be above median?)
density_grid = population_grid_geo |> filter(population > mean(population_grid$population)) #above mean
mapview::mapview(density_grid, zcol="population")
