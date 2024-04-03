# aim: landuse class for each cell


# landuse -----------------------------------------------------------------

# get OSM POIs with 6 categories
POIs = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/osm_poi_landuse.gpkg")
# table(POIs$group)
# amenity healthcare    leisure       shop      sport    tourism 
# 61513       2643       9395      41275      14192       9651 


# get census buildings
BUILDINGS = st_read("https://github.com/U-Shift/SiteSelection/releases/download/0.1/CENSUSpoint.gpkg")
BUILDINGS = BUILDINGS |>
  select(BGRI2021, N_EDIFICIOS_EXCLUSIV_RESID, Concelho, geom) |> 
  rename(buildings = N_EDIFICIOS_EXCLUSIV_RESID)

# entropy - diversity -----------------------------------------------------

categories = c("amenity", "healthcare", "leisure", "shop", "sport", "tourism", "residential")
n_categories = length(categories)

grid_poi = POIs[GRID,] |>
  st_join(GRID, join = st_intersects) |>
  group_by(ID, group) |>
  summarise(n = n()) |>
  ungroup() |> 
  st_drop_geometry()
grid_resid = BUILDINGS[GRID,] |>
  st_join(GRID, join = st_intersects) |>
  group_by(ID) |>
  summarise(n = sum(buildings)) |>
  ungroup() |>
  st_drop_geometry() |>
  mutate(group = "residential") |> 
  filter(n > 0)

# this is to verify the steps if make sense
grid_landuse = bind_rows(grid_poi, grid_resid) |> 
  group_by(ID) |> 
  summarise(group = group,
            n = n,
            prop = n/sum(n),
            entropy = -sum(prop * log(prop))) |> 
  ungroup()

# this is the final value
landuse_grid = grid_landuse |> 
  group_by(ID) |>
  summarise(entropy = entropy/log(n_categories)) |> 
  ungroup() |> 
  distinct()

# back to grid  --------------------------------------------------

landuse_grid = GRID |> left_join(landuse_grid)

mapview::mapview(landuse_grid, zcol = "entropy")
