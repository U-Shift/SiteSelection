# aim: select the appropriate cells depending on their indicators



# Centrality ----------------------------------------------------------------------------------

CELL_candidates = left_join(GRID, centrality_grid)
st_write(CELL_candidates, "database/CELL_candidates.gpkg")

plot(CELL_candidates["degree"]) 
plot(CELL_candidates["betweenness"]) 
plot(CELL_candidates["closeness"]) 
mapview::mapview(CELL_candidates["closeness"])

# Filter in thresholds #for Lisbon. Adjust for other places?
CELL_candidates = CELL_candidates %>% 
  filter(degree >= summary(centrality_grid$degree)[[4]], #1088 média 
         betweenness >= quantile(centrality_grid$betweenness, 0.40) & betweenness <= quantile(centrality_grid$betweenness, 0.60), #207 #too high
         closeness >= quantile(centrality_grid$closeness, 0.10) & closeness <= quantile(centrality_grid$closeness, 0.7) #2135 #too high
         ) 
# 56 results
# 42 results
mapview::mapview(CELL_candidates)
         

# Land use ----------------------------------------------------------------

# density -> higher than mean(density)
# entropy -> hiher than 0.5 (in 4 land uses, have at least 2)


# Public transport --------------------------------------------------------

grid = st_read("outputdata/Lisboa/grid.geojson")

gtfs = st_read("database/transit/bus_stop_freq.gpkg")
gtfs_1 = gtfs[grid, ] |> 
  st_join(grid, join = st_intersects) |>
  st_drop_geometry()

# Calculate the frequency sum per hour within a zone

gtfs_2 = gtfs_1 |> 
  group_by(ID, hour) |> 
  summarise(frequency = sum(frequency))

# Calculate the maximum frequency of stops_id per day

gtfs_3 = gtfs_2 |>  
  group_by(ID) |>  
  summarise(max_frequency = max(frequency))

grid_gtfs3 = grid |> left_join(gtfs_3, by = "ID")
mapview::mapview(grid_gtfs3, zcol = "max_frequency")
mapview::mapview(grid_gtfs3, zcol = "max_frequency") + mapview::mapview(gtfs[grid, ] )

# Calculate the level of service of each stop_id
#if max_frequency <= 4 -> 1
#if max_frequency > 4 and <= 10 -> 2
#if max_frequency > 10 and <= 20 -> 3
#if max_frequency > 20 -> 4

gtfs_4 = gtfs_3 |> 
  mutate(max_frequency = replace_na(max_frequency, 0)) |> 
  mutate(level = case_when(
    max_frequency <= 4 ~ 1,
    max_frequency > 4 & max_frequency <= 10 ~ 2,
    max_frequency > 10 & max_frequency <= 20 ~ 3,
    max_frequency > 20 ~ 4
  ))

# st_write(gtfs_4, "bus_stop_LS_gtfs_final.gpkg")

grid_gtfs4 = grid |> left_join(gtfs_4, by = "ID")
mapview::mapview(grid_gtfs4, zcol = "max_frequency")
mapview::mapview(grid_gtfs4, zcol = "max_frequency") + mapview::mapview(gtfs[grid, ] )

# Traffic -----------------------------------------------------------------

# traffic -> high (max(traffic) in each cell is 3 or 4), (1 and 2 is low)
