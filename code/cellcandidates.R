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



# Traffic -----------------------------------------------------------------

# traffic -> high (max(traffic) in each cell is 3 or 4), (1 and 2 is low)
