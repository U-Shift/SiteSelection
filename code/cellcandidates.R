# aim: select the appropriate cells depending on their indicators



# Centrality ----------------------------------------------------------------------------------

CELL_candidates = left_join(GRID, centrality_grid)

plot(CELL_candidates["degree"]) 
plot(CELL_candidates["betweenness"]) 
plot(CELL_candidates["closeness"]) 


# Filter in thresholds
CELL_candidates = CELL_candidates %>% 
  filter(degree >= 0.5, #1088 #for Lisbon. Adjust for other places?
         betweenness >= 0.25 & betweenness <= 0.75, #207 #too high
         closeness >= 0.25 & closeness <= 0.75 #2135 #too high
         ) 
# 56 results
mapview::mapview(CELL_candidates)
         

# Land use ----------------------------------------------------------------

# density -> higher than mean(density)
# entropy -> hiher than 0.5 (in 4 land uses, have at least 2)



# Traffic -----------------------------------------------------------------

# traffic -> high (max(traffic) in each cell is 3 or 4), (1 and 2 is low)
