# aim: select the appropriate cells depending on their indicators



# Centrality ----------------------------------------------------------------------------------

CELL_candidates = left_join(GRID, centrality_grid)

plot(CELL_candidates["betweenness"]) 
plot(CELL_candidates["closeness"]) 
plot(CELL_candidates["degree"]) 

# Filter in thresholds
CELL_candidates = CELL_candidates %>% 
  filter(betweenness %in% (0.25:0.75), #too high
         closeness %in% (0.25:0.75),  #too high
         degree >= 0.5) #for Lisbon. Adjust for other places?

         
# 
# LANDUSE
# density -> higher than mean(density)
# entropy -> hiher than 0.5 (in 4 land uses, have at least 2)

# TIME
# traffic -> ?
