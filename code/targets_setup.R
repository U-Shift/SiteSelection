library(targets)

CITY == "Lisbon"

use_targets()

tar_manifest()
tar_visnetwork()

tar_make()


tar_read()

centrality_grid = tar_read(centrality_grid)
summary(centrality_grid$degree)
summary(centrality_grid$betweenness)
quantile(centrality_grid$betweenness, 0.40, na.rm = TRUE)

summary(centrality_grid$closeness) #not good! necessario qgis
quantile(centrality_grid$closeness, 0.40, na.rm = TRUE)

tar_read(candidates_centrality)

# view results
tar_load(candidates_centrality)
mapview::mapview(candidates_centrality)

