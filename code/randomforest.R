# random forest


analysis_table_loop <- readRDS("analysis/analysis_table_loop.Rds")
analysis_cluster = analysis_table_loop |> 
  select(CITY, n_candidates,
         starts_with("mean_")) |>
  select(-mean_frequency, -mean_score)

# use random forest cluster to make cluster with variables starting with "mean_" as independent variables
library(randomForest)
set.seed(123)
rf_cluster <- randomForest(analysis_cluster[, grepl("^mean_", colnames(analysis_cluster))], ntree=1000, importance=TRUE, proximity = TRUE)
rf_cluster$importance


hclust_rf <- hclust(as.dist(1-rf_cluster$proximity), method = "ward.D2")
plot(hclust_rf)
rf_cluster2 = cutree(hclust_rf, k=4) # definir aqui o número de clusters
table(rf_cluster2)
analysis_cluster$rf.clusters <- rf_cluster2
table(rf_cluster2, analysis_cluster$CITY)

# plot box plot of mean_degree for each cluster
library(ggplot2)
ggplot(analysis_cluster, aes(x=factor(rf.clusters), y=mean_degree)) + geom_boxplot() + theme_bw()
# now with all other variables starting with "mean_", 1 plot for each variable andput the variable name in the title
for (i in grep("^mean_", colnames(analysis_cluster))) {
  print(ggplot(analysis_cluster, 
               aes(
    x = factor(rf.clusters),
    y = analysis_cluster[, i]
  )) +
    geom_boxplot() +
    theme_bw() +
    ggtitle(names(analysis_cluster)[i]))
}


# map clusters with CAOP
library(sf)
library(dplyr)
CAOP_municipios <- st_read("database/CAOP_municipios.gpkg") |> distinct()
map_clusters = CAOP_municipios |> left_join(analysis_cluster |> select(CITY, rf.clusters), by = c("Concelho" = "CITY"))
map_clusters$rf.clusters = as.factor(map_clusters$rf.clusters)
mapview::mapview(map_clusters, zcol = "rf.clusters", lwd = 0) 
