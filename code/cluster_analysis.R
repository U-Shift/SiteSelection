
# Cluster analysis
library(readxl) # Reading excel files
library(skimr) # Summary statistics
library(tidyverse) # Pack of useful tools
library(mclust) # Model based clustering
library(cluster) # Cluster analysis
library(factoextra) # Visualizing distances
library(DataExplorer)
library(corrplot)

# Import data
data <- readRDS("analysis/analysis_table_loop_wpop.Rds") 

  #All cities above 20 thousands

data <- data |>   
  select(CITY,
         population,
         n_candidates,
         starts_with(c("mean","sd"))) |> 
  select(-ends_with(c("frequency","score")))

df <- data.frame(data)

df_no_names <- df[,-1]

skim(df_no_names)

#Plot the histogram
plot_histogram(df_no_names)

#Plot correlation plot
res <- cor.mtest(df_no_names, conf.level = .95) #store the results so you can call the p-value at the corrplot

corrplot(cor(df_no_names), p.mat = res$p, method = "number", type = "upper", order="hclust", sig.level = 0.05)

df_reduced = df[,!(names(df) %in% c("CITY"))]

# Standardize variables

mean <- apply(df_reduced, 2, mean) # The "2" in the function is used to select the columns. MARGIN: c(1,2)
sd <- apply(df_reduced, 2, sd)
df_scaled <- scale(df_reduced, mean, sd)

# Hierarchical Clustering

distance <- dist(df_scaled, method = "euclidean")

# Visualize distances in heatmap
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"), order = FALSE)

#Hierarchical clustering

# 1. Single linkage

models <- hclust(distance, "single")
plot(models, labels = df$CITY, xlab = "Distance - Single linkage", cex=0.6, hang = -1)

rect.hclust(models, 4, border = "purple") # Visualize the cut on the dendogram, with 4 clusters

# 2. Complete linkage

modelc <- hclust(distance, "complete")
plot(modelc, labels = df$CITY, xlab = "Distance - Complete linkage", cex=0.6, hang = -1)
rect.hclust(modelc, 4, border = "blue") 

# 3. Average linkage

modela <- hclust(distance, "average")
plot(modela, labels = df$CITY, xlab = "Distance - Average linkage", cex=0.6, hang = -1)
rect.hclust(modelc, 10, border = "red")

#4. Ward's method

modelw <- hclust(distance, "ward.D2")
plot(modelw, labels = df$CITY, xlab = "Distance - Ward method", cex=0.6, hang = -1)
rect.hclust(modelw, 9, border = "red")

#5 Centroid method

modelcen <- hclust(distance, "centroid")
plot(modelcen, labels = df$CITY, xlab = "Distance - Centroid method", cex=0.6, hang = -1)
rect.hclust(modelcen, 4, border = "darkgreen")

#Comparision of results

member_single <- cutree(models, 9)
member_com <- cutree(modelc, 9)
member_av <- cutree(modela, 9)
member_ward <- cutree(modelw, 10)
member_cen <- cutree(modelcen, 9)

#Compare communality between clusters

table(member_com, member_av) # compare the complete linkage with the average linkage

#Silhouette plots

plot(silhouette(member_single, distance))
plot(silhouette(member_com, distance))
plot(silhouette(member_av, distance))
plot(silhouette(member_ward, distance))
plot(silhouette(member_cen, distance))

# Non-hierarchical cluster


df_scaled_mean <- df_reduced |>
  select(
    mean_degree,
    # sd_degree,
    mean_betweenness,
    # sd_betweenness,
    mean_closeness,
    # sd_closeness,
    mean_population,
    # sd_population,
    mean_entropy,
    # sd_entropy
  )

km_clust <- kmeans(df_scaled, 5)
km_clust #print the results

df_names = df |> 
  select(CITY) |> 
  cbind(km_clust$cluster)

#Setting number of clusters
k <- list()
for(i in 1:20){
  k[[i]] <- kmeans(df_scaled, i)
}

betSS_totSS <- list()
for(i in 1:20){
  betSS_totSS[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}
plot(1:20, betSS_totSS, type = "b", ylab = "Between SS / Total SS", xlab = "Number of clusters")

par(cex.axis=0.6, mar=c(11,2,1,1))# Make labels fit in the boxplot
boxplot(df_scaled, las = 2) #labels rotated to vertical


plot(mean_degree ~ mean_population, df, col = km_clust$cluster)
with(df, text(mean_degree ~ mean_population, label = CITY, pos = 1, cex = 0.6))
