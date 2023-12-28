# Kmeans clustering  ----
set.seed(123)  # Set a random seed for reproducibility
## assume 5 cluster s which seems be the case (visual inspection seems that way)
clusters <- kmeans(road_safety_greater_manchester[, c("lat", "lng")], centers = 5)
library(ggplot2)
library(factoextra)

## see kmeans clusters, however kmeans is sensible to outliers, then this may affect results
ggplot(road_safety_greater_manchester, aes(x = lng, y = lat, color = as.factor(clusters$cluster))) +
  geom_point(alpha = 0.5) +
  labs(title = "K-mean Clustering of Road Safety Data",
       x = "Longitude",
       y = "Latitude",
       color = "Cluster") +
  labs(x = "",y = "")+
  coord_sf()+
  theme_minimal()

## Partioning Cluster plot
fviz_cluster(clusters, data = road_safety_greater_manchester,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07", "#95B301"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot" )

# dbscan clustering ----
library(dbscan)
library(factoextra)

# Assume eps and minPts are chosen based on domain knowledge or experimentation
# these are just thrown, let's try to find the best
# TODO lascialo a dopo
eps_value <- 0.026 # example value, adjust based on your data
minPts_value <- 80  # example value, adjust based on your data

# Compute and plot the kNN distances (look where it elbows)
kNNdistplot(road_safety_greater_manchester[, c("lat", "lng")], k = minPts_val)
abline(h = eps_value, col = "red")  # An example, adjust this line after observing the plot

# Perform DBSCAN clustering (this finds 31 clusters)
dbscan_result <- dbscan(road_safety_greater_manchester[, c("lat", "lng")], eps = eps_value, minPts = minPts_value)
# Add cluster assignments to your data
road_safety_greater_manchester$cluster <- as.factor(dbscan_result$cluster)

# Plot dbscan results (this does not seem the best method) let's stick to the k-means
ggplot(road_safety_greater_manchester, aes(x = lng, y = lat, color = cluster)) +
  geom_point(alpha = 0.5) +
  scale_color_discrete(name = "Cluster") +
  labs(title = "DBSCAN Clustering of Road Safety Data",
       x = "",
       y = "") +
  coord_sf()+
  theme_minimal()


## Partioning Cluster plot
fviz_cluster(dbscan_result, data = road_safety_greater_manchester,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07", "#95B301", "#771E23"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot" )


# THIS IS THE ALGO
## a) assign obs to cluster based on algorithm (kmeans, k = 5)
## b) assign cluster observation to respective h3 hexagon to resolution
## c) measure mean distance between cluster centroid if cluster centroid is closer to each h3 index that contains observations wrt to other cluster centroids
## d) repeat

set.seed(123)  # for reproducibility
data("road_safety_greater_manchester", package = "h3")

if(!is.data.frame(road_safety_greater_manchester)){
  road_safety_greater_manchester = as.data.frame(road_safety_greater_manchester)
}

kmeans_result <- kmeans(road_safety_greater_manchester[, c("lat", "lng")], centers = 5)
# Add the cluster assignments to your base dataset
road_safety_greater_manchester$cluster <- kmeans_result$cluster

library(h3)
library(geosphere)

calculate_mean_distances <- function(data, res, kmeans_result) {

  data$h3_index <- geo_to_h3(data[, c("lat", "lng")], res)

  centroids <- kmeans_result$centers

  mean_distances <- numeric(nrow(centroids))

  for (i in 1:nrow(centroids)) {
    cluster_data <- subset(data, cluster == i)
    if (nrow(cluster_data) == 0) next

    hexagons <- unique(cluster_data$h3_index)
    distances <- sapply(hexagons, function(hex) {
      hex_coords <- h3_to_geo(hex)
      hex_data <- subset(cluster_data, h3_index == hex)
      other_centroids <- centroids[-i, , drop = FALSE]
      own_dist <- distm(hex_coords, centroids[i, , drop = FALSE], fun = distHaversine)
      other_dists <- apply(other_centroids, 1, function(centroid) {
        distm(hex_coords, centroid, fun = distHaversine)
      })

      # Check if the current cluster's centroid is the closest
      if (own_dist < min(other_dists)) {
        return(mean(distm(hex_coords, as.matrix(hex_data[, c("lat", "lng")]), fun = distHaversine)))
      } else {
        return(NA)
      }
    })

    mean_distances[i] <- mean(distances, na.rm = TRUE)
  }

  return(mean_distances)
}

# Example resolutions
resolutions <- 4:13
results <- list()

for (res in resolutions) {
  results[[res]] <- calculate_mean_distances(road_safety_greater_manchester, res, clusters)
}



