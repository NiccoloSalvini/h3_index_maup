## "2023-12-13 19:44:57 CET"
library(h3)
library(dplyr)
library(here)
library(tidyr)
library(purrr)
library(spdep)
library(ggplot2)
library(patchwork)
library(plotly)
library(readr)

gs_mean_prices_intersect_sf =  read_rds(here("data",  "gs_mean_prices_intersect_sf.rds"))

# test
# ggplot(gs_mean_prices_intersect_sf) +
#   geom_sf(alpha = .6, size = .2)


#  01 Brute Analysis for each for each resolution for counts ----
#  TODO implement for prices instead of counts
generate_h3_plot <- function(data, resolution, fill = "prezzo_medio_per_res") {
  # Convert coordinates to H3 index at given resolution
  h3_index <- geo_to_h3(data, res = resolution)

  # Aggregate data by H3 index
  tbl_res <- data %>%
    mutate(h3_indices = h3_index) %>%
    dplyr::group_by(h3_indices) %>%
    summarise(
      n = n(),
      prezzo_medio_per_res = mean(prezzo_medio)
    )

  # Create sf hexagons
  hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_indices) %>%
    dplyr::mutate(
      count = tbl_res$n,
      prezzo_medio_per_res = tbl_res$prezzo_medio_per_res,
      resolution = as.factor(resolution)
    )

  # Generate plot
  plot <- ggplot(hexagons_sf) +
    geom_sf(aes(fill = .data[[fill]])) +
    labs(title = paste("Resolution", resolution), x = "Longitude", y = "Latitude") +
    scale_fill_viridis_c() +
    theme_minimal()

  ret_obj = list(
    plot = plot,
    hexagons = hexagons_sf
  )

  return(ret_obj)
}

# plot prices
plot_res_3 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 3)$plot
plot_res_4 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 4)$plot
plot_res_5 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution =5)$plot
plot_res_6 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 6)$plot


# plot concentration
plot_res_3_count <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 3, fill = "count")$plot
plot_res_4_count <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 4, fill = "count")$plot
plot_res_5_count <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution =5, fill = "count")$plot
plot_res_6_count <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 6, fill = "count")$plot



# 01.2 visualise same point at different resolutions (3 res), on same map ----

combined_hexagons = map_dfr(c(3,4,5), ~generate_h3_plot(data = gs_mean_prices_intersect_sf, resolution = .x)$hexagons)

# Create the plot
ggplot(combined_hexagons) +
  geom_sf(aes(color = resolution, shape = resolution), size = 3) +
  geom_point(data = gs_mean_prices_intersect_sf, aes(x = st_coordinates(gs_mean_prices_intersect_sf)[,"X"], y = st_coordinates(gs_mean_prices_intersect_sf)[,"Y"]), alpha = 0.3, size = .2)+
  labs(title = "Observations Across Different H3 Resolutions, same map",
       x = "Longitude", y = "Latitude",
       color = "Resolution", shape = "Resolution") +
  theme_minimal() +
  scale_color_manual(values = c("3" = "blue", "4" = "red", "5" = "green")) +
  scale_shape_manual(values = c("3" = 15, "4" = 17, "5" = 18))


# Arrange maps for comparison
combined_plot <- plot_res_3 + plot_res_4 + plot_res_5 +
  plot_layout(ncol = 3)

# Print the combined plot
combined_plot


# 01.3 Visualise 3D (mancano i punti) ----
# functions to create list of h3 indexes and sf objects for each resolutions
# here h3 index
create_h3_index_tables <- function(data, resolutions) {
  lapply(resolutions, function(res) {
    # Convert geo data to h3 indices
    h3_indices <- geo_to_h3(data, res = res)

    # Create a table of counts per hex
    tbl_res <- table(h3_indices) %>%
      as_tibble() %>%
      rename(h3_index = h3_indices, count = n)

    return(tbl_res)
  })
}

# Example usage
# Define your data and resolutions
resolutions <- seq(3, 6, by = 1)

h3_index_tables_list <- create_h3_index_tables(coords, resolutions)

# here h3 index sf obj with prices and counts!
create_hexagon_sf_list <- function(data, resolutions) {
  lapply(resolutions, function(res) {
    # Convert geo data to h3 indices
    h3_indices <- geo_to_h3(data, res = res)

    # Create a table of counts per hex
    tbl_res <- data %>%
      mutate(h3_indices = h3_indices) %>%
      dplyr::group_by(h3_indices) %>%
      summarise(
       n = n(),
       prezzo_medio_per_res = mean(prezzo_medio)
      )

    # Create sf hexagons
    hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_indices) %>%
      dplyr::mutate(
        count = tbl_res$n,
        prezzo_medio_per_res = tbl_res$prezzo_medio_per_res,
        resolution = as.factor(res)
      )

    # Add original data points
    data_points_sf <- st_as_sf(data, coords = c("X", "Y"), crs = 4326) %>%
      mutate(resolution = as.character(res))

    return(list(hexagons = hexagons_sf, points = data_points_sf))

    # return(hexagons_sf)
  })
}



h3_hexagons_sf_list <- create_hexagon_sf_list(gs_mean_prices_intersect_sf, resolutions)

# 3D vis of hexagons at different resolutions
p <- plot_ly()

# Iterate over each resolution and add hexagons and points to the plot
# this works with uncommented `create_hexagon_sf_list` fun
for (i in seq_along(h3_hexagons_sf_list)) {
  hexagons_sf <- h3_hexagons_sf_list[[i]]$hexagons
  points_sf <- h3_hexagons_sf_list[[i]]$points

  # Add hexagons
  p <- add_sf(p, data = hexagons_sf,
              z = ~as.numeric(resolution),
              #i = ~mesh$it[,1]-1, j = ~mesh$it[,2]-1, k = ~mesh$it[,3]-1,
              color = ~resolution)

  # Add points
  p <- add_trace(p, data = points_sf, type = "scatter3d", mode = "markers",
                 x = ~st_coordinates(geometry)[,1], y = ~st_coordinates(geometry)[,2], z = ~as.numeric(resolution),
                 marker = list(size = 2, color = 'black'))
}

# Finalize the plot
p <- p %>% layout(title = '3D Plot of Observations Across H3 Resolutions',
                  scene = list(xaxis = list(title = 'Longitude'),
                               yaxis = list(title = 'Latitude'),
                               zaxis = list(title = 'Resolution')))
p


#  03 Find neighbours ----
## Queen criteria https://www.paulamoraga.com/tutorial-neighborhoodmatrices/#1_Neighbors_based_on_contiguity
# Function to analyze and plot neighborhood structures
analyze_hexagon_neighborhoods <- function(coordinates, resolution_vec) {

  # create hex list
  hexagons_sf_list = create_hexagon_sf_list(coordinates, resolutions = resolution_vec) %>%
    map(1)

  # Create neighborhood structures
  hexagons_nb_list <- lapply(hexagons_sf_list, function(x) poly2nb(x, queen = TRUE))

  # Compare neighborhood structures
  comparison <- isTRUE(all.equal(hexagons_nb_list[[1]], hexagons_nb_list[[2]], check.attributes = FALSE))

  # Plot neighborhoods
  par(mfrow = c(1, 2), mar = c(3, 3, 1, 1) + 0.1)
  for (i in 1:length(hexagons_sf_list)) {
    plot(st_geometry(hexagons_sf_list[[i]]), border = "lightgrey")
    plot.nb(hexagons_nb_list[[i]], st_geometry(hexagons_sf_list[[i]]), add = TRUE)
  }

  # Convert to listw
  weights_list <- lapply(hexagons_nb_list, function(x) nb2listw(x, style = "W", zero.policy = TRUE))

  # Calculate Moran I
  moran_i_results <- lapply(1:length(hexagons_sf_list), function(i) {
    moran.test(pull(hexagons_sf_list[[i]], "prezzo_medio_per_res"), weights_list[[i]], zero.policy = TRUE)
  })

  # Return results
  list(
    comparison = comparison,
    plots = par(mfrow = c(1, 1)),
    moran_i_results = moran_i_results
  )
}

# Example usage
# Assuming hexagons_res_6_sf, hexagons_res_8_sf, and hexagons_res_10_sf are defined
results <- analyze_hexagon_neighborhoods(coordinates = gs_mean_prices_intersect_sf, resolution_vec = resolutions)

# Access specific results
# results$comparison
# results$moran_i_results[[1]] # for resolution 6
# results$moran_i_results[[2]] # for resolution 8
# results$moran_i_results[[3]] # for resolution 10




# 06 Calculate Moran I for each resolution  ----

# Define your data here
data(road_safety_greater_manchester, package = "h3")

# questo non ha senso perchè è point pattern senza livello corretto di autocorrelazione
# road_safety_greater_manchester = as.data.frame.matrix(road_safety_greater_manchester)
# road_safety_greater_manchester$accidents = 1
#
# # Compute baseline Moran I for no H3 indexing
# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# sf_road_safety_greater_manchester <- st_as_sf(x = road_safety_greater_manchester,
#                                               coords = c("lat", "lng"),
#                                               crs = projcrs)
#
# road_safety_greater_manchester_nb <- poly2nb(sf_road_safety_greater_manchester)
#
# # Convert to listw
# weights_res_road_safety <- nb2listw(road_safety_greater_manchester_nb, style = "W", zero.policy = TRUE)
#
# # Calculate Moran I
# moran_i_res <- moran.test(sf_road_safety_greater_manchester$accidents, weights_res_road_safety, zero.policy = TRUE)

# Function to calculate Moran's I for a given resolution
# can also plot moran plots (uncomment row) -> it breaks with a weird error.
calculate_moran_i <- function(data = gs_mean_prices_intersect_sf, res) {

  h3_indices <- geo_to_h3(data, res = res)

  # Create a table of counts per hex
  tbl_res <- data %>%
    mutate(h3_indices = h3_indices) %>%
    dplyr::group_by(h3_indices) %>%
    summarise(
      n = n(),
      prezzo_medio_per_res = mean(prezzo_medio)
    )

  # Create sf hexagons
  hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_indices) %>%
    dplyr::mutate(
      count = tbl_res$n,
      prezzo_medio_per_res = tbl_res$prezzo_medio_per_res,
      resolution = as.factor(res)
    )

  N = nrow(hexagons_sf)

  # Find neighbours
  cat("calculating for res:", res , " \n")
  hexagons_nb <- poly2nb(hexagons_sf, queen=TRUE)

  # Convert to listw
  weights_res <- nb2listw(hexagons_nb, style = "W", zero.policy = TRUE)

  # Calculate Moran I
  moran_i_res <- moran.test(hexagons_sf$prezzo_medio_per_res, weights_res, zero.policy = TRUE)

  # moran_plot = moran.plot(hexagons_sf$accidents,labels = hexagons_sf$h3_index, weights_res, pch=19)

  # Return the results
  list(
    resolution = res,
    moran_estimate = moran_i_res[[c(3, 1)]],
    p_value = moran_i_res$p.value,
    data_points = N
    # moran_plot = moran_plot
  )
}

# Apply the function to all resolutions (0 to 15)
# TODO fix when res < 4 and > 13
# this takes a while (save it!)
moran_results <- map_df(4:13, ~calculate_moran_i(res = .x))

# TODO save moran_results
# saveRDS(moran_results, "data/moran_results.rds")
moran_results = read_rds("data/moran_results.rds")

# View the results
print(moran_results)

# 08 Impact analysis on local moran when introducing random outliers -----
gs_mean_prices_intersect_sf$is_outlier = FALSE

provinces_sf = st_read("data/provinces.geojson") %>%
  st_transform(crs = 4326) %>%
  st_as_sf()

# Function to generate random points within boundaries
# TODO Try different smpling method: https://dickbrus.github.io/SpatialSamplingwithR/
generate_random_points <- function(n, boundaries, type = "random") {
  st_sample(boundaries, size = n, type = type) %>%
    st_as_sf(crs = 4326) %>%
    st_coordinates() %>%
    as.data.frame()
}

# Function to add random outliers to your data
# TODO fare in modo che entrambi i df abbiano lo stesso
#  numero di colonne
add_sp_outliers_to_data <- function(data, boundaries, n_outliers) {
  random_points <- generate_random_points(n_outliers, boundaries)
  colnames(random_points) <- c("lng", "lat") # Ensure these match your data's column names
  outlier_data <- data.frame(lat = random_points$lat, lng = random_points$lng, is_outlier = TRUE) %>% st_as_sf(coords = c("lng", "lat"), crs = 4326)
  rbind(data, outlier_data)
}


# Example of using the function in a simulation
simulate_with_outliers <- function(resolution, iterations, n_outliers) {
  results <- vector("list", iterations)
  for (i in 1:iterations) {
    # Add random outliers
    modified_data <- add_sp_outliers_to_data(gs_mean_prices_intersect_sf, provinces_sf, n_outliers)

    # Recalculate Moran's I
    results[[i]] <- calculate_moran_i(data = modified_data, resolution)
  }
  results
}

# 09 Run Simulation. -----
# how to decide the number of outliers, is there any statistical way to see that?
# could be the case of generating outliers up to a point that Moran Estimates sensibilty changes
simulation_results <- map_df(4:13, ~simulate_with_outliers( resolution = .x,iterations = 10, n_outliers = 100))  # For example, 10 iterations with 5 outliers each



## 09.1 Visualise newly rndm added points ----
library(ggplot2)

ggplot() +
  geom_sf(data = gmanchester_sf, fill = NA, color = "gray") + # Plot the boundaries
  geom_point(data = as.data.frame.matrix(road_safety_greater_manchester), aes(x = lng, y = lat, col = "black"), size = 1) + # Original data points
  geom_point(data = generate_random_points(100, gmanchester_sf), aes(x = X, y = Y, col = "blue"), size = 3) + # Outlier data points
  scale_color_manual(values = c("black", "red"), labels = c("existing", "newly generated"))+
  labs(
    x = "",
    y = "",
    colour = "") +
  coord_sf()+
  theme_minimal()


## 09.2  Line Plot of Moran's I Estimate by Resolution ----
library(ggplot2)

ggplot(simulation_results, aes(x = resolution, y = moran_estimate, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 4:13) +
  labs(title = "Moran's I Estimate by Resolution",
       x = "Resolution",
       y = "Moran's I Estimate") +
  theme_minimal()


##  09.3  Scatter Plot of Moran's I Estimate and p-value by Resolution ----
ggplot(simulation_results, aes(x = moran_estimate, y = p_value, color = as.factor(resolution))) +
  geom_point() +
  geom_line()+
  labs(title = "Moran's I Estimate vs. p-value for Each Resolution",
       x = "Moran's I Estimate",
       y = "p-value",
       color = "Resolution") +
  theme_minimal()



##  09.4 Faceted Plot for Each Resolution ----
ggplot(simulation_results, aes(x = as.factor(1:100), y = moran_estimate)) +
  geom_point() +
  facet_wrap(~resolution) +
  labs(title = "Moran's I Estimate Across Iterations for Each Resolution",
       x = "Iteration",
       y = "Moran's I Estimate") +
  theme_minimal()


# 10  Kmeans clustering  ----
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

# 11 dbscan clustering ----
library(dbscan)
library(factoextra)

# Assume eps and minPts are chosen based on domain knowledge or experimentation
# these are just thrown, let's try to find the best
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



## now using kmeans with k = 5 visualise and analyse
##  which and how many randomly generated points (as done before)
##  change clusters across different resolutions compared to the base scenario
##  (where they are not indexed with H3)



# 10.1 THIS IS THE ALGO
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


# 12 show how points change or remains in the same h3 index over different resolutions ----
# Create both sf and and tb for each resolution


# Example usage
# Define your data and resolutions
dataset <- road_safety_greater_manchester # replace with your actual data
resolutions <- seq(5, 9, by = 1)

# Generate the list of sf objects
h3_hexagons_sf_list <- create_hexagon_sf_list(dataset, resolutions)



# here you verify that any of these indexes are consistent
# since event tho one are inside the other they have different name.
# TODO prendi tutti quelli che hanno appartengono allo stesso cluster ma hanno
# parente diverso (h3_parent())
rev(h3_index_tables_list) %>%
  reduce(left_join, by = "h3_index")

combined_data <- tbl_res_6 %>%
  left_join(tbl_res_8, by = "h3_index") %>%
  left_join(tbl_res_10, by = "h3_index")

# Identify changes and consistencies
combined_data <- combined_data %>%
  mutate(
    change_status = case_when(
      is.na(n.x) | is.na(n.y) | is.na(n) ~ "Changed",
      TRUE ~ "Consistent"
    )
  )

# Prepare data for visualization
visualization_data <- combined_data %>%
  count(change_status) %>%
  mutate(resolution = "Combined 6, 8, 10")

# Visualize with ggplot2
# just one colums as predicted
ggplot(visualization_data, aes(x = resolution, y = n, fill = change_status)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Comparison of Observation Changes Across H3 Resolutions",
       x = "Resolution",
       y = "Number of Observations") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


# LISA!
# TODO split analysis into more /notebooks


# spatial block sampling ----
# spatial resampling and modelling
library(spatialsample)
h3_hexagons_sf_list_pured = h3_hexagons_sf_list %>%  map(1)
set.seed(123)
blocks <- spatial_block_cv(h3_hexagons_sf_list_pured[[3]], v = 5)
autoplot(blocks) +
  scale_color_discrete(name = "Fold", labels = c("Piega1", "Piega2", "Piega3", "Piega4", "Piega5")) +
  guides(fill = FALSE)

purrr::walk(blocks$splits, function(x) print(autoplot(x) +
                                                 scale_fill_discrete(name = "Fold", labels = c("Test", "Training"))+
                                                 guides(colour = FALSE)
                                                 ))


