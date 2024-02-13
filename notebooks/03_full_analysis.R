## "2023-11-16 17:38:47 CET"
library(h3)
library(dplyr)
library(here)
library(tidyr)
library(purrr)
library(spdep)
library(patchwork)
library(ggplot2)


#  01 Brute Analysis for each for each resolution ----

h3_index_res_6 <- geo_to_h3(road_safety_greater_manchester, res = 6)

h3_index_res_8 <- geo_to_h3(road_safety_greater_manchester, res = 8)

h3_index_res_10 <- geo_to_h3(road_safety_greater_manchester, res = 10)


## How many accidents per hex? ----
tbl_res_6 <- table(h3_index_res_6) %>%
  tibble::as_tibble() %>%
  rename(h3_index = h3_index_res_6)
dim(tbl_res_6)


tbl_res_8 <- table(h3_index_res_8) %>%
  tibble::as_tibble() %>%
  rename(h3_index = h3_index_res_8)
dim(tbl_res_8)

tbl_res_10 <- table(h3_index_res_10) %>%
  tibble::as_tibble() %>%
  rename(h3_index = h3_index_res_10)
dim(tbl_res_10)

## function to check colnames
check_col_names <- function(x) {
  if(!all(colnames(x) %in% c("h3_index", "n"))) {
    stop("colnames in ", x, "are not well written")
  } else {
    return(NULL)
  }
}

purrr::map(list(tbl_res_6, tbl_res_8, tbl_res_10), check_col_names)

#  02 Create sf hex ----
## this is needed to convert to ploy2nb complaiant format
hexagons_res_6_sf <- h3_to_geo_boundary_sf(tbl_res_6$h3_index) %>%
  dplyr::mutate(
    accidents = tbl_res_6$n
    )

hexagons_res_8_sf <- h3_to_geo_boundary_sf(tbl_res_8$h3_index) %>%
  dplyr::mutate(
    accidents = tbl_res_8$n
  )

hexagons_res_10_sf <- h3_to_geo_boundary_sf(tbl_res_10$h3_index) %>%
  dplyr::mutate(
    accidents = tbl_res_10$n
  )


# 01.1 visualize density across the three resolutions ----
plot_res_6 <- ggplot(hexagons_res_6_sf) +
  geom_sf(aes(fill = accidents)) +
  labs(title = "Resolution 6", x = "Longitude", y = "Latitude") +
  scale_fill_viridis_c() +
  theme_minimal()

plot_res_8 <- ggplot(hexagons_res_8_sf) +
  geom_sf(aes(fill = accidents)) +
  labs(title = "Resolution 8", x = "Longitude", y = "Latitude") +
  scale_fill_viridis_c() +
  theme_minimal()

plot_res_10 <- ggplot(hexagons_res_10_sf) +
  geom_sf(aes(fill = accidents)) +
  labs(title = "Resolution 10", x = "Longitude", y = "Latitude") +
  scale_fill_viridis_c() +
  theme_minimal()


# 01.2 visualise same point at different resolutions (3 res), on same map ----
hexagons_res_6_sf <- hexagons_res_6_sf %>% mutate(resolution = '6')
hexagons_res_8_sf <- hexagons_res_8_sf %>% mutate(resolution = '8')
hexagons_res_10_sf <- hexagons_res_10_sf %>% mutate(resolution = '10')

# Combine the datasets
combined_hexagons <- rbind(hexagons_res_6_sf, hexagons_res_8_sf, hexagons_res_10_sf)

# Create the plot
ggplot(combined_hexagons) +
  geom_sf(aes(color = resolution, shape = resolution), size = 3) +
  geom_point(data = data, aes(x = lng, y = lat), alpha = 0.3, size = .2)+
  labs(title = "Observations Across Different H3 Resolutions, same map",
       x = "Longitude", y = "Latitude",
       color = "Resolution", shape = "Resolution") +
  theme_minimal() +
  scale_color_manual(values = c("6" = "blue", "8" = "red", "10" = "green")) +
  scale_shape_manual(values = c("6" = 15, "8" = 17, "10" = 18))


# Arrange maps for comparison
combined_plot <- plot_res_6 + plot_res_8 + plot_res_10 +
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
dataset <- as.data.frame.matrix(road_safety_greater_manchester) # replace with your actual data
resolutions <- seq(5, 9, by = 1)

h3_index_tables_list <- create_h3_index_tables(dataset, resolutions)

# here h3 index sf object (together with points, commented part)
create_hexagon_sf_list <- function(data, resolutions) {
  lapply(resolutions, function(res) {
    # Convert geo data to h3 indices
    h3_indices <- geo_to_h3(data, res = res)

    # Create a table of counts per hex
    tbl_res <- table(h3_indices) %>%
      tibble::as_tibble() %>%
      rename(h3_index = h3_indices, count = n)

    # Create sf hexagons
    hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_index) %>%
      dplyr::mutate(
        count = tbl_res$count,
        resolution = as.character(res)
      )

    # # Add original data points
    data_points_sf <- st_as_sf(data, coords = c("lng", "lat"), crs = 4326) %>%
      mutate(resolution = as.character(res))

    return(list(hexagons = hexagons_sf, points = data_points_sf))

    return(hexagons_sf)
  })
}

h3_hexagons_sf_list <- create_hexagon_sf_list(dataset, resolutions)



# represent just points ----
ggplot() +
  geom_sf(data = gmanchester_sf, fill = NA, color = "gray") + # Plot the boundaries
  geom_point(data = as.data.frame.matrix(road_safety_greater_manchester), aes(x = lng, y = lat), size = 1, alpha = .3) +
  labs(x = "",
       y = "") +
  theme_minimal(

  )


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
hexagons_res_6_nb <- poly2nb(hexagons_res_6_sf, queen=TRUE)
hexagons_res_8_nb <- poly2nb(hexagons_res_8_sf, queen=TRUE)
hexagons_res_10_nb <- poly2nb(hexagons_res_10_sf, queen=TRUE)

isTRUE(all.equal(hexagons_res_6_nb, hexagons_res_8_nb, check.attributes=FALSE))

## 03.1 plot neighbours for 6 an 8 (10 is too little) ----
oopar <- par(mfrow=c(1,2), mar=c(3,3,1,1)+0.1)
plot(st_geometry(hexagons_res_6), border = "lightgrey")
plot.nb(hexagons_res_6_nb, st_geometry(hexagons_res_6), add = TRUE)
plot(st_geometry(hexagons_res_8), border = "lightgrey")


## 03.2 look at only 10, most of them are not neighbours ----
par(mfrow=c(1,1))
plot(st_geometry(hexagons_res_10), border = "lightgrey")
plot.nb(hexagons_res_10_nb, st_geometry(hexagons_res_10), add = TRUE,col="red")
print(weights_res_10, zero.policy=TRUE) ## 1457 regions with no links.


# 04 Convert to listw ----
weights_res_6<-nb2listw(hexagons_res_6_nb, style = "W", zero.policy = TRUE)
weights_res_8<-nb2listw(hexagons_res_8_nb, style = "W", zero.policy = TRUE)
weights_res_10<-nb2listw(hexagons_res_10_nb, style = "W", zero.policy = TRUE)


# 05 Calculate Moran I ----
moran_i_res_6<-moran.test(pull(hexagons_res_6, accidents), weights_res_6, zero.policy = TRUE)
moran_i_res_8<-moran.test(pull(hexagons_res_8, accidents), weights_res_8, zero.policy = TRUE)
moran_i_res_10<-moran.test(pull(hexagons_res_10, accidents), weights_res_10, zero.policy = TRUE)

## res 6
moran_i_res_6[[c(3, 1)]]
moran_i_res_6$p.value

## res 8
moran_i_res_8[[c(3, 1)]]
moran_i_res_8$p.value

## res 10
moran_i_res_10[[c(3, 1)]]
moran_i_res_10$p.value

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
calculate_moran_i <- function(data = road_safety_greater_manchester, res) {

    # Convert geo data to h3 indices
    h3_indices <- geo_to_h3(data, res = res)

    # Create a table of accidents per hex
    tbl_res <- table(h3_indices) %>%
      tibble::as_tibble() %>%
      rename(h3_index = h3_indices)

    # Create sf hexagons
    hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_index) %>%
      dplyr::mutate(
        accidents = tbl_res$n
      )
    N = nrow(hexagons_sf)

    # Find neighbours
    cat("calculating for res:", res , " \n")
    hexagons_nb <- poly2nb(hexagons_sf, queen=TRUE)

    # Convert to listw
    weights_res <- nb2listw(hexagons_nb, style = "W", zero.policy = TRUE)

    # Calculate Moran I
    moran_i_res <- moran.test(hexagons_sf$accidents, weights_res, zero.policy = TRUE)

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
moran_results <- map_df(4:13, ~calculate_moran_i(res = .x))

# View the results
print(moran_results)


# 07 Calculate s-maup at each resolution ----
calculate_smaup <- function(res) {
  # Convert geo data to h3 indices
  h3_indices <- geo_to_h3(road_safety_greater_manchester, res = res)

  # Create a table of accidents per hex
  tbl_res <- table(h3_indices) %>%
    tibble::as_tibble() %>%
    rename(h3_index = h3_indices)

  # Create sf hexagons
  hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_index) %>%
    dplyr::mutate(
      accidents = tbl_res$n
    )

  # Find neighbours
  cat("calculating for res:", res , " \n")
  hexagons_nb <- poly2nb(hexagons_sf, queen=TRUE)

  # Convert to listw
  weights_res <- nb2listw(hexagons_nb, style = "W", zero.policy = TRUE)

  # Calculate Moran I
  moran_i_res <- moran.test(pull(hexagons_sf, accidents), weights_res, zero.policy = TRUE)


  N=nrow(hexagons_sf)
  m=7.0131
  b=-2.188
  p=0.516
  a=1.287
  B0=5.319
  B1=-5.532


  smaup_estimate = (1/(1+exp(b+m*N)))/(1+p*(N^a)*exp((B0+B1*N)*moran_i_res[[c(3, 1)]]))

  # Return the results
  list(
    resolution = res,
    moran_estimate = moran_i_res[[c(3, 1)]],
    p_value = moran_i_res$p.value,
    smaup_estimate = smaup_estimate,
    data_points = N
    )
}


results_smaup <- map_df(4:13, calculate_smaup)

# View the results
print(results_smaup)


# 08 Impact analysis on local moran when introducing random outliers -----

data(road_safety_greater_manchester, package = "h3")
road_safety_greater_manchester = as.data.frame.matrix(road_safety_greater_manchester)
road_safety_greater_manchester$is_outlier = FALSE

gmanchester_sf = st_read("https://raw.githubusercontent.com/OpenDataManchester/gm-hex-map/master/gmauthorities.geojson") %>%
  st_transform(crs = projcrs) %>%
  st_as_sf()

# Function to generate random points within boundaries
# TODO Try different smpling method: https://dickbrus.github.io/SpatialSamplingwithR/
generate_random_points <- function(n, boundaries, type = "random") {
  st_sample(boundaries, size = n, type = type) %>%
    st_as_sf() %>%
    st_coordinates() %>%
    as.data.frame()
}

# Function to add random outliers to your data
add_sp_outliers_to_data <- function(data, boundaries, n_outliers) {
  random_points <- generate_random_points(n_outliers, boundaries)
  colnames(random_points) <- c("lng", "lat") # Ensure these match your data's column names
  outlier_data <- data.frame(lat = random_points$lat, lng = random_points$lng, is_outlier = TRUE)
  rbind(data, outlier_data)
}


# Example of using the function in a simulation
simulate_with_outliers <- function(resolution, iterations, n_outliers) {
  results <- vector("list", iterations)
  for (i in 1:iterations) {
    # Add random outliers
    modified_data <- add_sp_outliers_to_data(road_safety_greater_manchester, gmanchester_sf, n_outliers)

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





## [FURTHER] see how model coefficients change across different resolutions
## i dont have any covariate



## [FURTHER] cool also to see with different type of grids like I did in app2
## to show how h3 is not doing its job
## semi-done + implement custom h3 res choice wrt data, calculate which custom resolution would
## instead beneficial
## approfondendo c'è anche la mistura Compacted (sparse) cioè usare risoluzioni diverse per alcune zone
## più larghe quando meno dense, più fini quando cluster



## [FURTHER] look math behind hexagons
## https://github.com/r-spatial/sf/issues/1505



