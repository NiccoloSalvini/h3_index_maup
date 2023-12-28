# Impact analysis on global moran when introducing random outliers -----
gs_mean_prices_intersect_sf$is_outlier = FALSE

provinces_sf = st_read("data/provinces.geojson") %>%
  st_transform(crs = 4326) %>%
  st_as_sf()

# Function to generate random points within boundaries
# TODO Try different smpling method: https://dickbrus.github.io/SpatialSamplingwithR/
# lascialo a dopo
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

# Run Simulation. -----
# how to decide the number of outliers, is there any statistical way to see that?
# could be the case of generating outliers up to a point that Moran Estimates sensibilty changes
simulation_results <- map_df(4:13, ~simulate_with_outliers( resolution = .x,iterations = 10, n_outliers = 100))  # For example, 10 iterations with 5 outliers each


## Visualise newly rndm added points ----
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



##  Line Plot of Moran's I Estimate by Resolution ----
library(ggplot2)

ggplot(simulation_results, aes(x = resolution, y = moran_estimate, group = 1)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = 4:13) +
  labs(title = "Moran's I Estimate by Resolution",
       x = "Resolution",
       y = "Moran's I Estimate") +
  theme_minimal()


##  Scatter Plot of Moran's I Estimate and p-value by Resolution ----
ggplot(simulation_results, aes(x = moran_estimate, y = p_value, color = as.factor(resolution))) +
  geom_point() +
  geom_line()+
  labs(title = "Moran's I Estimate vs. p-value for Each Resolution",
       x = "Moran's I Estimate",
       y = "p-value",
       color = "Resolution") +
  theme_minimal()



## Faceted Plot for Each Resolution ----
ggplot(simulation_results, aes(x = as.factor(1:100), y = moran_estimate)) +
  geom_point() +
  facet_wrap(~resolution) +
  labs(title = "Moran's I Estimate Across Iterations for Each Resolution",
       x = "Iteration",
       y = "Moran's I Estimate") +
  theme_minimal()
