## "2023-12-13 19:44:57 CET"
source(here("notebooks", "utils.R"))

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

# plot mean prices per H3 giving res
plot_res_3 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 3)$plot
plot_res_4 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 4)$plot
plot_res_5 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution =5)$plot
plot_res_6 <- generate_h3_plot(gs_mean_prices_intersect_sf, resolution = 6)$plot


# plot concentration of gas stations at different res
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

# 06 Calculate Moran I for each resolution and compare with ground truth  ----

# Calculate Moran base case:
listw_points <- nb2listw(knn2nb(knearneigh(st_coordinates(gs_mean_prices_intersect_sf))))
moran_point <- moran.test(gs_mean_prices_intersect_sf$prezzo_medio, listw_points)


# Using as data gs_mean_prices_intersect_sf
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
moran_results <- map_df(4:13, ~calculate_moran_i(res =.x))

# TODO save moran_results
# saveRDS(moran_results, "data/moran_results.rds")
moran_results = read_rds("data/moran_results.rds")

moran_results$moran_point = moran_point[[c(3, 1)]]

# calculate bias and represent it
moran_bias = moran_results %>%
  mutate(
    moran_bias = moran_point - moran_estimate,
    moran_abs_bias  = abs(moran_bias),
    moran_rel_bias = moran_point - moran_estimate/ moran_point
    )

# visualise bias




