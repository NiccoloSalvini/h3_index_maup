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
library(sf)
library(rgeoda)
library(tmap)
library(RColorBrewer)

# 1 path to save and read proproc file
lisa_plot_path = here("images", "lisa_plots")
gs_mean_prices_intersect_sf =  read_rds(here("data",  "gs_mean_prices_intersect_sf.rds"))


##
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

resolutions <- 4:13

h3_hexagons_sf_list <- create_hexagon_sf_list(gs_mean_prices_intersect_sf, resolutions)

# takes moran result and produce scatterplot
create_moran_scatterplot <- function(moran_result, data) {
  ggplot(data, aes(x = prezzo_medio_per_res, y = moran_result$residuals)) +
    geom_point() +
    xlab("Variable of Interest") +
    ylab("Spatially Lagged Variable") +
    ggtitle("Moran Scatterplot")
}

## function extracteed from
## https://spatialanalysis.github.io/handsonspatialdata/local-spatial-autocorrelation-1.html

lisa_map <- function(df, alpha = .05) {

  res = unique(pull(df, resolution))

  w <- queen_weights(df)
  lisa <- local_moran(w, df['prezzo_medio_per_res'])
  clusters <- lisa_clusters(lisa, cutoff = alpha)
  labels <- lisa_labels(lisa)
  pvalue <- lisa_pvalues(lisa)
  colors <- lisa_colors(lisa)
  lisa_patterns <- labels[clusters+1]

  pal <- match_palette(lisa_patterns,labels,colors)
  labels_name <- labels[labels %in% lisa_patterns]

  df["lisa_clusters"] <- clusters

  plt = tm_shape(df) +
    tm_fill("lisa_clusters",
            breaks = c(1, 2, 3, 4, 5, 6),
            title = "",
            palette =  c("red", "blue", "lightpink", "skyblue2", "white"),
            labels = c("High-High", "Low-Low", "High-Low",
                                "Low-High", "Non-significant")) +
    tm_legend(text.size = 1) +
    tm_borders(alpha = 0.5) +
    tm_layout(
      title = paste("LISA per H3, res: ", res),
      legend.outside = TRUE)

  tmap_save(tm = plt, paste0(lisa_plot_path, "/lisa_plot_res_", res, ".png", collapse = "/"))

  return(plt)

}


map(map(h3_hexagons_sf_list, 1), .f = lisa_map )


# questa al posto dello scatter plot con la linea
significance_map <- function(df, permutations = 999, alpha = .05) {

  res = unique(pull(df, resolution))
  w <- queen_weights(df)
  lisa <- local_moran(w, df['prezzo_medio_per_res'])

  pvalue <- lisa_pvalues(lisa)
  target_p <- 1 / (1 + permutations)
  potential_brks <- c(.00001, .0001, .001, .01)
  brks <- potential_brks[which(potential_brks > target_p & potential_brks < alpha)]
  brks2 <- c(target_p, brks, alpha)
  labels <- c(as.character(brks2), "Not Significant")
  brks3 <- c(0, brks2, 1)

  cuts <- cut(pvalue, breaks = brks3,labels = labels)
  df["sig"] <- cuts

  pal <- rev(brewer.pal(length(labels), "Greens"))
  pal[length(pal)] <- "#D3D3D3"

  plt_sign =  tm_shape(df) +
    tm_fill("sig", palette = pal, title =  "") +
    tm_borders(alpha = .5) +
    tm_layout(title = paste("LM Map per h3", res, collapse = " "), legend.outside = TRUE)

  tmap_save(tm = plt_sign, paste0(lisa_plot_path, "/significance/local_moran_res_", res, ".png", collapse = "/"))

  return(plt_sign)
}


map(map(h3_hexagons_sf_list, 1), .f = ~significance_map(df = .x,permutations = 999) )



## calculare local moran I (questa è da implementare)
# calculate_moran_i <- function(data = gs_mean_prices_intersect_sf, res) {
#
#   h3_indices <- geo_to_h3(data, res = res)
#
#   # Create a table of counts per hex
#   tbl_res <- data %>%
#     mutate(h3_indices = h3_indices) %>%
#     dplyr::group_by(h3_indices) %>%
#     summarise(
#       n = n(),
#       prezzo_medio_per_res = mean(prezzo_medio)
#     )
#
#   # Create sf hexagons
#   hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_indices) %>%
#     dplyr::mutate(
#       count = tbl_res$n,
#       prezzo_medio_per_res = tbl_res$prezzo_medio_per_res,
#       resolution = as.factor(res)
#     )
#
#   N = nrow(hexagons_sf)
#
#   # Find neighbours
#   cat("calculating for res:", res , " \n")
#   hexagons_nb <- poly2nb(hexagons_sf, queen=TRUE)
#
#   # Convert to listw
#   weights_res <- nb2listw(hexagons_nb, style = "W", zero.policy = TRUE)
#
#   # Calculate Moran I
#   moran_i_res <- moran.test(hexagons_sf$prezzo_medio_per_res, weights_res, zero.policy = TRUE)
#
#   # moran_plot = moran.plot(hexagons_sf$accidents,labels = hexagons_sf$h3_index, weights_res, pch=19)
#   scatterplot <- create_moran_scatterplot(moran_i_res, data)
#
#   # Return the results
#   list(
#     resolution = res,
#     moran_estimate = moran_i_res[[c(3, 1)]],
#     p_value = moran_i_res$p.value,
#     data_points = N,
#     scatter = scatterplot
#     # moran_plot = moran_plot
#   )
# }

