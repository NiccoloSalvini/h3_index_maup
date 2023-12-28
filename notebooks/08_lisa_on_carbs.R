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

# 1 path to save and read proproc file ----
lisa_plot_path = here("images", "lisa_plots")
gs_mean_prices_intersect_sf =  read_rds(here("data",  "gs_mean_prices_intersect_sf.rds"))


# 2 function to create sf each H3 res (you got also data points) ----
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

# 3 choose res vector and map through each resolution -----
resolutions <- 4:13

h3_hexagons_sf_list <- create_hexagon_sf_list(gs_mean_prices_intersect_sf, resolutions)

# 4 LISA plot via `rgeoda` ----
## function extracteed from
## https://spatialanalysis.github.io/handsonspatialdata/local-spatial-autocorrelation-1.html
## TODO da riguardare
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

# RUN!
map(map(h3_hexagons_sf_list, 1), .f = lisa_map )


# 5 LISA Significance map ----
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

# RUN!
map(map(h3_hexagons_sf_list, 1), .f = ~significance_map(df = .x,permutations = 999) )


# lisa resolution 0 contro lisa res x miss classification.
# perc per area
