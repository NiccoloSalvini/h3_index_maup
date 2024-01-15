# Required libraries
library(sf)
library(spdep)
library(h3)
library(ggplot2)
library(dplyr)
library(parallel)
library(igraph)
library(patchwork)
library(scales)
library(furrr)
library(parallel)
library(tictoc)
library(latex2exp)
library(rgeoda)
library(tmap)
library(RColorBrewer)
library(readr)
library(purrr)


# 1 path to save and read proproc file ----
lisa_plot_path = here("images", "lisa_plots")
map <- read_sf("data/ProvCM01012023_g/ProvCM01012023_g_WGS84.shp")
gs_mean_prices_intersect_sf =  read_rds(here("data",  "gs_mean_prices_intersect_sf.rds")) %>%
  rename(prezzo_medio_trimestre = prezzo_medio)


compute_stat <- function(y, stat_type) {
  switch(stat_type,
         "mean" = {
           mean(y, na.rm = TRUE)
         },
         "median" = {
           median(y, na.rm = TRUE)
         },
         stop(paste0("No statistics for ", stat_type))
  )
}

# Function to generate hexagonal maps
gen_hmap <- function(map, res=1){
  map %>%
    st_union() %>%
    st_transform(4326) %>%
    # from h3 pkg fills with hexs of a given res poly
    polyfill(res = res) %>%
    h3_to_geo_boundary_sf() %>%
    mutate(res = res)
}


match_palette <- function(patterns, classifications, colors){
  classes_present <- base::unique(patterns)
  mat <- matrix(c(classifications,colors), ncol = 2)
  logi <- classifications %in% classes_present
  pre_col <- matrix(mat[logi], ncol = 2)
  pal <- pre_col[,2]
  return(pal)
}

# test
# hex_map = gen_hmap(map = map, res = 5)

# 2 function to create sf each H3 res (you got also data points) ----
# create_hexagon_sf_list <- function(data, resolutions) {
#   lapply(resolutions, function(res) {
#     # Convert geo data to h3 indices
#     h3_indices <- geo_to_h3(data, res = res)
#
#     # Create a table of counts per hex
#     tbl_res <- data %>%
#       mutate(h3_indices = h3_indices) %>%
#       dplyr::group_by(h3_indices) %>%
#       summarise(
#         n = n(),
#         prezzo_medio_per_res = mean(prezzo_medio)
#       )
#
#     # Create sf hexagons
#     hexagons_sf <- h3_to_geo_boundary_sf(tbl_res$h3_indices) %>%
#       dplyr::mutate(
#         count = tbl_res$n,
#         prezzo_medio_per_res = tbl_res$prezzo_medio_per_res,
#         resolution = as.factor(res)
#       )
#
#     # Add original data points
#     data_points_sf <- st_as_sf(data, coords = c("X", "Y"), crs = 4326) %>%
#       mutate(resolution = as.character(res))
#
#     return(list(hexagons = hexagons_sf, points = data_points_sf))
#
#     # return(hexagons_sf)
#   })
# }

# 3 choose res vector and map through each resolution -----
plan(multisession, workers = parallel::detectCores())

res_vector = 3:8

# build maps
base_hmaps <- future_map(res_vector, gen_hmap, map=map)

# stop paralelle  backend
plan(sequential)

# save rds for res untill 8
# saveRDS(base_hmaps, "data/base_hmaps.rds")
base_hmaps = read_rds("data/base_hmaps.rds")

# 4 LISA plot via `rgeoda` ----
## https://spatialanalysis.github.io/handsonspatialdata/local-spatial-autocorrelation-1.html
lisa_map <- function(hex_map,
                     points = gs_mean_prices_intersect_sf,
                     stat_type = "mean",
                     alpha = .05) {

  res = unique(pull(hex_map, res))
  values <- st_join(st_as_sf(points) %>%
                      st_transform(4326), hex_map, join = st_within) %>%
    group_by(h3_index) %>%
    summarise(
      stat = compute_stat(prezzo_medio_trimestre, stat_type)
    ) %>%
    st_drop_geometry() %>%
    filter(!is.na(h3_index))

  hex_map <- hex_map %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(stat))

  w <- queen_weights(hex_map)
  lisa <- local_moran(w, hex_map['stat'])
  clusters <- lisa_clusters(lisa, cutoff = alpha)
  labels <- lisa_labels(lisa)
  pvalue <- lisa_pvalues(lisa)
  colors <- lisa_colors(lisa)
  lisa_patterns <- labels[clusters+1]

  pal <- match_palette(lisa_patterns,labels,colors)
  labels <- labels[labels %in% lisa_patterns]

  hex_map["lisa_clusters"] <- clusters

  plt = tm_shape(hex_map) +
    tm_fill(
      "lisa_clusters",
      title = "",
      labels = labels,
      palette = pal,
      style = "cat"
      ) +
    tm_borders(alpha = 0.2 ) +
    tm_layout(
      title = paste("LISA per H3, res: ", res),
      legend.outside = TRUE,
      frame = F
  )

  # plt = tm_shape(hex_map) +
  #   tm_fill("lisa_clusters") +
  #           # breaks = c(1, 2, 3, 4, 5, 6),
  #           # title = "",
  #           # palette =  c("red", "blue", "lightpink", "skyblue2", "white"),
  #           # labels = c("High-High", "Low-Low", "High-Low",
  #           #                     "Low-High", "Non-significant")) +
  #   tm_legend(text.size = 1) +
  #   tm_borders(alpha = 0.5) +
  #   tm_layout(
  #     title = paste("LISA per H3, res: ", res),
  #     legend.outside = TRUE)
  cat("generating plot for res:", res, "\n")
  tmap_save(tm = plt, paste0(lisa_plot_path, "/lisa_plot_res_", res, ".png", collapse = "/"))

  return(plt)

}

# RUN!
map(base_hmaps, .f = lisa_map)


# 5 LISA Significance map ----
#
significance_map <- function(hex_map, points = gs_mean_prices_intersect_sf  ,
                             permutations = 999, alpha = .05, stat_type = "mean") {

  res = unique(pull(hex_map, res))
  values <- st_join(st_as_sf(points) %>%
                      st_transform(4326), hex_map, join = st_within) %>%
    group_by(h3_index) %>%
    summarise(
      stat = compute_stat(prezzo_medio_trimestre, stat_type)
    ) %>%
    st_drop_geometry() %>%
    filter(!is.na(h3_index))

  hex_map <- hex_map %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(stat))

  w <- queen_weights(hex_map)
  lisa <- local_moran(w, hex_map['stat'])

  pvalue <- lisa_pvalues(lisa)
  target_p <- 1 / (1 + permutations)
  potential_brks <- c(.00001, .0001, .001, .01)
  brks <- potential_brks[which(potential_brks > target_p & potential_brks < alpha)]
  brks2 <- c(target_p, brks, alpha)
  labels <- c(as.character(brks2), "Not Significant")
  brks3 <- c(0, brks2, 1)

  cuts <- cut(pvalue, breaks = brks3,labels = labels)
  hex_map["sig"] <- cuts

  pal <- rev(brewer.pal(length(labels), "Greens"))
  pal[length(pal)] <- "#D3D3D3"

  plt_sign =  tm_shape(hex_map) +
    tm_fill("sig", palette = pal, title =  "") +
    tm_borders(alpha = .2) +
    tm_layout(
      title = paste("Significance h3 res:", res, collapse = " "),
      legend.outside = TRUE,
      frame = F
      )

  cat("generating significance map per h3 res:", res, "\n")
  tmap_save(tm = plt_sign, paste0(lisa_plot_path, "/significance/local_moran_res_", res, ".png", collapse = "/"))

  return(plt_sign)
}

# RUN!
map(base_hmaps, .f = ~significance_map(hex_map = .x) )

# paralle backend
plan(sequential)



# lisa resolution 0 contro lisa res x miss classification.
# perc per area
