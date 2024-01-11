ggplot() +
  geom_sf(data=map) +
  geom_sf(data=base_hmaps[[res]], size=0.8)

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

# Read the map of Italy
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
    h3_to_geo_boundary_sf()
}

# for res 0 (i.e. ground thruth)
N = nrow(gs_mean_prices_intersect_sf)
listw_points <- nb2listw(knn2nb(knearneigh(st_coordinates(gs_mean_prices_intersect_sf), k = 5)))
moran_ground_thruth <- moran.mc(gs_mean_prices_intersect_sf$prezzo_medio_trimestre, listw_points, 999)

# TODO
# - ha senso calcolare il prezzo mediano per tempo (e compararlo col prezzo mediano per zona)
ground_thruth_mean = data.frame(
  resolution = 0,
  stat_type = NA_character_,
  moran_estimate = moran_ground_thruth$statistic,
  p_value = moran_ground_thruth$p.value,
  data_points = N
)

simulate_moran_bias_per_res <- function(res,
                                        points = gs_mean_prices_intersect_sf,
                                        stat_type = "mean",
                                        quiet = FALSE){

  hmap <- base_hmaps[[res]]

  tic(msg = paste("calculating", stat_type, "per hex at res:", res , "... \n"))
  values <- st_join(st_as_sf(points) %>%
                      st_transform(4326), hmap, join = st_within) %>%
    group_by(h3_index) %>%
    summarise(
      stat = compute_stat(prezzo_medio_trimestre, stat_type)
      ) %>%
    st_drop_geometry() %>%
    filter(!is.na(h3_index))
  toc(quiet = quiet)

  tic("joining values on hmap")
  hmap <- hmap %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(stat))
  toc(quiet = quiet)

  N = nrow(hmap)

  if (N == 1){
    return(
      data.frame(
        resolution = res_vector[[res]],
        stat_type = stat_type,
        moran_estimate = NA_integer_,
        p_value = NA_integer_,
        data_points = N
      )
    )

  } else {

    tic("building neighbour mat...")
    nb <- poly2nb(hmap, queen=T)
    suppressWarnings(nb <- mstconnect(hmap, nb, distance = "centroid"))
    W_hmap <- nb2listw(nb)
    toc(quiet = quiet)

    tic(paste("calculating Moran's I index per res", res," when data points are:", N ))
    if( 2*N < 999 ){
      cat("max num nsim available", 2*N, "while N is ", N)
      moran <- moran.mc(values$stat, W_hmap, nsim = 2*N)
    } else {
      moran <- moran.mc(values$stat, W_hmap, nsim = 999)
    }
    toc(quiet = quiet)

    return(
      data.frame(
        resolution = res_vector[[res]],
        stat_type = stat_type,
        moran_estimate = moran$statistic,
        p_value = moran$p.value,
        data_points = N, row.names = NULL
      )
    )
  }

}

N = nrow(gs_mean_prices_intersect_sf)
listw_points <- nb2listw(knn2nb(knearneigh(st_coordinates(gs_mean_prices_intersect_sf), k = 5)))
moran_groud_thruth <- moran.mc(gs_mean_prices_intersect_sf$prezzo_medio_trimestre, listw_points, 999)

ground_thruth = data.frame(
  resolution = 0,
  stat_type = "mean",
  moran_estimate = moran_groud_thruth$statistic,
  p_value = moran_groud_thruth$p.value,
  data_points = N
)

# TODO fallo per tutte le risoluzioni
# - fallo per tutte le risoluzioni

plan(multicore, workers = parallel::detectCores())

res_vector = 1:8
sim_plan = expand_grid(
  res_vector = res_vector,
  stat = c("mean", "median")
)

# build maps
base_hmaps <- future_map(res_vector, gen_hmap, map=map)

# for 8 res takes 10 minutes (in parallel)
tic(msg = "start simulation")
results_df <- sim_plan %>%
  future_map2_dfr(
    .x = res_vector,
    .y = stat,
    .f =  ~simulate_moran_bias_per_res(res = .x, stat_type = .y)
    )
toc()

saveRDS(results_df, paste0("data/bias_results_1to", max(res_vector), ".rds"))

final <- bind_rows(ground_thruth, results_df)

# stop parallel workers
plan(sequential)

# Calculate the ground truth (Moran's estimate at resolution 0)
ground_truth_moran_estimate <- final$moran_estimate[final$resolution == 0]

# Calculate bias and relative bias (excluding resolution 0, 1 and 2)
final <- final %>%
  # Leave ground_truth, 1 and 2 behind since they are not relevant
  filter(resolution != 0 & resolution != 1 & resolution != 2) %>%
  mutate(
    bias = moran_estimate - ground_truth_moran_estimate,
    relative_bias = bias / abs(ground_truth_moran_estimate)
  ) %>%
  gather(key = "metric", value = "value", bias, relative_bias) %>%
  mutae


# metric_names <- list(
#   "bias" = TeX("$\textbf{B}$"),
#   "relative_bias" = TeX("$\textbf{RB}$")
# )

metric_names <- list(
  "bias" = md("**R**"),
  "relative_bia" =  md("**RB**")
)

# Plotting with faceting
# TODO change facet labels
bias_plot = ggplot(final, aes(x = factor(resolution), y = value, group = resolution)) +
  facet_wrap(~ metric, scales = "free_y", labeller = as_labeller(metric_names)) +
  geom_line(aes(y = value, group =metric )) +
  geom_point(aes(y = value, group =metric), alpha= .3) +
  ylab("Value") +
  xlab("Resolution") +
  ggtitle("Bias and Relative Bias per Resolution") +
  theme_minimal() +
  guides(size = guide_legend("Data Points"))

ggsave(plot = bias_plot, filename = "images/bias/bias_plot.png")


