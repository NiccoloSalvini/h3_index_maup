# prova a sfruttare LISA
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
library(Matrix)


# 1 path to save and read proproc file ----
lisa_plot_path = here("images", "lisa_plots")
map <- read_sf("data/ProvCM01012023_g/ProvCM01012023_g_WGS84.shp")
# gs_mean_prices_intersect_sf =  read_rds(here("data",  "gs_mean_prices_intersect_sf.rds")) %>%
#   rename(prezzo_medio_trimestre = prezzo_medio)


# 2 functions ----
mstconnect <- function(polys, nb, distance="centroid"){

  if(distance == "centroid"){
    coords = sf::st_coordinates(sf::st_centroid(sf::st_geometry(polys)))
    dmat = as.matrix(dist(coords))
  }else if(distance == "polygon"){
    dmat = sf::st_distance(polys) + 1 # offset for adjacencies
    diag(dmat) = 0 # no self-intersections
  }else{
    stop("Unknown distance method")
  }
  gfull = igraph::graph.adjacency(dmat, weighted=TRUE, mode="undirected")
  gmst = igraph::mst(gfull)
  edgemat = as.matrix(igraph::as_adj(gmst))
  edgelistw = spdep::mat2listw(edgemat, style = "M")
  edgenb = edgelistw$neighbour
  attr(edgenb,"region.id") = attr(nb, "region.id")
  allnb = spdep::union.nb(nb, edgenb)
  allnb
}

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

gen_points <- function(base_map, rho=0){
  nb <-poly2nb(base_map, queen=T)
  nb <- mstconnect(base_map, nb)

  W_mat<-nb2mat(nb)
  W <- nb2listw(nb)
  #I<-diag(1,nrow(base_map), nrow(base_map))
  y <- rnorm(nrow(base_map), mean=0, sd=1)
  #y <- solve(I-rho*W_mat)%*%(y)
  #y <- solve(I - rho * W_mat, y)

  W_mat_sparse <- as(W_mat, "sparseMatrix")

  # The identity matrix should also be a sparse matrix for efficient computation
  I_sparse <- Diagonal(x = rep(1, nrow(W_mat_sparse)))

  # Solve the linear system (I - rho * W_mat) * x = y using sparse matrix operations
  # The solve function from the Matrix package efficiently handles sparse matrices
  y <- solve(I_sparse - rho * W_mat_sparse, y)

  y <- as.numeric((y-mean(y))/sd(y))
  return(list(y = y, W=W))
}

match_palette <- function(patterns, classifications, colors){
  classes_present <- base::unique(patterns)
  mat <- matrix(c(classifications,colors), ncol = 2)
  logi <- classifications %in% classes_present
  pre_col <- matrix(mat[logi], ncol = 2)
  pal <- pre_col[,2]
  return(pal)
}

get_vals_and_clusters_from_map <- function(hmap_obj) {
  N = nrow(hmap_obj)
  w <- queen_weights(hmap_obj)
  nb <-poly2nb(hmap_obj, queen=T)
  nb <- mstconnect(hmap_obj, nb)
  W_hmap <- nb2listw(nb, zero.policy = TRUE)
  moran <- moran.mc(hmap_obj$y, W_hmap, nsim = 2*N)
  lisa <- local_moran(w, hmap_obj['y'])
  clusters <- lisa_clusters(lisa, cutoff = alpha)
  labels <- lisa_labels(lisa)
  lisa_vals = lisa_values(lisa)
  pvalue <- lisa_pvalues(lisa)
  colors <- lisa_colors(lisa)
  lisa_patterns <- labels[clusters+1]

  pal <- match_palette(lisa_patterns,labels,colors)
  labels <- labels[labels %in% lisa_patterns]

  hmap_obj["lisa_clusters"] <- clusters
  hmap_obj["lisa_vals"] = lisa_vals

  return(hmap_obj)
}



# 3 choose res vector and map through each resolution -----
plan(multisession, workers = parallel::detectCores())

res_vector <- 3:5
rhos <- c(0, 0.2, 0.4, 0.6, 0.8)
num_sim <- 100
sim_id = 1:num_sim
base_hmaps <- future_map(res_vector, gen_hmap, map=map)
base_map <- base_hmaps[[3]]

simulation_grid = crossing(res_vector, rhos, sim_id)

# save rds for res untill 8
# saveRDS(base_hmaps, "data/base_hmaps.rds")
# tutte le mappe già generate
# base_hmaps = read_rds("data/base_hmaps.rds")

# 4 LISA plot via `rgeoda` ----
## https://spatialanalysis.github.io/handsonspatialdata/local-spatial-autocorrelation-1.html
lisa_map <- function(res,
                     rho,
                     stat_type = "mean",
                     alpha = .05,
                     plt_show = F,
                     save_option = F) {

  hmap <- base_hmaps[[res]]

  # res = unique(h3_get_resolution(pull(base_hmaps[[res]], h3_index)))

  points <- gen_points(base_map, rho)

  values <- base_map %>%
    mutate(y= points$y) %>%
    st_centroid() %>%
    select(-h3_index) %>%
    st_join(hmap, join = st_within) %>%
    st_drop_geometry() %>%
    group_by(h3_index) %>%
    summarise(y = mean(y, na.rm = TRUE)) %>%
    st_drop_geometry() %>%
    filter(!is.na(h3_index))


  hex_map_5 <- base_hmaps[[3]] %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(y))

  hex_map_5 = get_vals_and_clusters_from_map(hex_map_5) %>%
    mutate(
      hex_index_4 = map_chr(h3_index, ~h3_to_parent(.x, res = 4)),
      hex_index_3 = map_chr(h3_index, ~h3_to_parent(.x, res = 3)),
    ) %>%
    group_by(hex_index_4) %>%
    mutate(
      lisa_vals_4 = mean(lisa_vals)
    ) %>%
    ungroup(hex_index_4) %>%
    group_by(hex_index_3) %>%
    mutate(
      lisa_vals_3 = mean(lisa_vals)
    ) %>%
    group_by(h3_index, hex_index_3, hex_index_4) %>%
    summarise(
      lisa_vals_per_hex_res_3 = mean(lisa_vals_3),
      lisa_vals_per_hex_res_4 = mean(lisa_vals_4),
      lisa_vals_per_hex_res_5 = mean(lisa_vals)
     )

  hex_map <- hmap %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(y))

  hex_map = get_vals_and_clusters_from_map(hex_map)

  if(plt == TRUE){
    plt_vals = tm_shape(hex_map) +
      tm_fill(
        "lisa_vals",
        title = "",
        midpoint = NA,
        palette = tmaptools::get_brewer_pal("YlOrRd", n = 6)
      ) +
      tm_borders(alpha = 0.2 ) +
      tm_layout(
        title = paste("LISA vals per H3, res: ", res),
        legend.outside = TRUE,
        frame = F
      )

    plt_vals_hex_5 = tm_shape(hex_map_5) +
      tm_fill(
        "lisa_vals",
        title = "",
        midpoint = NA,
        palette = tmaptools::get_brewer_pal("YlOrRd", n = 6)
      ) +
      tm_borders(alpha = 0.2 ) +
      tm_layout(
        title = paste("LISA vals per H3, res: ", res),
        legend.outside = TRUE,
        frame = F
      )

    composed_plt = tmap_arrange(plt_vals, plt_vals_hex_5)

    tmap_save(tm = plt, paste0(lisa_plot_path, "/lisa_plot_res_", res, ".png", collapse = "/"))
    return(plt_vals)
  }

  obj = list(values = lisa$lisa_vals,  pvalues = pvalue, morans_i =moran, res = res)

  cat("generated LISA plot for res:", res, "\n")

  return(obj)

}

pmap_dfr(simulation_grid, lisa_map)

# RUN!
lisas = map(base_hmaps[-1], .f = lisa_map)

## represent loss in spatial aggregation
calc_loss <- function(obj) {
  avg_lisa = mean(obj$values)
  actual_moran = obj$morans_i$statistic
  res = obj$res
  vec = tibble(
    avg_lisa = avg_lisa,
    actual_moran = actual_moran,
    res = res
  )

  return(vec)
}

df_comparisons = purrr::map_dfr(lisas, calc_loss)
df_comparisons %>%
  mutate(
    diff = abs(avg_lisa - actual_moran )
  )



## prima prova a vedere cosa succede sneza simulare per ogni mappa ripeti
rho = .8
points <- gen_points(base_map, rho)

values <- base_map %>%
  mutate(y= points$y) %>%
  st_centroid() %>%
  select(-h3_index) %>%
  st_join(base_hmaps[[3]], join = st_within) %>%
  st_drop_geometry() %>%
  group_by(h3_index) %>%
  summarise(y = mean(y, na.rm = TRUE)) %>%
  st_drop_geometry() %>%
  filter(!is.na(h3_index))

hex_map_5 <- base_hmaps[[3]] %>%
  left_join(values, by = join_by(h3_index)) %>%
  filter(!is.na(y))

N = nrow(hex_map_5)
w <- queen_weights(hex_map_5)
nb <-poly2nb(hex_map_5, queen=T)
nb <- mstconnect(hex_map_5, nb)
W_hmap <- nb2listw(nb, zero.policy = TRUE)
moran <- moran.mc(hex_map_5$y, W_hmap, nsim = 2*N)
lisa <- local_moran(w, hex_map_5['y'])
lisa$lisa_vals %>%  length()
hex_map_5$lisa_vals = lisa$lisa_vals
all_indexes = hex_map_5 %>%
  mutate(
    hex_index_4 = map_chr(h3_index, ~h3_to_parent(.x, res = 4)),
    hex_index_3 = map_chr(h3_index, ~h3_to_parent(.x, res = 3)),
  ) %>%
  group_by(hex_index_4) %>%
  mutate(
    lisa_vals_4 = mean(lisa_vals)
  ) %>%
  ungroup(hex_index_4) %>%
  group_by(hex_index_3) %>%
  mutate(
    lisa_vals_3 = mean(lisa_vals)
  ) %>%
  group_by(h3_index, hex_index_3, hex_index_4) %>%
  summarise(
    lisa_vals_per_hex_res_5 = mean(lisa_vals),
    lisa_vals_per_hex_res_4 = mean(lisa_vals_4),
    lisa_vals_per_hex_res_3 = mean(lisa_vals_3),

  )

morans_i_res_5 = mean(all_indexes$lisa_vals_per_hex_res_5);morans_i_res_5
morans_i_res_4 = mean(all_indexes$lisa_vals_per_hex_res_4);morans_i_res_4
morans_i_res_3 = mean(all_indexes$lisa_vals_per_hex_res_3);morans_i_res_3

## fai vedere le differenze di colore del lisa generato da matrice W
## o del lisa generato

## prova per ground thruth

## prova implementazione tutta in spdep per evitare cazzate across pkgs

