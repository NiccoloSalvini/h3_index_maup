library(sf)
library(spdep)
library(h3)
library(ggplot2)
library(dplyr)
library(parallel)
library(igraph)
library(patchwork)
library(scales)
library(rgeoda)

source("notebooks/utils.R")

# Assuming map and other required data are already loaded
# Generate simulated points
simulated_data <- gen_points(map, size=500, type="random", rho=0)

# Calculate Global Moran's I
W_global <- nb2listw(knn2nb(knearneigh(st_coordinates(simulated_data$points), k = 5)))
global_moran <- moran.mc(simulated_data$points$y, W_global, 999)$statistic

# Calculate Local Moran's I for each resolution
local_morans <- lapply(base_hmaps, function(hex_map) {
  # Assuming `y` is the variable of interest in `simulated_data$points`
  values <- st_join(st_as_sf(simulated_data$points) %>% st_transform(4326), hex_map , join = st_within) %>%
    # group_by(h3_index) %>%
    # summarise(y = mean(y, na.rm = TRUE)) %>%
    st_drop_geometry() %>%
    filter(!is.na(h3_index))

  hex_map <- hex_map %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(y))

  W_global <- nb2listw(knn2nb(knearneigh(st_coordinates(hex_map), k = 5)))
  global_moran <- moran.mc(hex_map$y, W_global, 999)$statistic

  W_local <- queen_weights(hex_map)
  lisa <- local_moran(W_local, hex_map["y"])
  lisa_values_per_h3 = lisa_values(lisa)
  hex_map["local_moran"] <- lisa_values_per_h3
  hex_map["global_moran"] <- global_moran

  hex_map
})


tm_shape(local_morans[[1]]) +
  tm_fill(
    "lisa_values",
    title = "",
    style = "cat"
  ) +
  tm_borders(alpha = 0.2 ) +
  tm_layout(
    legend.outside = TRUE,
    frame = F
  )

# Develop and Apply a Reweighting Scheme
sum_local_morans <- sapply(local_morans, function(lm) sum(lm$lisa_values))
scaling_factors <- global_moran$I / sum_local_morans

adjusted_local_morans <- mapply(function(lm, scale) lm$I * scale, local_morans, scaling_factors, SIMPLIFY = FALSE)

reweighted_data <- lapply(1:length(base_hmaps), function(i) {
  hex_map <- base_hmaps[[i]]
  lm_values <- adjusted_local_morans[[i]]
  hex_map$adjusted_local_moran <- lm_values
  return(hex_map)
})



## the idea instead of having a row standardised W matrix you need to readapt it  form prev resolitions
## bring in back something like a weight (a scale factor for each observation)


## now with thbe book exercise
library(pheatmap)
vec = c(5,4,6,13,12,20,11,31,31,34,18,38,25,37,41,33)
ex_mat = matrix(vec,nrow = 4, ncol = 4, byrow = T)
ex_mat = (ex_mat - mean(ex_mat))/sd(ex_mat)
pheatmap(
  ex_mat,
  display_numbers = T,
  color = colorRampPalette(c('white','red'))(100),
  cluster_rows = F,
  cluster_cols = F,
  fontsize_number = 15,
  legend = F
  )


library(sf)
st_make_grid(x = st_as_sf(as.data.frame.matrix(ex_mat)), cellsize = 0.1, what = "centers")
sfc = st_sfc(st_polygon(list(ex_mat)))
library(rgeoda)
w_ex =queen_weights(ex_mat)


f2_1

df <- st_sf(id = 1:nrows, geometry = st_sfc(lapply(1:nrows, function(x) st_geometrycollection())))




## partiamo da vincenzo
gen_hmap <- function(map, res=1){
  map %>%
    st_union() %>%
    st_transform(4326) %>%
    polyfill(res = res) %>%
    h3_to_geo_boundary_sf()
}

gen_points <- function(map, size=500, type="random", rho=0,  mean=5, sd=1, seed = 123){

  set.seed(seed = seed)

  points <- st_sample(map, size = size, type = type, exact = TRUE) %>%
    st_as_sf()

  neighbors <- knn2nb(knearneigh(st_coordinates(points), k = 5))
  W <- nb2listw(neighbors, style="W")

  # i pesi sono sempre 0.2 perchè sono 5 i vicini
  W_mat<-nb2mat(neighbors)
  I<-diag(1,nrow(points), nrow(points))
  y <- rnorm(nrow(points), mean=mean, sd=sd)
  y <- solve(I-rho*W_mat)%*%(y)
  points$y <- y
  return(list(points = points, W=W, W_mat = W_mat))
}

prova = gen_points(map,  mean=10, sd=3)
apply( prova$W_mat , 1, sum)



# il focus è da risoluzione 6 a 3
res_vector <- 3:6
base_hmaps <- lapply(res_vector, gen_hmap, map=map)


plot_hmap_points <- function(hmap, map = map) {

  points_generated <- gen_points(map)

  values <- st_join(st_as_sf(points_generated$points) %>%
                      st_transform(4326), hmap, join = st_within) %>%
    filter(!is.na(h3_index))

  values_grouped = values %>%
    group_by(h3_index) %>%
    summarise(y = mean(y.y, na.rm = TRUE))


  hmap <- hmap %>%
    left_join(st_drop_geometry(values), by = join_by(h3_index)) %>%
    filter(!is.na(y))

  plt = ggplot() +
    geom_sf(data=hmap) +
    geom_sf(data=values, aes(size=y), alpha = .5) +
    geom_sf_text(data = st_cast(x = values, to = "POINT"), aes(label = round(y,3)), colour = "grey0", check_overlap = T) +
    scale_size(range = c(0.7, 3)) +
    theme_minimal() +
    theme(legend.position="none") +
    labs(
      x = "",
      y = ""
    )

  return(plt)
}

plot_hmap_points(hmap = base_hmaps[[1]], map = map)
plot_hmap_points(hmap = base_hmaps[[2]], map = map)
plot_hmap_points(hmap = base_hmaps[[3]], map = map)


hmap_res_3 = base_hmaps[[1]]
hmap_res_4 = base_hmaps[[2]]

points_generated <- gen_points(map)

values <- st_join(st_as_sf(points_generated$points) %>%
                    st_transform(4326), hmap_prova, join = st_within) %>%
  filter(!is.na(h3_index))

values_grouped = values %>%
  group_by(h3_index) %>%
  summarise(y = mean(y.y, na.rm = TRUE)) %>%
  filter(!is.na(h3_index))

hmap_prova <- hmap_prova %>%
  left_join(st_drop_geometry(values), by = join_by(h3_index)) %>%
  filter(!is.na(y))

plt = ggplot() +
  geom_sf(data=hmap_prova) +
  geom_sf(data=values, aes(size=y), alpha = .5) +
  geom_sf_text(data = st_cast(x = values, to = "POINT"), aes(label = round(y,3)), colour = "grey0", check_overlap = T) +
  scale_size(range = c(0.7, 3)) +
  theme_minimal() +
  theme(legend.position="none") +
  labs(
    x = "",
    y = ""
  )


# Hierarchical Weight Propagation
# l'altra idea è quella del grafo

# Function to Retrieve Child Hexagons
get_child_hexagons <- function(hexagon, higher_res) {
  h3_to_children(hexagon, higher_res)
}

# Function to Create Hierarchical Weight Matrix
create_hierarchical_weight_matrix <- function(hmap, higher_res) {
  hmap$h3_children <- sapply(hmap$h3_index, get_child_hexagons, higher_res)
  hmap$weights <- sapply(hmap$h3_children, function(children) rep(1/7, length(children)))
  return(hmap)
}

create_hierarchical_weight_matrix(base_hmaps[[1]], 3)




## riprovaci ora
get_neighbors <- function(hexagon) {
  # Retrieve all hexagons within 1 step of the given hexagon
  neighbors <- k_ring(hexagon,radius = 1 )

  # Remove the original hexagon from the list to get only the neighbors
  neighbors <- neighbors[neighbors != hexagon]

  return(neighbors)
}

# Function to retrieve child hexagons for a given hexagon at a lower resolution
get_child_hexagons <- function(parent_hex, higher_res) {
  h3_to_children(parent_hex, resolution = higher_res)
}

# Function to compute the weights for the lower resolution based on child hexagon values
compute_weights <- function(lower_res_hex, higher_res_hex_values) {
  child_hexes <- get_child_hexagons(lower_res_hex, higher_res)
  child_values <- higher_res_hex_values[child_hexes]
  weights <- child_values / sum(child_values) # Normalize the weights
  return(weights)
}

# Iterate over each hexagon at the lower resolution to build the weighted neighbor matrix
build_weighted_matrix <- function(lower_res_hexagons, higher_res_hex_values) {
  W_matrix <- matrix(0, nrow = length(lower_res_hexagons), ncol = length(lower_res_hexagons))

  for (i in seq_along(lower_res_hexagons)) {
    hexagon <- lower_res_hexagons[i]
    neighbors <- get_neighbors(hexagon) # This function should be defined to get neighboring hexagons
    weights <- compute_weights(hexagon, higher_res_hex_values)

    # Assign weights to the neighbor matrix
    for (j in seq_along(neighbors)) {
      W_matrix[i, neighbors[j]] <- weights[j]
    }
  }

  return(W_matrix)
}

# Example usage
lower_res_hexagons <- c(2,3,4) # Define or retrieve your lower resolution hexagons
higher_res_hex_values <- c(3,4,5) # Define or retrieve your higher resolution hexagon values
W_matrix <- build_weighted_matrix(lower_res_hexagons, higher_res_hex_values)

lower_res_hex = base_hmaps[[3]]
higher_res_hex_values = base_hmaps[[4]]


# ... per ogni eagono
# prendi gli esagoni più piccoli
# calcola la matrice di vicinanza W
# prendi l'esagono più grande, e prendi i figli degli esagoni più grandi
#
