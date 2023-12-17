library(sf)
library(spdep)
library(h3)
library(ggplot2)
library(dplyr)
library(parallel)
library(igraph)
library(patchwork)
library(scales)

map <- read_sf("data/ProvCM01012023_g/ProvCM01012023_g_WGS84.shp")
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

gen_hmap <- function(map, res=1){
  map %>%
    st_union() %>%
    st_transform(4326) %>%
    polyfill(res = res) %>%
    h3_to_geo_boundary_sf()
}

gen_points <- function(map, size=500, type="random", rho=0){
  points <- st_sample(map, size = size, type = type, exact = TRUE) %>%
    st_as_sf()

  neighbors <- knn2nb(knearneigh(st_coordinates(points), k = 5))
  W <- nb2listw(neighbors, style="W")
  W_mat<-nb2mat(neighbors)
  I<-diag(1,nrow(points), nrow(points))
  y <- rnorm(nrow(points), mean=5, sd=1)
  y <- solve(I-rho*W_mat)%*%(y)
  points$y <- y
  return(list(points = points, W=W))
}

simulate_res_impact <- function(res, points){
  hmap <- base_hmaps[[res]]
  values <- st_join(st_as_sf(points) %>%
                      st_transform(4326), hmap, join = st_within) %>%
    group_by(h3_index) %>%
    summarise(y = mean(y, na.rm = TRUE)) %>%
    st_drop_geometry() %>%
    filter(!is.na(h3_index))
  hmap <- hmap %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(y))

  nb <-poly2nb(hmap, queen=T)
  nb <- mstconnect(hmap, nb, distance = "centroid")
  #plot(nb, st_coordinates(st_centroid(hmap)))
  W_hmap <- nb2listw(nb)
  moran <- moran.mc(values$y, W_hmap, 999)
  return(data.frame(moran=moran$statistic, p=moran$p.value))
}


# Simulation ----
res_vector <- 3:6
rhos <- 0
rhos <- c(0, 0.2, 0.4, 0.6, 0.8)
types <- c("random", "regular")
num_sim <- 500
base_hmaps <- lapply(res_vector, gen_hmap, map=map)


simulation <- function(sim_id, rho, type){
  system(sprintf('echo "\n%s\n"', paste(sim_id, "-", rho, "-", type)))

  points_generated <- gen_points(map, rho=rho, type=type)
  points <- points_generated$points
  W <- points_generated$W
  moran <- moran.mc(points$y, W, 999)
  real <- data.frame("moran"=moran$statistic, "p"=moran$p.value)
  colnames(real) <- c("moran", "p")
  results <- lapply(1:length(res_vector), simulate_res_impact, points=points)
  results_df <- do.call(rbind, results)
  final <- rbind(real, results_df)
  rownames(final) <- NULL
  final$res <- c(0, res_vector)
  final$sim_id = sim_id
  final$rho = rho
  final$type = type
  return(final)
}

experiments <- expand.grid(sim_id=1:num_sim,
                           rho=rhos,
                           type= types)
simulation_results <- mclapply(1:nrow(experiments), function(id) simulation(sim_id=experiments$sim_id[id],
                                                                            rho = experiments$rho[id],
                                                                            type = experiments$type[id]),
                               mc.cores = 11, mc.preschedule = FALSE)
results <- do.call(rbind, simulation_results)


# Graphs ----

# Fig 1

map_example_reg <- gen_points(map, size=500, type="regular")$points %>%
  st_as_sf(points) %>%
  st_transform(4326)

f1_1 <- ggplot() +
  geom_sf(data=map) +
  geom_sf(data=map_example_reg, size=0.8) +
  theme_minimal() +
  ggtitle("Regular pattern")

map_example_ran <- gen_points(map, size=500, type="random")$points %>%
  st_as_sf(points) %>%
  st_transform(4326)

f1_2 <- ggplot() +
  geom_sf(data=map) +
  geom_sf(data=map_example_ran, size=0.8) +
  theme_minimal() +
  ggtitle("Random pattern")

f1 <- f1_1 + f1_2
ggsave("export/f1.pdf", f1, width = 1200, height=600, units = "px", scale=2)


# Fig 2
f2_1 <- ggplot() +
  geom_sf(data=base_hmaps[[1]]) +
  theme_minimal() +
  ggtitle("H3 Res 3")

f2_2 <- ggplot() +
  geom_sf(data=base_hmaps[[2]]) +
  theme_minimal() +
  ggtitle("H3 Res 4")

f2_3 <- ggplot() +
  geom_sf(data=base_hmaps[[3]]) +
  theme_minimal() +
  ggtitle("H3 Res 5")

f2_4 <- ggplot() +
  geom_sf(data=base_hmaps[[4]]) +
  theme_minimal() +
  ggtitle("H3 Res 6")


f2 <- (f2_1 + f2_2 + f2_3 + f2_4) +
  plot_layout(ncol = 4)
f2
ggsave("export/f2.pdf", f2, width = 1200, height=400, units = "px", scale=2)


# Fig 3
f3 <- results %>%
  filter(res == 0) %>%
  ggplot() +
  geom_density(aes(moran, color=factor(rho))) +
  facet_wrap(~type, nrow=1) +
  theme_bw() +
  xlab("Moran's I") +
  labs(color=expression(rho))
ggsave("export/f3.pdf", f3, width = 1200, height=400, units = "px", scale=2)

# Fig 4
f4 <- results %>%
  filter(res != 0) %>%
  ggplot() +
  geom_density(aes(moran, color=factor(rho))) +
  facet_wrap(type~res, nrow=2) +
  theme_minimal() +
  theme_bw() +
  xlab("Moran's I") +
  labs(color=expression(rho))
ggsave("export/f4.pdf", f4, width = 1200, height=600, units = "px", scale=2)



results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(test = p < 0.05 & p_target < 0.05 |p > 0.05 & p_target > 0.05) %>%
  group_by(res, rho, type) %>%
  summarise(test = mean(test)) %>%
  ggplot() +
  geom_col(aes(rho, test, fill=factor(res)), position='dodge') +
  facet_wrap(~type, nrow=1)

# Fig 5
f5 <- results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(test = p < 0.05 & p_target < 0.05 | p > 0.05 & p_target > 0.05) %>%
  group_by(res, rho, type) %>%
  summarise(test = mean(test)) %>%
  ggplot() +
  geom_col(aes(rho, test, fill=factor(res)), position='dodge') +
  facet_wrap(~type, nrow=1) +
  scale_y_continuous(labels = percent) +  # Format y-axis as percentage
  scale_x_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +  # Set x-axis breaks
  ylab("Correct Tests") +  # Set y-axis label
  xlab(expression(rho)) +  # Set x-axis label
  labs(fill="Resolution") + # Set legend title
  theme_bw()

ggsave("export/f5.pdf", f5, width = 1200, height=400, units = "px", scale=2)



# Other graphs ----

results_target <- results %>%
  filter(res == 0) %>%
  select(sim_id, rho, type, moran_target = moran, p_target = p)

results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(bias = abs(moran - moran_target)) %>%
  group_by(rho) %>%
  summarise(bias = mean(bias))


results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(bias = (moran - moran_target)^2) %>%
  group_by(res, rho, type) %>%
  summarise(bias = mean(bias)) %>%
  ggplot() +
  geom_line(aes(res, bias, color=factor(rho))) +
  geom_point(aes(res, bias, color=factor(rho))) +
  facet_wrap(~type, nrow=2)

results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(bias = abs((moran - moran_target)/moran_target)) %>%
  group_by(res, rho, type) %>%
  summarise(MSE = mean(bias)) %>%
  ggplot() +
  geom_line(aes(rho, MSE, color=factor(res))) +
  geom_point(aes(rho, MSE, color=factor(res))) +
  facet_wrap(~type, nrow=2)


results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(test = p < 0.05 & p_target < 0.05 |p > 0.05 & p_target > 0.05) %>%
  group_by(res, rho, type) %>%
  summarise(test = mean(test))

results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(test = p < 0.05 & p_target < 0.05 |p > 0.05 & p_target > 0.05) %>%
  group_by(res, rho, type) %>%
  summarise(test = mean(test)) %>%
  ggplot() +
  geom_line(aes(rho, test, color=factor(res))) +
  geom_point(aes(rho, test, color=factor(res))) +
  facet_wrap(~type, nrow=2)

results %>%
  filter(res != 0) %>%
  left_join(results_target, by=c("sim_id", "rho", "type")) %>%
  mutate(test = p < 0.05 & p_target < 0.05 |p > 0.05 & p_target > 0.05) %>%
  group_by(res, rho, type) %>%
  summarise(test = mean(test)) %>%
  ggplot() +
  geom_col(aes(rho, test, fill=factor(res)), position='dodge') +
  facet_wrap(~type, nrow=2)
