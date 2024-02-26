library(sf)
library(spdep)
library(h3)
library(ggplot2)
library(dplyr)
library(parallel)
library(igraph)
library(patchwork)
library(scales)
library(Matrix)


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

map <- read_sf("data/ProvCM01012023_g/ProvCM01012023_g_WGS84.shp")

gen_hmap <- function(map, res=1){
  map %>%
    st_union() %>%
    st_transform(4326) %>%
    polyfill(res = res) %>%
    h3_to_geo_boundary_sf()
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

simulate_res_impact <- function(res, points){
  hmap <- base_hmaps[[res]]

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

  hmap <- hmap %>%
    left_join(values, by = join_by(h3_index)) %>%
    filter(!is.na(y))

  nb <-poly2nb(hmap, queen=T)
  nb <- mstconnect(hmap, nb)
  W_hmap <- nb2listw(nb)
  moran <- moran.mc(hmap$y, W_hmap, 999)
  return(data.frame(mean = mean(hmap$y), sd=sd(hmap$y), moran=moran$statistic, p=moran$p.value))
}





# Simulation ----
res_vector <- 3:5
rhos <- c(0, 0.2, 0.4, 0.6, 0.8)
num_sim <- 100
base_hmaps <- lapply(res_vector, gen_hmap, map=map)
base_map <- base_hmaps[[3]]


simulation <- function(sim_id, rho){
  system(sprintf('echo "\n%s\n"', paste(sim_id, "-", rho)))
  points <- gen_points(base_map, rho)
  results <- lapply(1:length(res_vector), simulate_res_impact, points=points)
  results_df <- do.call(rbind, results)
  rownames(results_df) <- NULL
  results_df$res <- res_vector
  results_df$sim_id = sim_id
  results_df$rho = rho
  return(results_df)
}

# simulation(1, 0.8)
#
# experiments <- expand.grid(sim_id=1:num_sim,
#                            rho=rhos)
# simulation_results <- lapply(1:nrow(experiments), function(id) simulation(sim_id=experiments$sim_id[id],
#                                                                             rho = experiments$rho[id]))
# results <- do.call(rbind, simulation_results)
# results

numCores <- detectCores()
cl <- makeCluster(numCores-1)
experiments <- expand.grid(sim_id=1:num_sim, rho=rhos)
clusterExport(cl, varlist = c("simulation", "experiments", "gen_points",
                              'base_map',"mstconnect",
                              "Diagonal", 'simulate_res_impact',
                              'res_vector','base_hmaps'))

clusterEvalQ(cl, {
  library(dplyr)
  library(spdep)
  library(sf)
})

simulation_results <- parLapply(cl, 1:nrow(experiments), function(id) {
  simulation(sim_id=experiments$sim_id[id], rho=experiments$rho[id])
})

stopCluster(cl)

results <- do.call(rbind, simulation_results)
results



# Graphs ----

# Fig 1
example_map <- gen_points(base_map, rho=0)
m1 <- base_map %>%
  mutate(y=example_map$y) %>%
  ggplot() +
  geom_sf(aes(fill=y)) +
  theme_minimal() +
  ggtitle("No autocorrelation")

example_map <- gen_points(base_map, rho=0.8)
m2 <- base_map %>%
  mutate(y=example_map$y) %>%
  ggplot() +
  geom_sf(aes(fill=y)) +
  theme_minimal() +
  ggtitle("Positive autocorrelation")

f1 <- m1  + m2
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



f2 <- (f2_1 + f2_2 + f2_3) +
  plot_layout(ncol = 3)
f2
ggsave("export/f2.pdf", f2, width = 1200, height=400, units = "px", scale=2)


# Fig 3
f3 <- results %>%
  filter(res == 5) %>%
  ggplot() +
  geom_density(aes(moran, color=factor(rho))) +
  #facet_wrap(~type, nrow=1) +
  theme_bw() +
  xlab("Moran's I") +
  labs(color=expression(rho))
ggsave("export/f3.pdf", f3, width = 1200, height=400, units = "px", scale=2)

# Fig 4
f4 <- results %>%
  filter(res != 0) %>%
  ggplot() +
  geom_density(aes(moran, color=factor(rho))) +
  facet_wrap(res~., ncol=3) +
  theme_minimal() +
  theme_bw() +
  xlab("Moran's I") +
  labs(color=expression(rho))
ggsave("export/f4.pdf", f4, width = 1200, height=600, units = "px", scale=2)


f5 <- results %>%
  group_by(res, rho) %>%
  summarise(mean = mean(mean)) %>%
  mutate(rho=as.factor(rho)) %>%
  ggplot() +
  geom_line(aes(res, mean, color=rho)) +
  theme_minimal() +
  theme_bw() +
  xlab("H3 Res") +
  labs(color=expression(rho)) +
  ylim(c(-1, 1)) +
  scale_x_continuous(breaks=res_vector)
ggsave("export/f5.pdf", f5, width = 1200, height=600, units = "px", scale=2)


f6 <- results %>%
  group_by(res, rho) %>%
  summarise(sd = mean(sd)) %>%
  mutate(rho=as.factor(rho)) %>%
  ggplot() +
  geom_line(aes(res, sd, color=rho)) +
  theme_minimal() +
  theme_bw() +
  xlab("H3 Res") +
  labs(color=expression(rho)) +
  scale_x_continuous(breaks=res_vector)
ggsave("export/f6.pdf", f6, width = 1200, height=600, units = "px", scale=2)

f7 <- results %>%
  group_by(res, rho) %>%
  summarise(moran = mean(moran)) %>%
  mutate(rho=as.factor(rho)) %>%
  ggplot() +
  geom_line(aes(res, moran, color=rho)) +
  theme_minimal() +
  theme_bw() +
  xlab("H3 Res") +
  ylab("Moran's I") +
  labs(color=expression(rho)) +
  scale_x_continuous(breaks=res_vector)
ggsave("export/f7.pdf", f7, width = 1200, height=600, units = "px", scale=2)

f8 <- results %>%
  group_by(res, rho) %>%
  mutate(p_value = p < 0.05) %>%
  summarise(p = mean(p_value)) %>%
  mutate(rho=as.factor(rho)) %>%
  ggplot() +
  geom_line(aes(res, p, color=rho)) +
  geom_point(aes(res, p, color=rho)) +
  theme_minimal() +
  geom_hline(yintercept=0.05) +
  theme_bw() +
  xlab("H3 Res") +
  ylab("Power") +
  labs(color=expression(rho)) +
  scale_x_continuous(breaks=res_vector)
ggsave("export/f8.pdf", f8, width = 1200, height=600, units = "px", scale=2)
