## "2023-12-13 19:44:57 CET"
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

gs_mean_prices_intersect_sf =  read_rds(here("data",  "gs_mean_prices_intersect_sf.rds"))

# here h3 index sf obj with prices and counts!
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


# h3_hexagons_sf_list <- create_hexagon_sf_list(gs_mean_prices_intersect_sf, resolutions)


library(ggplot2)
theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(# family = default_font_family
        color = "black"),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = "white",
                                     color = NA),
      panel.background = element_rect(fill = "white",
                                      color = NA),
      legend.background = element_rect(fill = "white",
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = "black"),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = "black"),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "black",
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}


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
