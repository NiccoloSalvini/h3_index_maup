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


h3_hexagons_sf_list <- create_hexagon_sf_list(gs_mean_prices_intersect_sf, resolutions)






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
