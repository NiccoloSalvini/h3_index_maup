library(h3)
library(dplyr)
library(spdep)
library(purrr)

# Define the different resolutions
resolutions <- 0:15

# Create a global hexagonal grid
hexagons <- lapply(resolutions, function(r) {
  data = data.frame(lng = runif(1000, -180, 180),
                    lat = runif(1000, -90, 90))
  tibble(
    h3_id = geo_to_h3(
      latlng = data,
      res = r
    )
  ) %>%
    count(h3_id) %>%
    mutate(resolution = r)
}) %>%
  bind_rows()



h3_to_geo_boundary_sf(hexagons$h3_id) %>%
  mutate(resolution = hexagons$resolution, n = hexagons$n) %>%
  filter(resolution ==10) %>%
  leaflet(width = "100%") %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(
    weight = 2,
    color = "white",
    fillColor = ~ pal(n),
    fillOpacity = 0.8
  )





# Calculate the average value for each hexagon
hexagons$value <- runif(nrow(hexagons))

# Example of aggregating data to coarser resolutions
aggregate_hexagons <- hexagons %>%
  group_by(h3_id) %>%
  summarise(value = sum(value), resolution = min(resolution)) %>%
  group_by(h3_id, resolution) %>%
  summarise(value = mean(value))


## below currently not working!

# Example of fitting a spatial autoregressive model
model <- spdep::lagsarlm(
  formula = value ~ 1,
  data = hexagons,
  listw = hex_to_hex_2d(list(hexagons$h3_id)),
  method = "BM",
  tol.solve = 1e-6,
  zero.policy = TRUE
)

summary(model)

# Example of incorporating uncertainty analysis
# we may think of having different level of spatial autocorelation
simulations <- replicate(10, hexagons %>% mutate(value = rnorm(n(), value, sd = 0.1 * value)))


aggregate_and_summarise <- function(.data, col_grouping, col_value) {
  .data %>%
    group_by(col_grouping) %>%
    summarise(mean_value = mean(col_value), sd_value = sd(col_value)) %>%
    return()
}


simulation_summary <- simulations %>%
  map_dfr(~aggregate_and_summarise(.x, col_grouping = h3_id, col_value = value))

group_by(h3_id) %>%
  summarise(
    mean_value = mean(mean_value),
    sd_value = mean(sd_value)
  ) %>%
  mutate(coeff_var = sd_value / mean_value)


simulation_summary <- simulations %>%
  map_dfr(~ .x %>%
            group_by(h3_id) %>%
            summarise(mean_value = mean(value), sd_value = sd(value)))
  group_by(h3_id) %>%
  summarise(
    mean_value = mean(mean_value),
    sd_value = mean(sd_value)
  ) %>%
  mutate(coeff_var = sd_value / mean_value)








# First, we need to create a spatial weights matrix to capture the spatial dependencies between the hexagonal cells:

w <- poly2nb(as_Spatial(H3ToGeoBoundary(hexagons$h3_id, "planar")), row.names = hexagons$h3_id)
w <- nb2listw(w, style = "B", zero.policy = TRUE)

#  Next, we can fit a spatial autoregressive model to the original dataset:

model_original <- spautolm(hexagons$value ~ 1, data = hexagons, listw = w)

# And to the aggregated dataset:

model_aggregated <- spautolm(aggregate_hexagons$value ~ 1, data = aggregate_hexagons, listw = w)

#  To incorporate uncertainty analysis into the spatial data, we can generate multiple realizations of the spatial dataset using the Monte Carlo method. For example, to generate 1000 realizations of the original dataset:

realizations <- lapply(1:1000, function(i) {
  data <- data.frame(lng = runif(1000, -180, 180), lat = runif(1000, -90, 90))
  data$h3_id <- geo_to_h3(latlng = data, res = 15)
  data$value <- runif(nrow(data))
  aggregate_hexagons <- data %>%
    group_by(h3_id) %>%
    summarise(value = sum(value), resolution = 15) %>%
    group_by(h3_id, resolution) %>%
    summarise(value = mean(value))
  aggregate_hexagons
})

# To compute the standard deviation of the data within each hexagonal cell and include this information in the H3 spatial indexing:

hexagons_stddev <- hexagons %>%
  group_by(h3_id) %>%
  summarise(value = sd(value), resolution = min(resolution))

#   We can then bind the standard deviation values to the original hexagons dataset:

hexagons <- left_join(hexagons, hexagons_stddev, by = "h3_id")

#   And include them in the aggregated dataset as well:

aggregate_hexagons_stddev <- aggregate_hexagons %>%
  group_by(h3_id) %>%
  summarise(value = sd(value), resolution = min(resolution))

aggregate_hexagons <- left_join(aggregate_hexagons, aggregate_hexagons_stddev, by = "h3_id")

# Finally, we can plot the results to visualize the MAUP effects:

library(ggplot2)

ggplot(hexagons, aes(x = resolution, y = value)) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~h3_id, ncol = 5)

ggplot(aggregate_hexagons, aes(x = resolution, y = value)) +
  geom_jitter(alpha = 0.5) +
  facet_wrap(~h3_id, ncol = 5)

    # The first plot shows the original dataset at different resolutions, while the second plot shows the aggregated dataset at different resolutions. As expected, the aggregated dataset shows a convergence of mean values at coarser resolutions, indicating the impact of MAUP.


## take inspiration for MAUP here: https://andrewmaclachlan.github.io/CASA0005repo_20192020/advanced-r-maup-and-more-regression.html
##
##
##
## very nice slides of MAUP and tassellation https://mikejohnson51.github.io/spds/lecture-15#1
##



 ## map theme
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
