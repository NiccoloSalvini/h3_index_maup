#  "2023-12-28 17:01:31 CET"
#  Niccolò Salvini
source(here("notebooks", "utils.R"))

# 3D vis of hexagons at different resolutions
p <- plot_ly()

# Iterate over each resolution and add hexagons and points to the plot
# this works with uncommented `create_hexagon_sf_list` fun
for (i in seq_along(h3_hexagons_sf_list)) {
  hexagons_sf <- h3_hexagons_sf_list[[i]]$hexagons
  points_sf <- h3_hexagons_sf_list[[i]]$points

  # Add hexagons
  p <- add_sf(p, data = hexagons_sf,
              z = ~as.numeric(resolution),
              #i = ~mesh$it[,1]-1, j = ~mesh$it[,2]-1, k = ~mesh$it[,3]-1,
              color = ~resolution)

  # Add points
  p <- add_trace(p, data = points_sf, type = "scatter3d", mode = "markers",
                 x = ~st_coordinates(geometry)[,1], y = ~st_coordinates(geometry)[,2], z = ~as.numeric(resolution),
                 marker = list(size = 2, color = 'black'))
}

# Finalize the plot
p <- p %>% layout(title = '3D Plot of Observations Across H3 Resolutions',
                  scene = list(xaxis = list(title = 'Longitude'),
                               yaxis = list(title = 'Latitude'),
                               zaxis = list(title = 'Resolution')))
p
