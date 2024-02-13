library(sf)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(h3)
library(here)

# source(here("notebooks","utils.R")
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



projcrs = "+proj=longlat +datum=WGS84 +no_defs"

gmanchester_st = st_read("https://raw.githubusercontent.com/OpenDataManchester/gm-hex-map/master/gmauthorities.geojson") %>%
  st_transform(crs = projcrs) %>%
  st_as_sf()

sf_road_safety_greater_manchester <-
  st_as_sf(x = as.data.frame(road_safety_greater_manchester),
           coords = c("lng", "lat"),
           crs = projcrs)


ui <- fluidPage(
  titlePanel("Grid Type Selector"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("resolution", "H3 Resolution:",
                  min = 5, max = 10, value = 7, step = 1),
      sliderInput("squareResolution", "Square Grid Resolution:",
                  min = 100, max = 1000, value = 500, step = 100, post = " meters"),
      radioButtons("gridType", "Grid Type:",
                   choices = c("H3", "Regular", "Voronoi", "Delaunay"),
                   selected = "H3")
    ),

    mainPanel(
      plotOutput("map")
    )
  )
)

server <- function(input, output) {
  output$map <- renderPlot({
    grid_data <- switch(input$gridType,
                        "H3" = {
#
#                           hex_grid <-
#                             st_make_grid(sf_road_safety_greater_manchester, square = F) %>%
#                             st_as_sf() %>%
#                             mutate(id = 1:n())
#
#                           index <- which(lengths(st_intersects(sf_road_safety_greater_manchester)) > 0)
#
#                           hex_grid <- hex_grid[index,]
#
#                           hexagons = hex_grid |>
#                             st_as_sf() |> # cast to sf
#                             mutate(grid_id = row_number()) |> # create unique ID
#                             st_join(sf_road_safety_greater_manchester) |> # join the species dataset
#                             group_by(grid_id) |> # group by the grid id
#                             count() |> # count the number of rows
#                             # fill the plot by the number of points in the grid
#                             ggplot() +
#                             # make kinda pretty
#                             geom_sf(data = gmanchester_st) +
#                             geom_sf(aes(fill = n, alpha= 0.6),lwd = 0.1, color = "white") +
#                             theme_map()
#
#
                          h3_index_res <- geo_to_h3(road_safety_greater_manchester, res = input$resolution)
                          tbl <- table(h3_index_res) %>%
                            tibble::as_tibble()

                          hexagons <- h3_to_geo_boundary_sf(tbl$h3_index_res) %>%
                            dplyr::mutate(index = tbl$h3_index_res, accidents = tbl$n)

                          hex_map =  ggplot(data = hexagons) +
                            geom_sf(aes(fill = accidents, alpha = 0.6)) +
                            geom_sf(data = gmanchester_st, fill = "transparent") +
                            geom_sf(data = sf_road_safety_greater_manchester, alpha = 0.3) +
                            theme_map()

                          hex_map

                        },
                      "Regular" = {
                        # Generate regular grid based on user-defined resolution
                        grid_size <- c(input$squareResolution, input$squareResolution)

                        grid <-
                          st_make_grid(sf_road_safety_greater_manchester, cellsize = grid_size/10000, square = T) %>% #, cellsize = grid_size
                          # reproj_sf_road_safety_greater_manchester
                          st_as_sf() %>%
                          dplyr::mutate(id = 1:dplyr::n())

                        index <- which(lengths(st_intersects(grid, sf_road_safety_greater_manchester)) > 0)

                        fishnet <- grid[index,]

                        regular = fishnet |>
                          st_as_sf() |> # cast to sf
                          mutate(grid_id = row_number()) |> # create unique ID
                          st_join(sf_road_safety_greater_manchester) |> # join the dataset
                          group_by(grid_id) |> # group by the grid id
                          count() |> # count the number of rows
                          ggplot() +
                          geom_sf(data = gmanchester_st) +
                          geom_sf(aes(fill = n, alpha= 0.6),lwd = 0.1, color = "white") +
                          geom_sf(data = sf_road_safety_greater_manchester, alpha = 0.3) +
                          theme_map()

                        regular
                      },

                    "Voronoi" = {
                          # Generate Voronoi polygons

                          box <- st_bbox( sf_road_safety_greater_manchester ) %>% st_as_sfc()

                          v_grid = st_voronoi(st_union(sf_road_safety_greater_manchester))
                          vparts = st_collection_extract(v_grid)
                          vcrop = st_crop(vparts, box) %>%
                            st_as_sf()

                          voronoi = ggplot(data = box) +
                            # make kinda pretty
                            geom_sf(data = gmanchester_st) +
                            geom_sf(data = sf_road_safety_greater_manchester, alpha=0.5)+
                            geom_sf(data = vcrop, fill = "transparent" )+
                            theme_map()

                          voronoi


                        },
                        "Delaunay" = {

                          box <- st_bbox( sf_road_safety_greater_manchester ) %>% st_as_sfc()

                          d_grid = st_triangulate(st_union(sf_road_safety_greater_manchester))
                          dparts = st_collection_extract(d_grid)
                          dcrop = st_crop(dparts, box) %>%
                            st_as_sf()

                          delaunay = ggplot(data = box) +
                            # make kinda pretty
                            geom_sf(data = gmanchester_st) +
                            geom_sf(data = sf_road_safety_greater_manchester, alpha=0.5)+
                            geom_sf(data = dcrop, fill = "transparent" )+
                            theme_map()

                          delaunay
                        }
    )
    grid_data
  })
}

shinyApp(ui = ui, server = server)
