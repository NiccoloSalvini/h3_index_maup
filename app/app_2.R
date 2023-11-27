library(sf)
library(ggplot2)
library(leaflet)
library(shiny)
library(h3)
library(here)
source(here("notebooks","utils.R"))


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
                          st_make_grid(reproj_sf_road_safety_greater_manchester, cellsize = grid_size/10000, square = T) %>% #, cellsize = grid_size
                          st_as_sf() %>%
                          mutate(id = 1:n())

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
                          geom_sf(data = reproj_sf_road_safety_greater_manchester, alpha = 0.3) +
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
