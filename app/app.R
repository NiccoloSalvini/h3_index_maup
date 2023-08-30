## remotes::install_github("crazycapivara/h3-r")
library(h3)
library(leaflet)
library(shiny)
library(sf)


ui <- fluidPage(
  titlePanel("H3 Resolution Tweaker"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("resolution", "H3 Resolution:",
                  min = 5, max = 10, value = 7, step = 1)
    ),

    mainPanel(
      leafletOutput("map")
    )
  )
)

server <- function(input, output) {
  output$map <- renderLeaflet({
    h3_index_res <- geo_to_h3(road_safety_greater_manchester, res = input$resolution)
    tbl <- table(h3_index_res) %>%
      tibble::as_tibble()

    hexagons <- h3_to_geo_boundary_sf(tbl$h3_index_res) %>%
      dplyr::mutate(index = tbl$h3_index_res, accidents = tbl$n)


    pal <- colorBin("YlOrRd", domain = hexagons$accidents)

    map <- leaflet(data = hexagons, width = "100%") %>%
      addProviderTiles("CartoDB.VoyagerLabelsUnder") %>%
      addPolygons(
        weight = 2,
        color = "white",
        fillColor = ~ pal(accidents),
        fillOpacity = 0.8,
        label = ~ sprintf("%i accidents (%s)", accidents, index)
      )

    map
  })
}

shinyApp(ui = ui, server = server)


