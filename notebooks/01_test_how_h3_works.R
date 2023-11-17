## remotes::install_github("crazycapivara/h3-r")
library(h3)

# Rendering
library(leaflet)

# Binning
# takes lat long and map back to unique hexagon
h3_index <- geo_to_h3(road_safety_greater_manchester)

## table how many accidents per hex
tbl <- table(h3_index) %>%
  tibble::as_tibble()

tbl

## convert to `sf` and rename columns
hexagons <- h3_to_geo_boundary_sf(tbl$h3_index) %>%
  dplyr::mutate(index = tbl$h3_index, accidents = tbl$n)

pal <- colorBin("YlOrRd", domain = hexagons$accidents)

map <- leaflet(data = hexagons, width = "100%") %>%
  addProviderTiles("Stamen.Toner") %>%
  addPolygons(
    weight = 2,
    color = "white",
    fillColor = ~ pal(accidents),
    fillOpacity = 0.8,
    label = ~ sprintf("%i accidents (%s)", accidents, index)
  )

map

## sono organizzati come un grafo ( solo 1 parente ma più figli)
## come funzionano i parenti via h3_to_parent
h3_index_res_10<- geo_to_h3(road_safety_greater_manchester, res = 10)

# nessun parente con sè stesso
h3_to_parent(h3_index_res_4, res = 10)

# parenteì: 8419519ffffffff
h3_to_parent(h3_index_res_4, res = 4)

## figli
h3_to_children(h3_index_res_4, res = 11)




