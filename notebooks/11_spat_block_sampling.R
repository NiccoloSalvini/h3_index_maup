# "2023-12-28 17:01:07 CET"
# Niccolò Salvini
source(here("notebooks", "utils.R"))


# spatial block sampling ----
# spatial resampling and modelling
# TODO lascialo a dopo
library(spatialsample)
h3_hexagons_sf_list_pured = h3_hexagons_sf_list %>%  map(1)
set.seed(123)

blocks <- spatial_block_cv(h3_hexagons_sf_list_pured[[3]], v = 5)
autoplot(blocks) +
  scale_color_discrete(name = "Fold", labels = c("Piega1", "Piega2", "Piega3", "Piega4", "Piega5")) +
  guides(fill = FALSE)

purrr::walk(blocks$splits, function(x) print(autoplot(x) +
                                               scale_fill_discrete(name = "Fold", labels = c("Test", "Training"))+
                                               guides(colour = FALSE)
))
