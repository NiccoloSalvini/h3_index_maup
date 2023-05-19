
library(h3)

simulated_data =data.frame(lat = sample(-90:90, 1000, replace = TRUE), lng = sample(-180:180, 1000, replace = TRUE))



hexagons <- geo_to_h3(simulated_data, res = 1)






## take inspiration for MAUP here: https://andrewmaclachlan.github.io/CASA0005repo_20192020/advanced-r-maup-and-more-regression.html
