library(here)
library(sf)
library(geojsonsf)


path = here('data', 'defibrillatori.geojson')

defibrillatori_sf <- geojson_sf(path)


## fare la stessa analisi dei degli accidents ma su dati
## più sanitari, con risvolto sanitario, molto figo, c'è coerenza
## appetibile anche per tesi di dottorato


## il problema è che non abbiamo variabili per modello significative, ma a quel punto chi se ne frega, basta
## solo variabilelluogo evento!
##
