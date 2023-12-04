# Niccolò Salvini "2023-11-30 20:15:16 CET"
# the idea is to scrape data of last three months prepare and aggregate is

# 1.0 Scrape table and put last three months together ----
library(rvest)
library(dplyr)
library(here)
library(janitor)
library(stringr)
library(httr)
library(utils)
library(readr)
library(purrr)
library(tidyr)
library(anytime)
library(sf)
library(lubridate)
library(pins)

# ask for aws credentials
# board <- board_s3("prezzi-benzina-trimestre", region = "eu-south-1")

url_prezzi_carb  = "https://www.mimit.gov.it/it/open-data/elenco-dataset/carburanti-archivio-prezzi"
url_prezzi_carb %>%
  read_html() %>%
  html_table() %>%
  .[[1]] %>%
  clean_names()

hrefs_tab_anagrafica = url_prezzi_carb %>%
  read_html() %>%
  html_elements(css = "td:nth-child(2) a") %>%
  html_attr("href")

hrefs_tab_prezzo = url_prezzi_carb %>%
  read_html() %>%
  html_elements(css = "td~ td+ td a") %>%
  html_attr("href")

# Set timeout higher
options(timeout=240)

download_and_unzip <- function(href, folder) {
  tryCatch({
    # Create the folder if it doesn't exist
    if (!dir.exists(folder)) {
      dir.create(folder)
    }

    # Construct file paths
    temp_file <- tempfile(fileext = ".zip")
    dest_file = here(basename(href))
    # dest_file <- file.path(folder, basename(href))

    # Download the file
    download.file(href, temp_file, mode = "wb")

    # Unzip the file
    untar(temp_file, exdir = folder, verbose = T)

    # Clean up the temporary file
    unlink(temp_file)
  }, error = function(e) {
    message("Error in downloading or unzipping: ", href)
    message("Error message: ", e$message)
  })
}

# Process all the URLs (this takes a while)
# sapply(hrefs_tab_anagrafica, download_and_unzip, folder = "data/anagrafica")
# sapply(hrefs_tab_prezzo, download_and_unzip, folder = "data/prezzi")

# download first quarter
# as of today: "2023-11-30 20:45:51 CET" (Jul - Sep)
# TODO rename when untar
download_and_unzip(href = hrefs_tab_anagrafica[1], folder = "data/anagrafica")
download_and_unzip(href = hrefs_tab_prezzo[1], folder = "data/prezzo")



aggregate_within_folder <- function(path, start, end) {

  filename = basename(path)

  if(str_detect(filename, "anagrafica")){
    desume_date_and_day = str_sub(filename, start = 28, end = 35)

    file = read_delim(path, name_repair = make_clean_names, delim = ";", skip = 1) %>%
      mutate(
        data_estrazione = anydate(desume_date_and_day),
        latitudine = as.numeric(latitudine),
        longitudine = as.numeric(longitudine)
      )
  } else {
    desume_date_and_day  = str_sub(filename, start = 14, end = 20)

    file = read_delim(path, name_repair = make_clean_names, delim = ";", skip = 1) %>%
      separate(dt_comu, into =c("data", "ora"), sep = " ") %>%
      mutate(
        data = dmy(data),
        ora = hms(ora)
      )
  }
  return(file)
}


## TODO use pin boards to register files.
last_quarter_anagrafiche = map_dfr(list.files("data/anagrafica/ftproot/osservaprezzi/copied", full.names = T), aggregate_within_folder)
last_quarter_prezzi = map_dfr(list.files("data/prezzo/ftproot/osservaprezzi/copied", full.names = T), aggregate_within_folder)

# saveRDS(last_quarter_anagrafiche, here("data","last_quarter_anagrafiche.rds"))
# saveRDS(last_quarter_prezzi, here("data", "last_quarter_prezzi.rds"))

last_quarter_anagrafiche= read_rds(here("data", "last_quarter_anagrafiche.rds"))
board <- board_s3("prezzi-benzina-trimestre", region = "eu-south-1")

# ~8ml rows (too many) select only benzina
last_quarter_prezzi= read_rds(here("data", "last_quarter_prezzi.rds"))


# ~3ml rows (now manageable)
last_quarter_prezzi_bezina = last_quarter_prezzi %>%
  filter(desc_carburante == "Benzina")

municipalities_sf =read_sf(here("data", "municipalities.geojson")) %>%
  st_transform(crs = 4326)

# only with benzina this is ~286mln rows (!!)
last_quarter =  last_quarter_prezzi_bezina %>%
  left_join(last_quarter_anagrafiche, by = "id_impianto")

xsaveRDS(last_quarter, here("data","last_quarter.rds"))

## le tolgo per ora, poi vanno pulite
last_quarter_municip_sf = last_quarter %>%
  distinct(across(everything())) %>%
  filter(provincia %in% regione_toscana) %>%
  separate(dt_comu, into =c("data", "ora"), sep = " ") %>%
  mutate(
    data_dmy = dmy(data),
    ora_hms = hms(ora)
  ) %>%
  filter(!is.na(longitudine)) %>%
  st_as_sf(coords = c("longitudine", "latitudine"), crs = 4326)


# intersect, some of them have wrong coordinates and end up outside tuscany
# TODO trova quelli che finiscono fuori dalla toscana, quelli con coordinate sbagliate.

gas_station_intersect_sf =  st_intersects(sf_municipalities, whole_tuscany_data_sf)
gas_station_intersect_sf <- whole_tuscany_data_sf[unlist(gas_station_intersect_sf),]


board <- board_s3("prezzi-benzina-trimestre", region = "eu-south-1")

board %>% pin_write(last_quarter)


read_delim("data/prezzo/ftproot/osservaprezzi/copied/prezzo_alle_8-20230701.csv", delim = ";", skip = 1, name_repair = make_clean_names)
