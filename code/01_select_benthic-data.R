# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Load gcrmndb_benthos data ----

load("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

# 3. Filter required data ----

data_benthic <- synthetic_data %>% 
  # Filter GCRMN region
  filter(higherGeography == "Pacific" & territory != "Australia") %>% 
  # Remove useless datasets
  filter(!(datasetID %in% c("0009", "0020", "0042")))

# 4. Save the data ----

save(data_benthic, file = "data/09_misc/data-benthic.RData")

# 5. Export site coordinates (for predictors extraction) ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  mutate(site_id = row_number(.)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_write(., dsn = "data/04_site-coords/site-coords_obs.shp", delete_dsn = TRUE)
