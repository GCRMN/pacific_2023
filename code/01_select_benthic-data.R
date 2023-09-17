# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)

# 2. Load gcrmndb_benthos data ----

load("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

# 3. Filter required data ----

data_benthic <- synthetic_data %>% 
  filter(higherGeography == "Pacific") %>% 
  filter(datasetID != "0009")

# 4. Save the data ----

save(data_benthic, file = "data/04_data-benthic.RData")

# 5. Export site coordinates (for predictors extraction) ----

data_benthic %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_write(., dsn =  "data/15_benthic-site-coords/benthic-site-coords.shp", delete_dsn = TRUE)
