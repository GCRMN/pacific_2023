# 1. Load packages ----

library(tidyverse) # Core tidyverse packages

# 2. Load gcrmndb_benthos data ----

load("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

# 3. Filter required data ----

data_benthic <- synthetic_data %>% 
  filter(higherGeography == "Pacific") %>% 
  filter(datasetID != "0009")

# 4. Save the data ----

save(data_benthic, file = "data/04_data-benthic.RData")
