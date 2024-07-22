# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)

# 2. Load data ----

load("data/09_misc/data-benthic.RData")

# 3. DatasetID per territory ----

data_benthic %>% 
  select(territory, datasetID) %>% 
  distinct() %>% 
  arrange(territory, datasetID) %>% 
  group_by(territory) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  write.csv2(., file = "figs/03_methods/table-1.csv",
             row.names = FALSE)

# 4. Extract data sources for datasets used in the analyses ----

read_xlsx(path = "../../2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx",
          sheet = 1) %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  write.csv2(., file = "figs/04_supp/data-sources.csv",
             row.names = FALSE)

# 5. Data contributors per territory ----

data_benthic %>% 
  select(datasetID, country, territory) %>% 
  distinct() %>% 
  full_join(., read_xlsx(path = "../../2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx",
                         sheet = 1) %>% 
              filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
              select(datasetID, last_name, first_name, email)) %>% 
  select(-datasetID) %>% 
  distinct() %>% 
  write.csv2(., file = "figs/04_supp/contributors-territory.csv",
             row.names = FALSE)
  
# 6. DatasetID per territory ----

data_eez <- st_read("data/01_background-shp/03_eez/data_eez.shp") %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1) %>% 
  rename(territory = TERRITORY1)

data_benthic %>% 
  select(territory, datasetID) %>% 
  distinct() %>% 
  # Add territories with no data
  left_join(data_eez, .) %>% 
  group_by(territory) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  ungroup() %>% 
  arrange(territory, datasetID) %>% 
  write.csv2(., file = "figs/04_supp/datasetid-territory.csv",
             row.names = FALSE)
