# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)

# 2. Load data ----

data_eez <- st_read("data/01_background-shp/03_eez/data_eez.shp") %>% 
  rename(territory = TERRITORY1) %>% 
  select(territory)

load("data/09_misc/data-benthic.RData")

# 3. DatasetID per territory ----

data_benthic %>% 
  select(territory, datasetID) %>% 
  distinct() %>% 
  arrange(territory, datasetID) %>% 
  group_by(territory) %>% 
  summarise(datasetID = paste0(datasetID, collapse = ", ")) %>% 
  left_join(data_eez %>% st_drop_geometry(), .) %>% 
  arrange(territory) %>% 
  openxlsx::write.xlsx(., file = "figs/04_supp/tbl-1.xlsx")

# 4. List of contributors per datasetID ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, rightsHolder, last_name, first_name, email) %>% 
  openxlsx::write.xlsx(., file = "figs/04_supp/tbl-2.xlsx")

# 5. List of contributors emails ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(last_name, first_name, email) %>% 
  distinct() %>% 
  arrange(last_name) %>% 
  openxlsx::write.xlsx(., file = "figs/04_supp/tbl-3.xlsx")

# 6. List of contributors per area ----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, last_name, first_name) %>% 
  full_join(data_benthic %>% 
              select(datasetID, territory) %>% 
              distinct(),
            .) %>% 
  drop_na(last_name) %>% 
  arrange(territory, last_name) %>% 
  mutate(last_name = str_to_title(last_name),
         name = paste0(first_name, " ", last_name)) %>% 
  select(-datasetID, -last_name, -first_name) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  mutate(name = paste0(name, collapse = ", ")) %>% 
  distinct() %>% 
  openxlsx::write.xlsx(., file = "figs/04_supp/tbl-4.xlsx")

# 7. Acknowledgments and citations to include -----

read_xlsx("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/05_data-sources.xlsx") %>% 
  filter(datasetID %in% unique(data_benthic$datasetID)) %>% 
  select(datasetID, citation, acknowledgments) %>% 
  distinct() %>% 
  filter(!(is.na(citation) & is.na(acknowledgments)))
