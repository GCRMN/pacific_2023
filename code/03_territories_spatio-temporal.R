# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(sf)
sf_use_s2()

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/map_survey_years.R")

# 3. Load data ----

load("data/04_data-benthic.RData")
load("data/01_background-shp/03_eez/data_eez.RData")

# 4. Extract descriptors ----

# 4.1 Number of monitoring sites --

data_sites <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count(name = "n_sites")

# 4.2 Number of surveys --

data_surveys <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year, month, day) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count(name = "n_surveys")

# 4.3 Number of datasets --

data_datasets <- data_benthic %>% 
  select(territory, datasetID) %>% 
  group_by(territory) %>% 
  distinct() %>% 
  count(name = "n_datasets")

# 4.4 Combine datasets --

data_descriptors <- left_join(data_sites, data_surveys) %>% 
  left_join(., data_datasets)

rm(data_sites, data_surveys, data_datasets)

# 5. Number of surveys per year ----

# 5.1 Select data --

data_benthic <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  distinct()

# 5.2 Make the plots --

map(unique(data_benthic$territory), ~map_survey_years(territory_i = .))

# 6. Map of spatio-temporal distribution of monitoring sites ----

# 6.1 Select data --

data_benthic <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, year) %>% 
  distinct() %>% 
  group_by(territory, decimalLatitude, decimalLongitude) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 6.2 Make the maps --

map(unique(data_benthic$territory), ~map_eez_sites(territory_i = .))
