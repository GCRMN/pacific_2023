# 1. Load packages ----

library(tidyverse)
library(sf)

# 2. Load benthic cover data ----

load("data/04_data-benthic.RData")

# 3. Combine predictors ----

## 3.1 Load predictor values ----

site_coords <- st_read("data/15_site-coords/site-coords_all.shp") %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  mutate(year = 2000) %>% 
  tidyr::complete(year = seq(1980, 2023), nesting(site_id, type, territory, decimalLongitude, decimalLatitude))

data_pred_elevation <- read.csv("data/14_predictors/pred_elevation.csv") %>% 
  rename(pred_elevation = mean) %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0))

data_pred_population <- read.csv("data/14_predictors/pred_human-pop.csv") %>% 
  mutate(year = as.numeric(str_split_fixed(system.index, "_", 8)[,6])) %>% 
  select(-system.index) %>% 
  tidyr::complete(year = seq(1980, 2023), nesting(site_id, type), fill = list(sum = NA)) %>% 
  arrange(type, site_id, year) %>% 
  rename(pred_population = sum)

data_pred_reef_extent <- read.csv("data/14_predictors/pred_reef-extent.csv") %>% 
  rename(pred_reefextent = sum)

data_pred_land <- read.csv("data/14_predictors/pred_land.csv") %>% 
  rename(pred_land = sum)

data_pred_chla <- read.csv("data/14_predictors/pred_chla.csv") %>% 
  rename(pred_chla = mean)

data_pred_sst_mean <- read.csv("data/14_predictors/pred_sst_mean.csv") %>% 
  rename(pred_sst_mean = first) %>% 
  mutate(pred_sst_mean = pred_sst_mean/100)

data_pred_sst_sd <- read.csv("data/14_predictors/pred_sst_sd.csv") %>% 
  rename(pred_sst_sd = first) %>% 
  mutate(pred_sst_sd = pred_sst_sd/100)

data_pred_sst_skew <- read.csv("data/14_predictors/pred_sst_skew.csv") %>% 
  rename(pred_sst_skewness = first)

data_pred_sst_kurtosis <- read.csv("data/14_predictors/pred_sst_kurtosis.csv") %>% 
  rename(pred_sst_kurtosis = first)

data_pred_gravity <- read.csv("data/14_predictors/pred_gravity.csv")

data_pred_enso <- read.csv("data/14_predictors/pred_enso.csv") %>% 
  rename(pred_enso = enso)

## 3.2 Join values ----

data_predictors <- site_coords %>% 
  left_join(., data_pred_population) %>% 
  left_join(., data_pred_enso) %>% 
  
  left_join(., data_pred_elevation) %>% 
  left_join(., data_pred_land) %>% 
  left_join(., data_pred_reef_extent) %>% 
  left_join(., data_pred_chla) %>% 
  left_join(., data_pred_sst_mean) %>% 
  left_join(., data_pred_sst_sd) %>% 
  left_join(., data_pred_sst_skew) %>% 
  left_join(., data_pred_sst_kurtosis) %>% 
  left_join(., data_pred_gravity)

## 3.3 Remove useless objects ----

rm(data_pred_elevation, data_pred_population, data_pred_reef_extent,
   data_pred_land, data_pred_chla, data_pred_sst_mean, data_pred_sst_sd,
   data_pred_gravity, site_coords, data_pred_sst_skew, data_pred_sst_kurtosis)

# 4. Split predictors in observed and to predict tibbles ----

## 4.1 Predictors values for sites with observed data ----

data_predictors_obs <- data_predictors %>% 
  filter(type == "obs") %>% 
  select(-type, -site_id, -territory)

save(data_predictors_obs, file = "data/16_model-data/data_predictors_obs.RData")

## 4.2 Predictors values for sites to predict ----

data_predictors_pred <- data_predictors %>% 
  filter(type == "pred") %>% 
  select(-type, -site_id) %>% 
  expand_grid(., year = seq(from = 1980, to = 2023, by = 1)) %>% 
  mutate(datasetID = NA,
         month = NA,
         day = NA,
         verbatimDepth = NA,
         parentEventID = NA,
         eventID = NA)

save(data_predictors_pred, file = "data/16_model-data/data_predictors_pred.RData")

# 5. Check number of NA per predictor ----

## 5.1 Predictors values for sites with observed data ----

data_predictors_obs %>% 
  select(-decimalLatitude, -decimalLongitude) %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_obs),
         percent = (na*100)/n)

## 5.2 Predictors values for sites to predict ----

data_predictors_pred %>% 
  select(-decimalLatitude, -decimalLongitude) %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_pred),
         percent = (na*100)/n)

# 6. Transform benthic data ----

## 6.1 Modify NCRMP data (from semi-quantitative to quantitative) by averaging at the scale of a transect ----

data_benthic_ncrmp <- data_benthic %>% 
  filter(datasetID %in% c("0011", "0012", "0013", "0014")) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category, subcategory) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category, subcategory) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  filter(measurementValue <= 100)

## 6.2 Summarize data and add predictors ----

data_benthic <- data_benthic %>% 
  filter(!(datasetID %in% c("0011", "0012", "0013", "0014"))) %>% 
  bind_rows(., data_benthic_ncrmp) %>% 
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  # Sum of benthic cover per sampling unit and category
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # Remove useless variables
  select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
  # Convert to factors
  mutate_if(is.character, factor) %>% 
  # Add predictors
  left_join(., data_predictors_obs)

## 6.3 Export data ----

save(data_benthic, file = "data/16_model-data/data_benthic_prepared.RData")
