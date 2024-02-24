# 1. Load packages ----

library(tidyverse)
library(sf)

# 2. Load benthic cover data ----

load("data/09_misc/data-benthic.RData")

# 3. Load and combine predictors ----

data_predictors <- st_read("data/04_site-coords/site-coords_all.shp") %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  mutate(year = 2000) %>% 
  tidyr::complete(year = seq(1980, 2023), nesting(site_id, type, territory, decimalLongitude, decimalLatitude))

data_predictors <- read.csv("data/10_predictors/pred_elevation.csv") %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_reef-extent.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_land.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_gravity.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_enso.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_skewness.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_kurtosis.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_max.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_mean.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_min.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_chla_mean.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_dhw_max.csv") %>% 
  left_join(data_predictors, .)

# 4. Round values of predictors ----

data_predictors <- data_predictors %>% 
  # Change unit for SST (°C)
  mutate(across(c(pred_sst_sd, pred_sst_max, 
                  pred_sst_mean, pred_sst_min), ~.x/100)) %>%
  # Round to 3 digits
  mutate(across(c(pred_elevation, pred_reefextent, pred_land,
                  pred_enso, pred_sst_sd, pred_sst_skewness,
                  pred_sst_kurtosis, pred_sst_max, pred_sst_mean,
                  pred_sst_min, pred_chla_mean), ~ round(.x, digits = 3)))

# 5. Split predictors in observed and to predict tibbles ----

## 5.1 Predictors values for sites with observed data ----

data_predictors_obs <- data_predictors %>% 
  filter(type == "obs") %>% 
  select(-type, -site_id, -territory)

save(data_predictors_obs, file = "data/11_model-data/data_predictors_obs.RData")

## 5.2 Predictors values for sites to predict ----

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

save(data_predictors_pred, file = "data/11_model-data/data_predictors_pred.RData")

# 6. Check number of NA per predictor ----

## 6.1 Predictors values for sites with observed data ----

data_predictors_obs %>% 
  select(-decimalLatitude, -decimalLongitude) %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_obs),
         percent = (na*100)/n)

## 6.2 Predictors values for sites to predict ----

data_predictors_pred %>% 
  select(-decimalLatitude, -decimalLongitude) %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_pred),
         percent = (na*100)/n)

# 7. Transform benthic data ----

## 7.1 Modify NCRMP data (from semi-quantitative to quantitative) by averaging at the scale of a transect ----

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

## 7.2 Summarize data and add predictors ----

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

## 7.3 Export data ----

save(data_benthic, file = "data/11_model-data/data_benthic_prepared.RData")
