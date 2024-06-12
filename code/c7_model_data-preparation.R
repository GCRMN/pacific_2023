# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load benthic cover data ----

load("data/09_misc/data-benthic.RData")

# 3. Load and combine predictors ----

## 3.1 Add territory for each site ----

data_eez <- st_read("data/01_background-shp/03_eez/data_eez.shp") %>% 
  select(TERRITORY1) %>% 
  rename(territory = TERRITORY1) %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

data_predictors <- st_read("data/04_site-coords/site-coords_all.shp") %>% 
  st_intersection(., data_eez) %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  mutate(year = 2000) %>% 
  tidyr::complete(year = seq(1980, 2023), nesting(site_id, type, territory, decimalLongitude, decimalLatitude))

## 3.2 Estimate human population for missing years ----

### 3.2.1 Load the data ----

pred_human_pop <- read.csv("data/10_predictors/pred_human-pop.csv") %>% 
  rename(year = system.index) %>% 
  mutate(year = as.numeric(str_split_fixed(year, "_", 9)[,6]))

### 3.2.2 Create the function ----

extract_coeff <- function(data){
  
  model <- lm(pred_population ~ year, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

### 3.2.3 Map over the function ----

pred_human_pop <- pred_human_pop %>% 
  # Extract linear model coefficients
  group_by(site_id, type) %>% 
  group_modify(~extract_coeff(data = .x)) %>% 
  ungroup() %>% 
  left_join(pred_human_pop, .) %>% 
  # Estimate human population for all years between 2000 and 2023
  tidyr::complete(year = seq(2000, 2023), nesting(site_id, type, intercept, slope)) %>% 
  mutate(pred_population = (year*slope)+intercept) %>% 
  select(-intercept, -slope) %>% 
  mutate(pred_population = round(pred_population))

data_predictors <- left_join(data_predictors, pred_human_pop)

## 3.3 Add other predictors ----

data_predictors <- read.csv("data/10_predictors/pred_elevation.csv") %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_land.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_reef-extent.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_chla_mean.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_chla_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_gravity.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_enso.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_mean_y1 = lag(pred_sst_mean, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_sst_max_y1 = lag(pred_sst_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_min.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_sst_skewness.csv") %>% 
  left_join(data_predictors, .)

#data_predictors <- read.csv("data/10_predictors/pred_sst_sd.csv") %>% 
#  left_join(data_predictors, .)

#data_predictors <- read.csv("data/10_predictors/pred_sst_kurtosis.csv") %>% 
#  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_dhw_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_dhw_max_y1 = lag(pred_dhw_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_cyclones.csv") %>% 
  left_join(data_predictors, .) %>% 
  mutate(across(c(wind_speed_y5, nb_cyclones, nb_cyclones_y5), ~replace_na(.x, 0)))

# 3.4 Round values of predictors ----

data_predictors <- data_predictors %>% 
  # Change unit for SST (°C)
  mutate(across(c(pred_sst_sd, pred_sst_max, 
                  pred_sst_mean, pred_sst_min,
                  pred_sst_max_y1, pred_sst_mean_y1), ~.x/100)) %>%
  # Round to 3 digits
  mutate(across(c(pred_elevation, pred_reefextent, pred_land,
                  pred_enso, pred_chla_mean, pred_chla_sd),
                ~ round(.x, digits = 3))) %>% 
  # Round to 2 digits
  mutate(across(c(pred_sst_sd, pred_sst_skewness,
                  pred_sst_kurtosis, pred_sst_max, pred_sst_mean,
                  pred_sst_min, pred_dhw_max, pred_dhw_max_y1,
                  pred_sst_max_y1, pred_sst_mean_y1),
                ~ round(.x, digits = 2)))

# 4. Split predictors in observed and to predict data ----

## 4.1 Predictors values for sites with observed data ----

data_predictors_obs <- data_predictors %>% 
  filter(type == "obs") %>% 
  select(-type, -site_id, -territory)

save(data_predictors_obs, file = "data/11_model-data/data_predictors_obs.RData")

## 4.2 Predictors values for sites to predict ----

data_predictors_pred <- data_predictors %>% 
  filter(type == "pred") %>% 
  select(-type, -site_id) %>% 
  mutate(datasetID = NA,
         month = NA,
         day = NA,
         verbatimDepth = NA,
         parentEventID = NA,
         eventID = NA)

save(data_predictors_pred, file = "data/11_model-data/data_predictors_pred.RData")

# 5. Summarize data and add predictors ----

## 5.1 Transform the data ----

data_benthic <- data_benthic %>% 
  # 1. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  # 2. Summarise data at the transect level (i.e. mean of photo-quadrats)
  # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
  # This is the case for datasets "0011", "0012", "0013", "0014", and "0043" at least
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup() %>% 
  # 3. Remove values greater than 100 (unlikely but included to avoid any issues later)
  filter(measurementValue <= 100) %>% 
  # 4. Remove useless variables
  select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
  # 5. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 6. Add predictors
  left_join(., data_predictors_obs)

## 5.2 Export the data ----

save(data_benthic, file = "data/11_model-data/data_benthic_prepared.RData")
