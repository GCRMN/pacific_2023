# 1. Load packages ----

library(tidyverse)
library(googledrive)
library(sf)
sf_use_s2(FALSE)
source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/prepare_benthic_data.R")
source("code/function/download_predictors.R")

# 2. Load data ----

## 2.1 Benthic cover data ----

load("data/09_misc/data-benthic.RData")

## 2.2 Download predictors extracted through GEE ----

download_predictors()

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
  mutate(site_id = as.numeric(site_id),
         year = 2000) %>% 
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

data_predictors <- read.csv("data/10_predictors/pred_sst_sd.csv") %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_dhw_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  mutate(pred_dhw_max_y1 = lag(pred_dhw_max, n = 1)) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_ssta_max.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_ssta_mean.csv") %>% 
  arrange(site_id, type, year) %>% 
  group_by(site_id, type) %>% 
  left_join(data_predictors, .)

data_predictors <- read.csv("data/10_predictors/pred_cyclones.csv") %>% 
  left_join(data_predictors, .) %>% 
  mutate(across(c(wind_speed_y5, nb_cyclones, nb_cyclones_y5), ~replace_na(.x, 0)))

# 3.4 Round values of predictors ----

data_predictors2 <- data_predictors %>% 
  # Change unit for SST (°C)
  mutate(across(c(pred_sst_sd), ~.x/100)) %>%
  # Round to 4 digits
  mutate(across(c(pred_sst_sd, pred_sst_skewness,
                  pred_sst_max, pred_sst_mean,
                  pred_sst_min,
                  pred_dhw_max, pred_dhw_max_y1,
                  pred_sst_max_y1, pred_sst_mean_y1,
                  pred_ssta_max, pred_ssta_mean,
                  pred_elevation, pred_reefextent, pred_land,
                  pred_enso, pred_chla_mean, pred_chla_sd),
                ~ round(.x, digits = 4)))

# 4. Feature selection (remove correlated predictors) ----

## 4.1 Find correlation coefficients between predictors ----

data_correlation <- data_predictors %>% 
  select(-site_id, -year, -type, -territory) %>% 
  cor(., use = "complete.obs") %>% 
  round(., 2) %>% 
  as_tibble(.) %>% 
  mutate(predictor_a = colnames(.)) %>% 
  pivot_longer(1:ncol(.)-1, names_to = "predictor_b", values_to = "coefficient") %>% 
  filter(predictor_a != predictor_b) %>% 
  arrange(coefficient) %>% 
  # Remove odd row number to remove duplicated pairs of predictors
  filter(row_number(.) %% 2 == 0)

## 4.2 Remove useless predictors based on correlation ----

data_predictors <- data_predictors %>% 
  select(-pred_sst_mean)

# 5. Export predictors for data to predict ----

data_predictors_pred <- data_predictors %>% 
  filter(type == "pred") %>% 
  select(-type, -site_id) %>% 
  mutate(datasetID = NA,
         month = NA,
         verbatimDepth = NA,
         parentEventID = NA)

save(data_predictors_pred, file = "data/11_model-data/data_predictors_pred.RData")

# 6. Summarize data and add predictors ----

data_site_coords_obs <- st_read("data/04_site-coords/site-coords_obs.shp") %>% 
  mutate(site_id = as.numeric(site_id),
         decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

data_benthic <- data_benthic %>% 
  # 1. Prepare benthic data
  prepare_benthic_data(data = .) %>%
  # 2. Remove useless variables
  select(-region, -subregion, -ecoregion, -country, -locality, -habitat, -eventDate, -day) %>% 
  # 3. Convert to factors
  mutate_if(is.character, factor) %>% 
  # 4. Add site_id and type (to join on step 7)
  left_join(., data_site_coords_obs) %>% 
  # 5. Add predictors
  left_join(., data_predictors %>%
              filter(type == "obs") %>% 
              # Remove lat and long because GEE slightly modify these, which break the join
              select(-decimalLongitude, -decimalLatitude),
            by = c("site_id", "year", "territory", "type")) %>% 
  select(-site_id, -type)

save(data_benthic, file = "data/11_model-data/data_benthic_prepared.RData")

# 7. Check the number of NA per predictors ----

## 7.1 Sites for predictions ----

pred_na_pred <- data_predictors_pred %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_pred),
         percent = (na*100)/n,
         type = "Sites prediction") %>% 
  select(-n, -na) %>% 
  filter(!(predictor %in% c("measurementValue", "category")))

## 7.2 Sites for observed data ----

pred_na_obs <- data_benthic %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_benthic),
         percent = (na*100)/n,
         type = "Sites observed") %>% 
  select(-n, -na) %>% 
  filter(!(predictor %in% c("measurementValue", "category")))

## 7.3 Combine data ----

pred_na <- bind_rows(pred_na_pred, pred_na_obs)

## 7.4 Make the plot ----

ggplot(data = pred_na, aes(x = fct_reorder(predictor, desc(predictor)), y = percent)) +
  geom_bar(stat = "identity", fill = "#c44d56", width = 0.6) +
  coord_flip() +
  lims(y = c(0, 100)) +
  facet_grid(~type) +
  labs(x = NULL, y = "Percentage of NA") +
  theme_graph()

ggsave("figs/04_supp/02_model/na_predictors.png", width = 8, height = 12)

## 7.5 Raw values ----

pred_na <- pred_na %>% 
  pivot_wider(names_from = type, values_from = percent) %>% 
  mutate(predictor = str_remove_all(predictor, "pred_")) %>% 
  arrange(predictor) %>% 
  rename(training = `Sites observed`, prediction = `Sites prediction`) %>% 
  select(predictor, training, prediction)
