# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(sf)
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(pdp)
library(future)
library(furrr)

plan(multisession(workers = 4)) # Set parallelization with 4 cores

# 2. Data preparation ----

## 2.1 Load benthic cover data ----

load("data/04_data-benthic.RData")

## 2.2 Combine predictors ----

### 2.2.1 Load predictor values ----

site_coords <- st_read("data/15_site-coords/site-coords_all.shp") %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

data_pred_elevation <- read.csv("data/14_predictors/pred_elevation.csv") %>% 
  rename(pred_elevation = mean) %>% 
  mutate(pred_elevation = replace_na(pred_elevation, 0))

data_pred_population <- read.csv("data/14_predictors/pred_human-pop.csv") %>% 
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

### 2.2.2 Join values ----

data_predictors <- site_coords %>% 
  left_join(., data_pred_population) %>% 
  left_join(., data_pred_elevation) %>% 
  left_join(., data_pred_land) %>% 
  left_join(., data_pred_reef_extent) %>% 
  left_join(., data_pred_chla) %>% 
  left_join(., data_pred_sst_mean) %>% 
  left_join(., data_pred_sst_sd) %>% 
  left_join(., data_pred_sst_skew) %>% 
  left_join(., data_pred_sst_kurtosis) %>% 
  left_join(., data_pred_gravity)

### 2.2.3 Predictors values for sites with observed data ----

data_predictors_obs <- data_predictors %>% 
  filter(type == "obs") %>% 
  select(-type, -site_id, -territory)

### 2.2.4 Predictors values for sites to predict ----

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

## 2.6 Modify NCRMP data (from semi-quantitative to quantitative) by averaging at the scale of a transect ----

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

## 2.7 Summarize data, add predictors and weight ----

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

rm(data_pred_elevation, data_pred_population, data_pred_reef_extent,
   data_pred_land, data_pred_chla, data_pred_sst_mean, data_pred_sst_sd,
   data_pred_gravity, site_coords, data_benthic_ncrmp,
   data_pred_sst_skew, data_pred_sst_kurtosis, data_predictors)

# 3. Model ----

## 3.1 Filter data ----

data_benthic <- data_benthic %>% 
  filter(category == "Hard coral") %>% 
  select(-category)

## 3.2 Create the model ----

bootstrap_model <- function(iteration){
  
  # 1. Sample data (bootstrap) ----
  
  data_i <- slice_sample(data_benthic, n = nrow(data_benthic), replace = TRUE)
  
  # 2. Split data into training and testing datasets ----
  
  data_split <- initial_split(data_i, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 3. Define the recipe ----
  
  boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors())
  
  # 4. Define the model ----
  
  boosted_model <- boost_tree(trees = 615, 
                              min_n = 16, 
                              tree_depth = 14, 
                              learn_rate = 0.043) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  # 5. Define the workflow ----
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  # 6. Run the model ----
  
  final_model <- boosted_workflow %>%
    last_fit(data_split)
  
  # 7. Extract model performance metrics ----
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate")
  
  # 8. Predict values for new set of predictors ----
  
  final_fitted <- final_model$.workflow[[1]]
  
  results_predicted <- data_predictors_pred %>% 
    mutate(measurementValuepred = predict(final_fitted, data_predictors_pred)$.pred)
  
  # 9. Summarise predictions ----
  
  # 9.1 For the entire Pacific region ----
  
  results_region <- results_predicted %>% 
    group_by(year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(bootstrap = iteration)
  
  # 9.2 For each territory ----
  
  results_territory <- results_predicted %>% 
    group_by(territory, year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(bootstrap = iteration)
  
  # 10. Return the results ----
  
  return(lst(results_region, 
             results_territory))
  
}

# 3.3 Map over the function (bootstrap) ----

model_results <- map(1:5, ~ bootstrap_model(iteration = .)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

# 4. Dataviz ----

## 4.1 Entire Pacific region ----

### 4.1.1 Calculate confidence intervals ----

model_results_region <- model_results$results_region %>% 
  rename(cover = mean) %>% 
  group_by(year) %>% 
  summarise(mean = mean(cover),
            sd = sd(cover),
            n = length(cover)) %>% 
  ungroup() %>% 
  mutate(t_score_95 = qt(p = 0.05/2, df = n, lower.tail = FALSE),
         lower_ci_95 = mean - (t_score_95*(sd/sqrt(n))),
         upper_ci_95 = mean + (t_score_95*(sd/sqrt(n))),
         t_score_80 = qt(p = 0.2/2, df = n, lower.tail = FALSE),
         lower_ci_80 = mean - (t_score_80*(sd/sqrt(n))),
         upper_ci_80 = mean + (t_score_80*(sd/sqrt(n)))) %>% 
  select(-sd, -n, -t_score_95, -t_score_80)

### 4.1.2 Make the plot ----

ggplot(data = model_results_region) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95), fill = "lightgrey") +
  geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80), fill = "grey") +
  geom_line(aes(x = year, y = mean))

## 4.2 Territories ----

### 4.2.1 Calculate confidence intervals ----

model_results_territory <- model_results$results_territory %>% 
  rename(cover = mean) %>% 
  group_by(year, territory) %>% 
  summarise(mean = mean(cover),
            sd = sd(cover),
            n = length(cover)) %>% 
  ungroup() %>% 
  mutate(t_score_95 = qt(p = 0.05/2, df = n, lower.tail = FALSE),
         lower_ci_95 = mean - (t_score_95*(sd/sqrt(n))),
         upper_ci_95 = mean + (t_score_95*(sd/sqrt(n))),
         t_score_80 = qt(p = 0.2/2, df = n, lower.tail = FALSE),
         lower_ci_80 = mean - (t_score_80*(sd/sqrt(n))),
         upper_ci_80 = mean + (t_score_80*(sd/sqrt(n)))) %>% 
  select(-sd, -n, -t_score_95, -t_score_80)

### 4.2.2 Make the plot ----

ggplot(data = model_results_territory) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95), fill = "lightgrey") +
  geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80), fill = "grey") +
  geom_line(aes(x = year, y = mean)) +
  facet_wrap(~territory, scales = "free")
