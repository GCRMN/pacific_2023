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

plan(multisession(workers = 2)) # Set parallelization with 6 cores

source("code/function/summarise_cover.R")

# 2. Data preparation ----

# 2.1 Load benthic cover data ----

load("data/04_data-benthic.RData")

# 2.2 Load and combine predictors ----

site_coords <- st_read("data/15_benthic-site-coords/benthic-site-coords.shp") %>% 
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

data_pred_gravity <- read.csv("data/14_predictors/pred_gravity.csv")

data_pred <- site_coords %>% 
  left_join(., data_pred_population) %>% 
  left_join(., data_pred_elevation) %>% 
  left_join(., data_pred_land) %>% 
  left_join(., data_pred_reef_extent) %>% 
  left_join(., data_pred_chla) %>% 
  left_join(., data_pred_sst_mean) %>% 
  left_join(., data_pred_sst_sd) %>% 
  left_join(., data_pred_gravity) %>% 
  select(-site_id)

# 2.3 Load weights ----

load("data/12_weight-model-benthic-cover.RData")

# 2.4 Summarize data and add predictors and weight ----

data_benthic <- data_benthic %>% 
  filter(!(datasetID %in% c("0011", "0012", "0013", "0014", "0020"))) %>% 
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              TRUE ~ category)) %>% 
  # Filter and summarize data
  summarise_cover(., category_i = "Hard coral") %>% 
  # Add predictors
  left_join(., data_pred) %>% 
  # Add weight
  left_join(., data_weight) %>% 
  mutate(weight = importance_weights(weight))

rm(data_pred_elevation, data_pred_population, data_pred_reef_extent,
   data_pred_land, data_pred_chla, data_pred_sst_mean, data_pred_sst_sd,
   data_pred, data_weight, site_coords)

# 3. Create a function ---- 

model_steps <- function(data_benthic, n_bootstrap){
  
  # 1. Split data ----
  
  data_split <- initial_split(data_benthic, prop = 1/5)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 2. Define the recipe ----
  
  boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors())
  
  # 3. Define the model ----
  
  boosted_model <- boost_tree(trees = tune(), 
                              min_n = tune(), 
                              tree_depth = tune(), 
                              learn_rate = tune()) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  # 4. Define the workflow ----
  
  boosted_workflow <- workflow() %>%
    add_case_weights(weight) %>% 
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  # 5. Hyperparameters tuning ----
  
  # 5.1 Create the grid ----
  
  tune_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                learn_rate(),
                                min_n(),
                                size = 10)
  
  # 5.2 Run the hyperparameters tuning ----
  
  tuned_results <- tune_grid(boosted_workflow,
                             resamples = bootstraps(data_train, times = 5),
                             grid = tune_grid)
  
  # 5.3 Get best set of parameters ----
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config")
  
  # 6. Create a function for bootstrap ----
  
  model_bootstrap <- function(iteration){
    
    # 1. Sample data with replacement
    
    data_benthic_i <- sample_n(data_benthic, size = nrow(data_benthic), replace = TRUE)
    
    # 2. Split data ----
    
    data_split <- initial_split(data_benthic_i, prop = 1/4)
    data_train <- training(data_split)
    data_test <- testing(data_split)
    
    # 3. Run the model with the best set of parameters ----
    
    final_model <- boosted_workflow %>%
      finalize_workflow(model_hyperparams) %>%
      last_fit(data_split)
    
    # 4. Evaluate final model performance ----
    
    # 4.1 Model performance metrics ----
    
    model_performance <- collect_metrics(final_model) %>% 
      select(-".estimator", -".config") %>% 
      pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
      mutate(iteration = iteration, .before = 1)
    
    # 4.2 Dataviz (Pred. vs Obs.) ----
    
    final_fitted <- final_model$.workflow[[1]]
    
    result_pred_obs <- data_test %>% 
      mutate(measurementValuepred = predict(final_fitted, data_test)$.pred) %>% 
      rename(observed = measurementValue, predicted = measurementValuepred) %>% 
      mutate(residual = observed - predicted,
             iteration = iteration, .before = 1)
    
    # 5. Variable importance ----
    
    result_vip <- final_model %>% 
      extract_fit_parsnip() %>% 
      vip(num_features = 100) %>% 
      .$data %>% 
      rename(predictor = 1, importance = 2) %>% 
      mutate(iteration = iteration, .before = 1)
    
    # 6. Partial Dependance Plot (PDP) ----
    
    model_explain <- explain_tidymodels(model = final_fitted, 
                                        data = dplyr::select(data_train, -measurementValue, -weight), 
                                        y = data_train$measurementValue,
                                        weights = data_train$weight)
    
    # 6.1 For territories --
    
    result_pdp_territory <- model_profile(explainer = model_explain,
                                          N = NULL, 
                                          center = FALSE,
                                          type = "partial",
                                          groups = "territory",
                                          variables = "year") %>% 
      .$agr_profiles %>% 
      as_tibble(.) %>% 
      select(-"_label_", -"_ids_") %>% 
      rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_", territory = "_groups_") %>% 
      mutate(iteration = iteration, .before = 1)
    
    # 6.2 For entire region --
    
    result_pdp_region <- model_profile(explainer = model_explain,
                                       N = NULL, 
                                       center = FALSE,
                                       type = "partial",
                                       variables = "year") %>% 
      .$agr_profiles %>% 
      as_tibble(.) %>% 
      select(-"_label_", -"_ids_") %>% 
      rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_") %>% 
      mutate(iteration = iteration, .before = 1)
    
    # 7. Format results ----
    
    model_hyperparams <- model_hyperparams %>% 
      mutate(iteration = iteration, .before = 1)
    
    return(lst(model_hyperparams, 
               model_performance, 
               result_pred_obs,
               result_vip,
               result_pdp_territory,
               result_pdp_region))
    
  }
  
  # 7. Map over the function ----
  
  list_results <- future_map(1:n_bootstrap,
                             ~model_bootstrap(iteration = .),
                             .progress = TRUE)
  
  # 8. Return the results ----
  
  return(list_results)
  
}

data_results <- model_steps(data_benthic, n_bootstrap = 10)

data_results <- map(map_df(data_results, ~ as.data.frame(map(.x, ~ unname(nest(.))))), bind_rows)

save(data_results, file = "data/hard-coral_model-outputs.RData")

# PDP ----

ggplot() +
  geom_line(data = data_results$result_pdp_territory,
            aes(x = x, y = y_pred, group = iteration)) +
  lims(x = c(1987, 2023)) +
  facet_wrap(~territory, scales = "free") +
  lims(y = c(0, NA)) +
  theme_bw() +
  labs(x = "Year", y = "Cover (%)")

# Distribution of residuals ----

ggplot(data = data_results$result_pred_obs, aes(x = residual)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))*100),
                 fill = "#2c82c9", alpha = 0.5) +
  geom_vline(xintercept = 0) +
  theme_bw() +
  facet_wrap(~territory, scales = "free") +
  labs(x = "Residuals (obs. - pred.)", y = "Percentage of observations") +
  lims(x = c(-100, 100))

# Pred. vs Obs. ----

data_abline <- data_results$result_pred_obs %>% 
  select(observed, predicted, territory) %>% 
  group_by(territory) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(predicted ~ observed, data = .)),
         coefficients = map(model, coefficients),
         intercept = map_dbl(coefficients, 1),
         slope = map_dbl(coefficients, 2)) %>% 
  select(territory, intercept, slope)

ggplot(data = data_results$result_pred_obs, aes(x = observed, y = predicted)) +
  geom_point(color = "#2c82c9", alpha = 0.5) +
  geom_abline(slope = 1) +
  geom_abline(data = data_abline, aes(intercept = intercept, slope = slope), col = "red") +
  theme_bw() +
  facet_wrap(~territory, scales = "free") +
  labs(x = "Observed", y = "Predicted") +
  lims(x = c(0, 100), y = c(0, 100))
