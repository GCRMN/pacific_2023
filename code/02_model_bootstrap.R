# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(sf)
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)

# 2. Load data ----

load("data/16_model-data/data_benthic_prepared.RData")
load("data/16_model-data/data_predictors_pred.RData")
load("data/16_model-data/hyperparameters.RData")

# 3. Create the model bootstrap function ----

bootstrap_model <- function(iteration, category_i){
  
  # 1. Sample data (bootstrap) ----
  
  data_i <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category)
  
  data_i <- slice_sample(data_i, n = nrow(data_i), replace = TRUE)
  
  # 2. Split data into training and testing datasets ----
  
  data_split <- initial_split(data_i, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 3. Define the recipe ----
  
  boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors())
  
  # 4. Define the model ----
  
  hyperparameters_i <- hyperparameters %>% 
    filter(category == category_i)
  
  boosted_model <- boost_tree(trees = hyperparameters_i$trees, 
                              min_n = hyperparameters_i$min_n, 
                              tree_depth = hyperparameters_i$tree_depth, 
                              learn_rate = hyperparameters_i$learn_rate) %>% # Model type
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
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(bootstrap = iteration, category = category_i)
  
  # 8. Variable importance ----
  
  result_vip <- final_model %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 100) %>% 
    .$data %>% 
    rename(predictor = 1, importance = 2) %>% 
    mutate(bootstrap = iteration, category = category_i)
  
  # 10. Predict values for new set of predictors ----
  
  final_fitted <- final_model$.workflow[[1]]
  
  results_predicted <- data_predictors_pred %>% 
    mutate(measurementValuepred = predict(final_fitted, data_predictors_pred)$.pred)
  
  # 11. Summarise predictions ----
  
  # 11.1 For the entire Pacific region ----
  
  results_region <- results_predicted %>% 
    group_by(year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(bootstrap = iteration, category = category_i)
  
  # 11.2 For each territory ----
  
  results_territory <- results_predicted %>% 
    group_by(territory, year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(bootstrap = iteration, category = category_i)
  
  # 12. Return the results ----
  
  return(lst(model_performance,
             result_vip,
             results_region, 
             results_territory))
  
}

# 4. Map over the function (bootstrap) ----

n_bootstrap <- 5

## 4.1 Hard corals ----

results_hard_coral <- map(1:n_bootstrap,
                          ~ bootstrap_model(iteration = ., category_i = "Hard coral")) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(results_hard_coral, file = "data/16_model-data/raw-results_hard-coral.RData")

## 4.2 Macroalgae ----

results_macroalgae <- map(1:n_bootstrap,
                          ~ bootstrap_model(iteration = ., category_i = "Macroalgae")) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(results_macroalgae, file = "data/16_model-data/raw-results_macroalgae.RData")

## 4.3 Turf algae ----

results_turf_algae <- map(1:n_bootstrap,
                          ~ bootstrap_model(iteration = ., category_i = "Turf algae")) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(results_turf_algae, file = "data/16_model-data/raw-results_turf-algae.RData")

## 4.4 Coralline algae ----

results_coralline_algae <- map(1:n_bootstrap,
                               ~ bootstrap_model(iteration = ., category_i = "Coralline algae")) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(results_coralline_algae, file = "data/16_model-data/raw-results_coralline-algae.RData")

# 5. Combine and export raw results ----

results_all_raw <- lst(results_hard_coral, results_coralline_algae, results_macroalgae, results_turf_algae) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(results_all_raw, file = "data/16_model-data/raw-results_all.RData")
