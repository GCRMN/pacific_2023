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

load("data/11_model-data/data_benthic_prepared.RData")
load("data/11_model-data/data_predictors_pred.RData")
load("data/12_model-output/model_tuning.RData")

model_hyperparams <- tuning_results$model_hyperparams
  
# 3. Create the function ----

model_bootstrap <- function(category_i, bootstrap_i, pdp){
  
  # 1. Data preparation
  
  ## 1.1 Filter the category
  
  data_split <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category)
  
  ## 1.2 Sample with replacement by territory (for bootstrap)
  
  data_split <- slice_sample(data_split, n = nrow(data_split), replace = TRUE)
  
  ## 1.3 Split into training and testing data
  
  data_split <- initial_split(data_split, prop = 3/4)
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 2. Fit the model
  
  ## 2.1 Define the recipe
  
  boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors())
  
  ## 2.2 Filter model_hyperparams
  
  model_hyperparams_i <- model_hyperparams %>% 
    filter(category == category_i)
  
  ## 2.3 Define the model
  
  boosted_model <- boost_tree(learn_rate = model_hyperparams_i$learn_rate,
                              trees = model_hyperparams_i$trees, 
                              min_n = model_hyperparams_i$min_n, 
                              tree_depth = model_hyperparams_i$tree_depth) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 2.4 Define the workflow
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  ## 2.5 Fit the final model
  
  final_model <- boosted_workflow %>%
    last_fit(data_split)
  
  # 3. Model outputs
  
  ## 3.1 Model performance
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ## 3.2 Variable importance
  
  result_vip <- final_model %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 100) %>% 
    .$data %>% 
    rename(predictor = 1, importance = 2) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  ## 3.3 Partial Dependence Plots
  
  final_fitted <- final_model$.workflow[[1]]
  
  if(pdp == TRUE){
    
    model_explain <- explain_tidymodels(model = final_fitted, 
                                        data = dplyr::select(data_train, -measurementValue), 
                                        y = data_train$measurementValue)
    
    result_pdp <- model_profile(explainer = model_explain,
                                N = NULL, 
                                center = FALSE,
                                type = "partial",
                                variables = c("year", "decimalLongitude", "decimalLatitude", "nb_cyclones",
                                              "wind_speed_y5", "nb_cyclones_y5", "pred_elevation", "pred_reefextent",
                                              "pred_land", "pred_gravity", "pred_enso", "pred_sst_sd", "pred_sst_skewness",
                                              "pred_sst_kurtosis", "pred_sst_max", "pred_sst_max_y1", "pred_sst_mean",
                                              "pred_sst_mean_y1", "pred_sst_min", "pred_dhw_max", "pred_dhw_max_y1",
                                              "pred_chla_mean", "pred_chla_sd", "pred_population", "verbatimDepth"),
                                variable_splits_type = "uniform") %>% 
      .$agr_profiles %>% 
      as_tibble(.) %>% 
      select(-"_label_", -"_ids_") %>% 
      rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_") %>% 
      mutate(category = category_i, bootstrap = bootstrap_i)
    
  }
  
  # 4. Predictions
  
  ## 4.1 Predict values for new set of predictors
  
  results_predicted <- data_predictors_pred %>% 
    mutate(measurementValuepred = predict(final_fitted, data_predictors_pred)$.pred)
  
  ## 4.2 Summarise predictions over space and time
  
  ### 4.2.1 For the entire Pacific region
  
  results_region <- results_predicted %>% 
    group_by(year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, region = "Pacific", territory = "All")
  
  ### 4.2.2 For each territory
  
  results_territory <- results_predicted %>% 
    group_by(territory, year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, region = "Pacific")
  
  ### 4.2.3 Combine results 
  
  result_trends <- bind_rows(results_region, results_territory)
  
  # 5. Return the results
  
  if(pdp == TRUE){
    
    return(lst(model_performance,
               result_vip,
               result_pdp,
               result_trends))
    
  }else{
    
    return(lst(model_performance,
               result_vip,
               result_trends))
    
  }
  
}

# 4. Map over the function ----

## 4.1 Hard coral ----

model_results <- map(1:5, ~model_bootstrap(category_i = "Hard coral",
                                           bootstrap_i = .,
                                           pdp = FALSE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(model_results, file = "data/12_model-output/model_results_hard-coral.RData")

## 4.2 Macroalgae ----

model_results <- map(1:30, ~model_bootstrap(category_i = "Macroalgae",
                                           bootstrap_i = .,
                                           pdp = FALSE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(model_results, file = "data/12_model-output/model_results_macroalgae.RData")

## 4.3 Turf algae ----

model_results <- map(1:10, ~model_bootstrap(category_i = "Turf algae",
                                           bootstrap_i = .,
                                           pdp = FALSE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(model_results, file = "data/12_model-output/model_results_turf-algae.RData")

## 4.4 Coralline algae ----

model_results <- map(1:10, ~model_bootstrap(category_i = "Coralline algae",
                                           bootstrap_i = .,
                                           pdp = FALSE)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(model_results, file = "data/12_model-output/model_results_coralline-algae.RData")
