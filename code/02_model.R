# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(sf)
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(future)
library(furrr)

options(future.globals.maxSize= 3000*1024^2) # 3000 mb
plan(multisession, workers = 4)

# 2. Load data ----

load("data/11_model-data/data_benthic_prepared.RData")
load("data/11_model-data/data_predictors_pred.RData")

# 3. Create the model workflow ----

model_workflow <- function(category_i, bootstrap_i, pdp = FALSE){
  
  # 1. Data preparation ----
  
  start_time <- Sys.time()
  
  data_split <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category) %>% 
    initial_split(., prop = 3/4)
  
  data_train <- training(data_split)
  data_test <- testing(data_split)
  
  # 2. Define the recipe ----
  
  boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
    step_dummy(all_nominal_predictors())
  
  end_time <- Sys.time()
  
  model_time <- tibble(step = "Data preparation", substep = NA, time = end_time - start_time)
  
  # 3. First hyperparameter tuning step (learning rate) ----
  
  start_time <- Sys.time()
  
  ## 3.1 Define the model ----
  
  boosted_model <- boost_tree(learn_rate = tune()) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 3.2 Define the workflow ----
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  ## 3.3 Hyperparameters tuning ----
  
  ### 3.3.1 Create the grid ----
  
  tune_grid <- grid_max_entropy(learn_rate(),
                                size = 30)
  
  ### 3.3.2 Run the hyperparameters tuning ----
  
  tuned_results <- tune_grid(boosted_workflow,
                             resamples = vfold_cv(data_train, v = 5),
                             grid = tune_grid)
  
  ### 3.3.3 Get best set of parameters ----
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config") %>% 
    as_tibble(.) %>% 
    mutate(category = category_i)
  
  learning_rate <- model_hyperparams$learn_rate
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Tuning", 
                      substep = "Learning rate", 
                      time = end_time - start_time))
  
  # 4. Second hyperparameter tuning step (other hyperparameters) ----
  
  start_time <- Sys.time()
  
  ## 4.1 Define the model ----
  
  boosted_model <- boost_tree(learn_rate = learning_rate,
                              trees = tune(), 
                              min_n = tune(), 
                              tree_depth = tune()) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 4.2 Define the workflow ----
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  ## 4.3 Hyperparameters tuning ----
  
  ### 4.3.1 Create the grid ----
  
  tune_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                min_n(),
                                size = 5)
  
  ### 4.3.2 Run the hyperparameters tuning ----
  
  tuned_results <- tune_grid(boosted_workflow,
                             resamples = bootstraps(data_train, times = 5),
                             grid = tune_grid)
  
  ### 4.3.3 Get best set of parameters ----
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config") %>% 
    as_tibble(.) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, learn_rate = learning_rate)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Tuning", 
                      substep = "Other parameters", 
                      time = end_time - start_time))
  
  # 5. Final fit ----
  
  start_time <- Sys.time()
  
  ## 5.1 Define the model ----
  
  boosted_model <- boost_tree(learn_rate = model_hyperparams$learn_rate,
                              trees = model_hyperparams$trees, 
                              min_n = model_hyperparams$min_n, 
                              tree_depth = model_hyperparams$tree_depth) %>% # Model type
    set_engine("xgboost") %>% # Model engine
    set_mode("regression") # Model mode
  
  ## 5.2 Define the workflow ----
  
  boosted_workflow <- workflow() %>%
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  ## 5.3 Fit the final model ----
  
  final_model <- boosted_workflow %>%
    last_fit(data_split)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Final fit", 
                      substep = NA, 
                      time = end_time - start_time))
  
  # 6. Model outputs ----
  
  ## 6.1 Model performance ----
  
  start_time <- Sys.time()
  
  model_performance <- collect_metrics(final_model) %>% 
    select(-".estimator", -".config") %>% 
    pivot_wider(names_from = ".metric", values_from = ".estimate") %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Outputs", 
                      substep = "Performance", 
                      time = end_time - start_time))
  
  ## 6.2 Variable importance ----
  
  start_time <- Sys.time()
  
  result_vip <- final_model %>% 
    extract_fit_parsnip() %>% 
    vip(num_features = 100) %>% 
    .$data %>% 
    rename(predictor = 1, importance = 2) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Outputs", 
                      substep = "Var. imp.", 
                      time = end_time - start_time))
  
  ## 6.3 Partial Dependence Plots ----
  
  if(pdp == TRUE){
  
  start_time <- Sys.time()
  
  final_fitted <- final_model$.workflow[[1]]
  
  model_explain <- explain_tidymodels(model = final_fitted, 
                                      data = dplyr::select(data_train, -measurementValue), 
                                      y = data_train$measurementValue)
  
  result_pdp <- model_profile(explainer = model_explain,
                              N = NULL, 
                              center = FALSE,
                              type = "partial",
                              variables = c("year", "decimalLatitude", "decimalLongitude", "pred_population",
                                            "pred_reefextent", "pred_dhw_max", "pred_enso", "pred_elevation"),
                              variable_splits_type = "uniform") %>% 
    .$agr_profiles %>% 
    as_tibble(.) %>% 
    select(-"_label_", -"_ids_") %>% 
    rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_") %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Outputs", 
                      substep = "PDP", 
                      time = end_time - start_time))
  
  }
  
  ## 6.4 Predicted (yhat) vs observed (y) ----
  
  start_time <- Sys.time()
  
  result_pred_obs <- data_test %>% 
    mutate(yhat = predict(final_fitted, data_test)$.pred) %>% 
    rename(y = measurementValue) %>% 
    select(territory, y, yhat) %>% 
    mutate(category = category_i, bootstrap = bootstrap_i)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Outputs", 
                      substep = "Pred. vs obs.", 
                      time = end_time - start_time))
  
  # 7. Predictions ----
  
  ## 7.1 Predict values for new set of predictors ----
  
  start_time <- Sys.time()
  
  results_predicted <- data_predictors_pred %>% 
    mutate(measurementValuepred = predict(final_fitted, data_predictors_pred)$.pred)
  
  ## 7.2 Summarise predictions over space and time ----
  
  ### 7.2.1 For the entire Pacific region ----
  
  results_region <- results_predicted %>% 
    group_by(year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, region = "Pacific", territory = "All")
  
  ### 7.2.2 For each territory ----
  
  results_territory <- results_predicted %>% 
    group_by(territory, year) %>% 
    summarise(mean = mean(measurementValuepred)) %>% 
    ungroup() %>% 
    mutate(category = category_i, bootstrap = bootstrap_i, region = "Pacific")
  
  ### 7.2.3 Combine results ----
  
  result_trends <- bind_rows(results_region, results_territory)
  
  end_time <- Sys.time()
  
  model_time <- model_time %>% 
    add_row(., tibble(step = "Predictions", 
                      substep = NA, 
                      time = end_time - start_time)) %>% 
    mutate(category = category_i,
           bootstrap = bootstrap_i)
  
  # 8. Return the results ----
  
  if(pdp == TRUE){
    
    return(lst(model_hyperparams,
               model_performance,
               model_time,
               result_vip,
               result_pdp,
               result_pred_obs,
               result_trends))
    
  }else{
    
    return(lst(model_hyperparams,
               model_performance,
               model_time,
               result_vip,
               result_pred_obs,
               result_trends))
    
  }
  
}

model_results <- future_map(1:4, ~model_workflow(category_i = "Hard coral", bootstrap_i = .)) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

save(model_results, file = "data/12_model-output/model_results_hard-coral.RData")

load("data/12_model-output/model_results_hard-coral.RData")







