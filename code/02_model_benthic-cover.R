# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(pdp)
source("code/function/summarise_cover.R")

# 2. Data preparation ----

# 2.1 Load benthic cover data --

load("data/04_data-benthic.RData")

# 2.2 Load predictors --

load("data/14_predictors/population.RData")
load("data/14_predictors/reef_extent.RData")

data_pred <- data_pred_pop %>% 
  filter(year == 2020) %>% 
  select(-year) %>% 
  left_join(., data_pred_reef) %>% 
  rename(pred_pop = population,
         pred_reef = reef_area) # /!\ Nom des variables à changer directement lors de l'extraction

# 2.3. Filter and summarise data ----

data_benthic <- data_benthic %>% 
  filter(territory %in% c("French Polynesia", "New Caledonia", "Fiji")) %>% 
  # Filter and summarise data
  summarise_cover(., category_i = "Hard coral") %>% 
  # Add predictors
  left_join(data_pred)

rm(data_pred_pop, data_pred_reef, data_pred)

# 1. Split data ----

data_split <- initial_split(data_benthic, prop = 1/5)
data_train <- training(data_split)
data_test <- testing(data_split)

# 2. Define the recipe ----

boosted_recipe <- recipe(measurementValue ~ ., data = data_train) %>% 
  step_dummy(all_nominal_predictors())

# 2. Define the model ----

boosted_model <- boost_tree(trees = 1000, 
                            min_n = tune(), 
                            tree_depth = tune(), 
                            learn_rate = tune()) %>% # Model type
  set_engine("xgboost", num.threads = 8) %>% # Model engine
  set_mode("regression") # Model mode

# 3. Define the workflow ----

boosted_workflow <- workflow() %>%
  add_recipe(boosted_recipe) %>% 
  add_model(boosted_model)

# 4. Hyperparameters tuning ----

# 4.1 Create the grid ----

tune_grid <- grid_max_entropy(tree_depth(),
                              learn_rate(),
                              min_n(),
                              size = 5)

# 4.2 Run the hyperparameters tuning ----

tuned_results <- tune_grid(boosted_workflow,
                           resamples = bootstraps(data_train, times = 5),
                           grid = tune_grid)

# 4.3 Get best set of parameters ----

best_params <- select_best(tuned_results, metric = "rmse") %>% 
  select(-".config")

# 5. Run the model with the best set of parameters ----

final_model <- boosted_workflow %>%
  finalize_workflow(best_params) %>%
  last_fit(data_split)

# 6. Evaluate final model performance ----

# 6.1 Model performance metrics ----

model_performance <- collect_metrics(final_model) %>% 
  select(-".estimator", -".config") %>% 
  pivot_wider(names_from = ".metric", values_from = ".estimate")

# 6.2 Dataviz (Pred. vs Obs.) ----

final_fitted <- final_model$.workflow[[1]]

data_test <- data_test %>% 
  mutate(measurementValuepred = predict(final_fitted, data_test)$.pred)

# 7. Variable importance ----

var_importance <- final_model %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 100) %>% 
  .$data %>% 
  rename(predictor = 1, importance = 2)

# 8. Partial Dependance Plot (PDP) ----

model_explain <- explain_tidymodels(final_fitted, 
                                    data = dplyr::select(data_train, -measurementValue), 
                                    y = data_train$measurementValue)

pdp_boosted <- model_profile(explainer = model_explain,
                             N = NULL, 
                             center = FALSE,
                             type = "partial",
                             groups = "territory",
                             variables = "year")

# 9. Format results ----

data_results <- as_tibble(pdp_boosted$agr_profiles) %>% 
  select(-"_label_", -"_ids_") %>% 
  rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_", territory = "_groups_") %>% 
  # Add variable importance
  left_join(., var_importance) %>% 
  # Add model performance 
  bind_cols(., model_performance) %>% 
  # Add best set of hyperparameters used
  bind_cols(., best_params)

# 10. Export results ----

save(data_results, file = "data/results-model-coral.RData")
