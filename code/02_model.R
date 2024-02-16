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





# 1. Data preparation ----

start_time <- Sys.time()

category_i <- "Hard coral"

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
                              size = 20)

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
  mutate(category = category_i, learn_rate = learning_rate)

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
  mutate(category = category_i)

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
  mutate(category = category_i)

end_time <- Sys.time()

model_time <- model_time %>% 
  add_row(., tibble(step = "Outputs", 
                    substep = "Var. imp.", 
                    time = end_time - start_time))

## 6.3 Partial Dependence Plots ----

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
                                          "pred_reefextent", "pred_chla", "pred_land", "pred_elevation"),
                            variable_splits_type = "uniform") %>% 
  .$agr_profiles %>% 
  as_tibble(.) %>% 
  select(-"_label_", -"_ids_") %>% 
  rename(x = "_x_", y_pred = "_yhat_", predictor = "_vname_") %>% 
  mutate(category = category_i)

end_time <- Sys.time()

model_time <- model_time %>% 
  add_row(., tibble(step = "Outputs", 
                    substep = "PDP", 
                    time = end_time - start_time))

## 6.4 Predicted (yhat) vs observed (y) ----

start_time <- Sys.time()

result_pred_obs <- data_test %>% 
  mutate(yhat = predict(final_fitted, data_test)$.pred) %>% 
  rename(y = measurementValue) %>% 
  select(territory, y, yhat) %>% 
  mutate(category = category_i)

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
  mutate(category = category_i, region = "Pacific", territory = "All")

### 7.2.2 For each territory ----

results_territory <- results_predicted %>% 
  group_by(territory, year) %>% 
  summarise(mean = mean(measurementValuepred)) %>% 
  ungroup() %>% 
  mutate(category = category_i, region = "Pacific")

### 7.2.3 Combine results ----

result_trends <- bind_rows(results_region, results_territory)

end_time <- Sys.time()

model_time <- model_time %>% 
  add_row(., tibble(step = "Predictions", 
                    substep = NA, 
                    time = end_time - start_time)) %>% 
  mutate(category = category_i)

# 8. Return the results ----

list <- lst(model_performance,
            model_time,
            result_vip,
            result_pdp,
            result_pred_obs,
            result_trends)

save(list, file = "data/16_model-data/test.RData")







# PDP
list$result_pdp %>% 
  ggplot(data = ., aes(x = x, y = y_pred)) +
  geom_line() +
  facet_wrap(~predictor, scales = "free")

# VIP
list$result_vip %>% 
  arrange(desc(importance)) %>% 
  slice_head(n = 10) %>% 
  ggplot(data = ., aes(x = predictor, y = importance)) +
  geom_bar(stat = "identity") +
  coord_flip()

# RMSE and RSQ per territory
list$result_pred_obs %>% 
  mutate(residual = yhat - y,
         res = sum((y - yhat)^2),
         tot = sum((y - mean(y))^2),
         rsq_global = 1 - (res/tot),
         rmse_global = sqrt(sum(residual^2/n()))) %>% 
  group_by(territory, rsq_global, rmse_global) %>% 
  summarise(res = sum((y - yhat)^2),
            tot = sum((y - mean(y))^2),
            rsq = 1 - (res/tot),
            rmse = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  select(-res, -tot)

# Pred. vs Obs.
list$result_pred_obs %>% 
  ggplot(data = ., aes(x = y, y = yhat)) +
  geom_point(alpha = 0.1) +
  geom_abline(slope = 1) +
  labs(x = "Observed value (y)", y = "Predicted value (ŷ)")

# Distribution residuals
list$result_pred_obs %>% 
  mutate(residual = yhat - y) %>% 
  ggplot(data = ., aes(x = residual)) + 
  geom_histogram(aes(y = after_stat(count / sum(count))*100),
                 alpha = 0.5) +
  geom_vline(xintercept = 0) +
  lims(x = c(-100, 100)) +
  labs(x = "Residual (ŷ - y)", y = "Percentage")

# Time
list$model_time %>% 
  mutate(time = seconds_to_period(as.difftime(time)))

list$model_time %>% 
  mutate(time = as.difftime(time)) %>% 
  summarise(total = seconds_to_period(sum(time)))

# Trend Pacific
list$result_trends %>% 
  filter(territory == "All") %>% 
  ggplot(data = ., aes(x = year, y = mean)) +
  geom_point() +
  geom_line()

# Trends territories
list$result_trends %>% 
  filter(territory != "All") %>% 
  ggplot(data = ., aes(x = year, y = mean)) +
  geom_point() +
  geom_line() +
  facet_wrap(~territory, scales = "free")
