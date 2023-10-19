# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(pdp)

################################################################################
# Test with iris dataset
# Based on https://juliasilge.com/blog/mario-kart/
################################################################################

# 1. Split data ----

data_iris <- iris
data_split <- initial_split(data_iris, prop = 1/5)
data_train <- training(data_split)
data_test <- testing(data_split)

# 2. Define the model ----

boosted_model <- boost_tree(trees = 1000, 
                            min_n = tune(), 
                            tree_depth = tune(), 
                            learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# 3. Define the workflow ----

boosted_workflow <- workflow() %>%
  add_model(boosted_model) %>%
  add_formula(Petal.Length ~ .)

# 4. Hyperparameters tuning ----

# 4.1 Create the grid ----

tune_grid <- grid_max_entropy(tree_depth(),
                              learn_rate(),
                              min_n(),
                              size = 20)

# 4.2 Run the hyperparameters tuning ----

tuned_results <- tune_grid(boosted_workflow,
                           resamples = bootstraps(data_train, times = 5),
                           grid = tune_grid)

# 4.3 Get best set of parameters ----

best_params <- select_best(tuned_results, metric = "rmse")

# 5. Run the model with the best set of parameters ----

final_model <- boosted_workflow %>%
  finalize_workflow(best_params) %>%
  last_fit(data_split)

# 6. Evaluate final model performance ----

# 6.1 Model performance metrics ----

collect_metrics(final_model)

# 6.2 Dataviz (Pred. vs Obs.) ----

final_fitted <- final_model$.workflow[[1]]

data_test <- data_test %>% 
  mutate(Petal.Length.pred = predict(final_fitted, data_test)$.pred)

ggplot(data = data_test, aes(x = Petal.Length.pred, y = Petal.Length)) +
  geom_point() +
  geom_abline() +
  labs(x = "Predicted", y = "Observed") +
  theme_light()

# 7. Variable importance ----

final_model %>% 
  extract_fit_parsnip() %>% 
  vip()

# 8. Partial Dependance Plot (PDP) ----

model_explain <- explain_tidymodels(final_fitted, 
                                    data = dplyr::select(data_train, -Petal.Length), 
                                    y = data_train$Petal.Length)

pdp_rf <- model_profile(model_explain,
                        N = NULL, 
                        variables = c("Petal.Width", "Sepal.Width", "Sepal.Length"))

data_pdp <- as_tibble(pdp_rf$agr_profiles) %>% 
  rename(x = "_x_", y = "_yhat_", predictor = "_vname_")

ggplot(data = data_pdp, aes(x = x, y = y)) +
  geom_line() +
  facet_grid(~predictor, scales = "free")
