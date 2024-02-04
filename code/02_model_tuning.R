# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages

# 2. Load data ----

load("data/16_model-data/data_benthic_prepared.RData")

# 3. Create the function for hyperparameters tuning ----

model_hypertuning <- function(data_benthic, category_i){
  
  # 1. Split data ----
  
  data_split <- data_benthic %>% 
    filter(category == category_i) %>% 
    select(-category) %>% 
    initial_split(., prop = 3/4)
  
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
    add_recipe(boosted_recipe) %>% 
    add_model(boosted_model)
  
  # 5. Hyperparameters tuning ----
  
  # 5.1 Create the grid ----
  
  tune_grid <- grid_max_entropy(trees(),
                                tree_depth(),
                                learn_rate(),
                                min_n(),
                                size = 20)
  
  # 5.2 Run the hyperparameters tuning ----
  
  tuned_results <- tune_grid(boosted_workflow,
                             resamples = bootstraps(data_train, times = 5),
                             grid = tune_grid)
  
  # 5.3 Get best set of parameters ----
  
  model_hyperparams <- select_best(tuned_results, metric = "rmse") %>% 
    select(-".config") %>% 
    as_tibble(.) %>% 
    mutate(category = category_i, .before = "trees")
  
  # 6. Return the results ----
  
  return(model_hyperparams)

}

# 4. Map over the function ----

hyperparameters <- map_dfr(unique(data_benthic$category), 
                           ~model_hypertuning(data_benthic = data_benthic, category_i = .))

# 5. Export the results ----

save(hyperparameters, file = "data/16_model-data/hyperparameters.RData")
