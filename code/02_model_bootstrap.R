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

# 2. Load data ----

load("data/16_model-data/data_benthic_prepared.RData")
load("data/16_model-data/data_predictors_pred.RData")

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
