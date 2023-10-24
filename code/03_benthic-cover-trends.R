# 1. Load packages ----

library(tidyverse) # Core tidyverse packages

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

# 3. Load model results ----

load("data/16_model-results/results-model-coral.RData")

# 4. PDP ----

ggplot(data = data_results, aes(x = x, y = y_pred)) +
  geom_line() +
  facet_grid(~territory, scales = "free")

# 5. Variable importance ----

data_results %>% 
  select(-x, -y_pred) %>% 
  distinct() %>% 
  ggplot(data = ., aes(x = predictor, y = importance)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip()
