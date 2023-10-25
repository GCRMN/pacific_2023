# 1. Load packages ----

library(tidyverse) # Core tidyverse packages

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

# 3. Load model results ----

load("data/16_model-results/results-model-coral.RData")

# 4. Variable importance ----

data_results %>% 
  .$result_vip %>% 
  ggplot(data = ., aes(x = predictor, y = importance)) +
  geom_point() +
  coord_flip()

# 5. PDP ----

# 5.1 For entire Pacific region --

ggplot(data = data_results$result_pdp_region, aes(x = x, y = y_pred, group = iteration)) +
  geom_line() +
  lims(y = c(0, 100)) +
  labs(x = "Year", y = "Hard coral cover (%)")

data_pdp_region <- data_results %>% 
  .$result_pdp_region %>% 
  group_by(x) %>% 
  summarise(mean = mean(y_pred, na.rm = TRUE),
            upper_ci_95 = quantile(y_pred, probs = 0.975),
            lower_ci_95 = quantile(y_pred, probs = 0.025),
            upper_ci_80 = quantile(y_pred, probs = 0.90),
            lower_ci_80 = quantile(y_pred, probs = 0.10)) %>% 
  ungroup()

ggplot(data = data_pdp_region) +
  geom_ribbon(aes(x = x, ymin = lower_ci_95, ymax = upper_ci_95), fill = "lightgrey") +
  geom_ribbon(aes(x = x, ymin = lower_ci_80, ymax = upper_ci_80), fill = "darkgrey") +
  geom_line(aes(x = x, y = mean), linewidth = 1, col = "black") +
  lims(y = c(0, 100)) +
  labs(x = "Year", y = "Hard coral cover (%)")

# 5.2 For territories --

ggplot(data = data_results$result_pdp_territory, aes(x = x, y = y_pred, group = iteration)) +
  geom_line() +
  facet_grid(~territory, scales = "free") +
  lims(y = c(0, 100)) +
  labs(x = "Year", y = "Hard coral cover (%)")

data_pdp_territory <- data_results %>% 
  .$result_pdp_territory %>% 
  group_by(x, territory) %>% 
  summarise(mean = mean(y_pred, na.rm = TRUE),
            upper_ci_95 = quantile(y_pred, probs = 0.975),
            lower_ci_95 = quantile(y_pred, probs = 0.025),
            upper_ci_80 = quantile(y_pred, probs = 0.90),
            lower_ci_80 = quantile(y_pred, probs = 0.10)) %>% 
  ungroup()

ggplot(data = data_pdp_territory) +
  geom_ribbon(aes(x = x, ymin = lower_ci_95, ymax = upper_ci_95), fill = "lightgrey") +
  geom_ribbon(aes(x = x, ymin = lower_ci_80, ymax = upper_ci_80), fill = "grey") +
  geom_line(aes(x = x, y = mean), linewidth = 1, col = "#446CB3") +
  facet_grid(~territory, scales = "free") +
  lims(y = c(0, 100)) +
  labs(x = "Year", y = "Hard coral cover (%)")
