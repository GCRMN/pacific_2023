# 1. Load packages ----

library(tidyverse) # Core tidyverse packages

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

# 3. Load model results ----

load("data/16_model-results/results-model-coral.RData")

# 4. Variable importance ----

# 4.1 Extract data --

data_imp_raw <- data_results %>% 
  .$result_vip %>% 
  mutate(importance = importance*100)

# 4.2 Calculate mean and sd of all iteration --

data_imp_summary <- data_imp_raw %>% 
  group_by(predictor) %>% 
  summarise(mean_imp = mean(importance),
            sd_imp = sd(importance)) %>% 
  ungroup() %>% 
  mutate(predictor = fct_reorder(predictor, mean_imp))

# 4.3 Add mean to order factor --

data_imp_raw <- left_join(data_imp_raw, data_imp_summary) %>% 
  mutate(predictor = fct_reorder(predictor, mean_imp))

# 4.4 Make the plot --

ggplot() +
  geom_jitter(data = data_imp_raw, aes(x = predictor, y = importance),
              alpha = 0.075, col = "#446CB3", width = 0.1) +
  geom_linerange(data = data_imp_summary, aes(x = predictor, 
                                              ymin = mean_imp - sd_imp,
                                              ymax = mean_imp + sd_imp),
                 col = "black", linewidth = 0.7) +
  geom_point(data = data_imp_summary, aes(x = predictor, y = mean_imp),
             fill = "#446CB3", shape = 21, size = 3.25, col = "black") +
  coord_flip() +
  labs(y = "Importance (%)", x = NULL)

# 5. PDP ----

# 5.1 For entire Pacific region --

ggplot() +
  geom_line(data = data_results$result_pdp_region, aes(x = x, y = y_pred, group = iteration)) +
  geom_smooth(data = data_results$result_pdp_region, aes(x = x, y = y_pred)) +
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

ggplot() +
  geom_line(data = data_results$result_pdp_territory, aes(x = x, y = y_pred, group = iteration)) +
  geom_smooth(data = data_results$result_pdp_territory, aes(x = x, y = y_pred)) +
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
