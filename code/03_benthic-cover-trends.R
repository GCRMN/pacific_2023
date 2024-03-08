# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

# 3. Load and combine model results ----

## 3.1 Load data ----

load("data/12_model-output/model_results_hard-coral.RData")
model_results_coral <- model_results

load("data/12_model-output/model_results_macroalgae.RData")
model_results_macroalgae <- model_results

load("data/12_model-output/model_results_turf-algae.RData")
model_results_turf <- model_results

load("data/12_model-output/model_results_coralline-algae.RData")
model_results_cca <- model_results

## 3.2 Combine data ----

model_results <- lst(model_results_coral, model_results_macroalgae,
                     model_results_turf, model_results_cca) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

## 3.3 Export combined data ----

save(model_results, file = "data/12_model-output/model_results_all.RData")

rm(model_results_coral, model_results_macroalgae, model_results_turf, model_results_cca)

## 3.4 Add the color, text titles and factor levels ----

model_results <- model_results %>% 
  map(., ~ .x %>% mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                                           category == "Coralline algae" ~ palette_second[3],
                                           category == "Macroalgae" ~ palette_second[4],
                                           category == "Turf algae" ~ palette_second[5]),
                         text_title = case_when(category == "Hard coral" ~ 
                                                  glue("**A.**<span style='color:{color}'> {category}</span>"),
                                                category == "Coralline algae" ~ 
                                                  glue("**B.**<span style='color:{color}'> {category}</span>"),
                                                category == "Macroalgae" ~ 
                                                  glue("**C.**<span style='color:{color}'> {category}</span>"),
                                                category == "Turf algae" ~ 
                                                  glue("**D.**<span style='color:{color}'> {category}</span>")),
      category = as.factor(category),
      category = fct_expand(category, "Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
      category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae", "Turf algae")))

# 4. Model characteristics ----

## 4.1 Running time ----

model_results$model_time %>% 
  mutate(time = round(seconds_to_period(as.difftime(time))))

model_results$model_time %>% 
  mutate(time = as.difftime(time)) %>% 
  group_by(category, bootstrap) %>% 
  summarise(total = round(seconds_to_period(sum(time)))) %>% 
  ungroup()

model_results$model_time %>% 
  mutate(time = as.difftime(time)) %>% 
  group_by(category, step, substep) %>% 
  summarise(time = round(seconds_to_period(mean(time)))) %>% 
  ungroup()

model_results$model_time %>% 
  mutate(time = as.difftime(time)) %>% 
  group_by(bootstrap) %>% 
  summarise(time = sum(time)) %>% 
  ungroup() %>% 
  # total_time must be divided by the number of core use to parallelise
  summarise(mean_time_bootstrap = round(seconds_to_period(mean(time))),
            total_time = round(seconds_to_period(sum(time))))

## 4.2 Hyperparameters ----







# 5. Model performance ----

## 5.1 RMSE and R² ----

### 5.1.1 Transform data ----

data_perf <- model_results$result_pred_obs %>% 
  group_by(category, bootstrap, text_title, color) %>% 
  mutate(residual = yhat - y,
         res = sum((y - yhat)^2),
         tot = sum((y - mean(y))^2),
         rsq_global = 1 - (res/tot),
         rmse_global = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  group_by(category, territory, bootstrap, rsq_global,
           rmse_global, text_title, color) %>% 
  summarise(res = sum((y - yhat)^2),
            tot = sum((y - mean(y))^2),
            rsq = 1 - (res/tot),
            rmse = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  select(-res, -tot)

data_perf_mean_all <- data_perf %>% 
  group_by(category, text_title, color) %>% 
  summarise(mean_rmse_all = mean(rmse_global),
            mean_rsq_all = mean(rsq_global)) %>% 
  ungroup()

data_perf_mean_ter <- data_perf %>% 
  group_by(category, territory, text_title, color) %>% 
  summarise(rmse_mean = mean(rmse),
            rsq_mean = mean(rsq)) %>% 
  ungroup() %>% 
  left_join(., data_perf_mean_all)

### 5.1.2 Make the plot for RMSE ----

ggplot(data = data_perf, aes(x = reorder(territory, desc(territory)), color = color, y = rmse)) +
  geom_jitter(alpha = 0.2, width = 0.05) +
  geom_hline(data = data_perf_mean_all, aes(yintercept = mean_rmse_all)) +
  geom_segment(data = data_perf_mean_ter, aes(x = reorder(territory, desc(territory)),
                                                y = rmse_mean, yend = mean_rmse_all),
               color = "black") +
  geom_point(data = data_perf_mean_ter, aes(x = reorder(territory, desc(territory)),
                                            fill = color, y = rmse_mean),
             size = 3.5, shape = 21, color = "white") +
  coord_flip() +
  facet_wrap(~text_title, nrow = 1) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(x = NULL, y = "RMSE") +
  theme(strip.text = element_markdown(hjust = 0.5),
        strip.background = element_rect(fill = "#efeff0", color = NULL))

ggsave("figs/04_supp/02_model/01_perf_rmse.png", width = 10, height = 8, dpi = 600)

### 5.1.3 Make the plot for R² ----

ggplot(data = data_perf, aes(x = reorder(territory, desc(territory)), color = color, y = rsq)) +
  geom_jitter(alpha = 0.2, width = 0.05) +
  geom_hline(data = data_perf_mean_all, aes(yintercept = mean_rsq_all)) +
  geom_segment(data = data_perf_mean_ter, aes(x = reorder(territory, desc(territory)),
                                              y = rsq_mean, yend = mean_rsq_all),
               color = "black") +
  geom_point(data = data_perf_mean_ter, aes(x = reorder(territory, desc(territory)),
                                            fill = color, y = rsq_mean),
             size = 3.5, shape = 21, color = "white") +
  coord_flip() +
  facet_wrap(~text_title, nrow = 1) +
  scale_color_identity() +
  scale_fill_identity() +
  labs(x = NULL, y = "R²") +
  theme(strip.text = element_markdown(hjust = 0.5),
        strip.background = element_rect(fill = "#efeff0", color = NULL))

ggsave("figs/04_supp/02_model/01_perf_rsq.png", width = 10, height = 8, dpi = 600)

rm(data_perf, data_perf_mean_all, data_perf_mean_ter)

## 5.2 Predicted vs observed ----

### 5.2.1 Create the function to make the plot ----

plot_pred_obs <- function(category_i, all = FALSE){
  
  if(all == TRUE){
    
    plot_i <- model_results$result_pred_obs %>%
      ggplot(data = ., aes(x = y, y = yhat, color= color)) +
      geom_point(alpha = 0.1) +
      geom_abline(slope = 1) +
      scale_color_identity() +
      facet_wrap(~text_title, scales = "free") +
      labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
      theme(strip.text = element_markdown(hjust = 0),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = "figs/04_supp/02_model/02_pred-vs-obs_all.png", dpi = 600)
    
  }else{
    
    data_i <- model_results$result_pred_obs %>%
      filter(category == category_i)
    
    plot_i <- ggplot(data = data_i, aes(x = y, y = yhat, color= color)) +
      geom_point(alpha = 0.2) +
      geom_abline(slope = 1) +
      scale_color_identity() +
      facet_wrap(~territory, scales = "free", ncol = 5) +
      labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
      theme(strip.text = element_markdown(hjust = 0.5),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = paste0("figs/04_supp/02_model/02_pred-vs-obs_", 
                                     str_replace_all(str_to_lower(category_i), " ", "-"),
                                     ".png"),
           width = 15, height = 12, dpi = 600)
    
  }
  
}

### 5.2.2 Map over the function ----

plot_pred_obs(all = TRUE)

map(unique(model_results$result_pred_obs$category), ~plot_pred_obs(category_i = .))

rm(plot_pred_obs)

## 5.3 Distribution of residuals ----

### 5.3.1 Create the function to make the plot ----

plot_residuals <- function(category_i, all = FALSE){
  
  if(all == TRUE){
    
    plot_i <- model_results$result_pred_obs %>% 
      mutate(residual = yhat - y) %>% 
      ggplot(data = ., aes(x = residual, fill = color)) + 
      geom_histogram(aes(y = after_stat(count / sum(count))*100),
                     alpha = 0.5) +
      geom_vline(xintercept = 0) +
      scale_fill_identity() +
      facet_wrap(~text_title, scales = "free") +
      lims(x = c(-100, 100)) +
      labs(x = "Residual (ŷ - y)", y = "Percentage") +
      theme(strip.text = element_markdown(hjust = 0),
            strip.background = element_blank())
    
    ggsave(plot_i, filename = "figs/04_supp/02_model/03_distri-residuals_all.png", dpi = 600)
    
    
  }else{
    
    data_i <- model_results$result_pred_obs %>% 
      mutate(residual = yhat - y) %>%
      filter(category == category_i)
    
    plot_i <- ggplot(data = data_i, aes(x = residual, fill = color)) + 
      geom_histogram(aes(y = after_stat(count / sum(count))*100),
                     alpha = 0.5) +
      geom_vline(xintercept = 0) +
      scale_fill_identity() +
      facet_wrap(~territory, scales = "free", ncol = 5) +
      lims(x = c(-100, 100)) +
      labs(x = "Residual (ŷ - y)", y = "Percentage") +
      theme(strip.text = element_markdown(hjust = 0.5),
            strip.background = element_blank())

    ggsave(plot_i, filename = paste0("figs/04_supp/02_model/03_distri-residuals_", 
                                     str_replace_all(str_to_lower(category_i), " ", "-"),
                                     ".png"),
           width = 15, height = 12, dpi = 600)
    
  }
  
}

### 5.3.2 Map over the function ----

plot_residuals(all = TRUE)

map(unique(model_results$result_pred_obs$category), ~plot_residuals(category_i = .))

rm(plot_residuals)

# 6. Model interpretation ----

## 6.1 Variable importance ----

### 6.1.1 Transform data ----

data_imp_summary <- model_results$result_vip %>% 
  mutate(predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  group_by(predictor, category, color) %>% 
  summarise(mean = mean(importance),
            lower_ci_95 = quantile(importance, 0.05),
            lower_ci_80 = quantile(importance, 0.10),
            upper_ci_95 = quantile(importance, 0.95),
            upper_ci_80 = quantile(importance, 0.80)) %>% 
  ungroup() %>% 
  group_by(category, color) %>% 
  arrange(desc(mean)) %>% 
  slice_head(n = 25) %>% 
  ungroup()

data_imp_raw <- model_results$result_vip %>% 
  mutate(predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  left_join(data_imp_summary, .) %>% 
  group_by(category)

### 6.1.2 Create the function to make the plot ----

plot_vimp <- function(category_i){
  
  data_imp_summary_i <- data_imp_summary %>% 
    filter(category == category_i)
  
  data_imp_raw_i <- data_imp_raw %>% 
    filter(category == category_i)
  
  plot_i <- ggplot(data = data_imp_summary_i, aes(x = fct_reorder(predictor, mean), color = color)) +
    geom_jitter(data = data_imp_raw_i,
                aes(x = fct_reorder(predictor, mean), y = importance, color = color),
                alpha = 0.3, width = 0.15) +
    geom_linerange(aes(ymin = lower_ci_95, ymax = upper_ci_95), linewidth = 1) +
    geom_linerange(aes(ymin = lower_ci_80, ymax = upper_ci_80), linewidth = 1.5) +
    geom_point(aes(y = mean), size = 3.5, shape = 21, fill = "white") +
    coord_flip() +
    scale_color_identity() +
    labs(x = NULL, y = "Relative importance (%)")
  
  ggsave(filename = paste0("figs/04_supp/02_model/04_vimp_", str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_i, height = 8, width = 4, dpi = 600)
  
}

## 6.1.3 Map over the function ----

map(unique(data_imp_summary$category), ~plot_vimp(category_i = .))

## 6.2 Partial Dependence Plot ----

model_results$result_pdp %>% 
  ggplot(data = ., aes(x = x, y = y_pred, group = bootstrap)) +
  geom_line() +
  facet_grid(predictor~text_title, scales = "free") +
  theme(strip.text = element_markdown(hjust = 0),
        strip.background = element_blank())

# 7. Benthic cover trends ----

## 7.1 Transform data ----

data_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, region, territory, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.05),
            lower_ci_80 = quantile(cover, 0.10),
            upper_ci_95 = quantile(cover, 0.95),
            upper_ci_80 = quantile(cover, 0.80)) %>% 
  ungroup()

save(data_trends, file = "data/12_model-output/model_results_trends.RData")

## 7.2 Create the function to make a plot ----

plot_trends <- function(category_i, data_trends_i){
  
  data_trends_j <- data_trends_i %>% 
    filter(category == category_i)
  
  plot_j <- ggplot(data = data_trends_j) +
    geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
    geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
    geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
    labs(x = "Year", y = "Cover (%)", title = unique(data_trends_j$text_title)) +
    theme(plot.title = element_markdown(size = 12))
  
  return(plot_j)
  
}

## 7.3 Create the function to combine the plots ----

combine_plot_trends <- function(territory_i){
  
  data_trends_i <- data_trends %>% 
    filter(territory == territory_i)
  
  plot_list <- map(levels(data_trends_i$category),
                   ~plot_trends(category_i = ., data_trends_i = data_trends_i))
  
  plot_i <- wrap_plots(plot_list, ncol = 1)
  
  if(territory_i == "All"){
    
    ggsave(filename = "figs/01_part-1/fig-12.png", plot = plot_i, height = 12, width = 4, dpi = 600)  
    
  }else{
    
    ggsave(filename = paste0("figs/02_part-2/fig-8/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 12, width = 4, dpi = 600)
    
  }
  
}

## 7.4 Map over the function ----

map(unique(data_trends$territory), ~combine_plot_trends(territory_i = .))
