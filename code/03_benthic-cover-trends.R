# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

path_models <- "data/16_model-outputs/2024-01-29/model-outputs_"

# 3. Load and combine data ----

# 3.1 Load data --

load(file = paste0(path_models, "hard-coral.RData"))

data_coral <- data_results %>% 
  map(., ~ .x %>% mutate(category = "Hard coral"))

load(file = paste0(path_models, "macroalgae.RData"))

data_macroalgae <- data_results %>% 
  map(., ~ .x %>% mutate(category = "Macroalgae"))

load(file = paste0(path_models, "turf-algae.RData"))

data_turf <- data_results %>% 
  map(., ~ .x %>% mutate(category = "Turf algae"))

load(file = paste0(path_models, "coralline-algae.RData"))

data_coralline <- data_results %>% 
  map(., ~ .x %>% mutate(category = "Coralline algae"))

# 3.2 Combine data --

data_results <- lst(data_coral, data_macroalgae, data_turf, data_coralline) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

rm(data_coral, data_macroalgae, data_turf, data_coralline)

# 4. Partial Dependence Plots (PDP) ----

## 4.1 For the Pacific region ----

### 4.1.1 Transform data ----

data_pdp <- data_results$result_pdp_region %>% 
  group_by(x, category) %>% 
  summarise(y_pred_min = min(y_pred),
            y_pred_max = max(y_pred),
            y_pred_mean = mean(y_pred)) %>%
  ungroup() %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                           category == "Coralline algae" ~ palette_5cols[3],
                           category == "Macroalgae" ~ palette_5cols[4],
                           category == "Turf algae" ~ palette_5cols[5]),
         text_title = case_when(category == "Hard coral" ~ 
                                  glue("**A.**<span style='color:{color}'> {category}</span>"),
                                category == "Coralline algae" ~ 
                                  glue("**B.**<span style='color:{color}'> {category}</span>"),
                                category == "Macroalgae" ~ 
                                  glue("**C.**<span style='color:{color}'> {category}</span>"),
                                category == "Turf algae" ~ 
                                  glue("**D.**<span style='color:{color}'> {category}</span>")))

### 4.1.2 Create the function ----

plot_pdp_category <- function(category_i, data_pdp){
  
  data_pdp_i <- data_pdp %>% 
    filter(category == category_i)
  
  plot_i <- ggplot(data = data_pdp_i) +
    geom_ribbon(aes(ymin = y_pred_min, ymax = y_pred_max, x = x, fill = color), alpha = 0.35) +
    geom_line(aes(x = x, y = y_pred_mean, color = color)) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
    labs(x = "Year", y = "Cover (%)", title = unique(data_pdp_i$text_title)) +
    theme(plot.title = element_markdown(size = 12))
  
  plot_i
  
}

### 4.1.3 Map over the function ----

plot_list <- map(unique(data_pdp$category), ~plot_pdp_category(category_i = ., data_pdp = data_pdp))

plot_i <- wrap_plots(plot_list[[2]], plot_list[[1]], plot_list[[3]], plot_list[[4]], ncol = 1)

ggsave(filename = paste0("figs/01_part-1/fig-12.png"), plot = plot_i, height = 12, width = 4, dpi = 600)  

## 4.2 For countries and territories ----

### 4.2.1 Transform data ----

data_pdp <- data_results$result_pdp_territory %>% 
  group_by(x, category, territory) %>% 
  summarise(y_pred_min = min(y_pred),
            y_pred_max = max(y_pred),
            y_pred_mean = mean(y_pred)) %>%
  ungroup() %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                           category == "Coralline algae" ~ palette_5cols[3],
                           category == "Macroalgae" ~ palette_5cols[4],
                           category == "Turf algae" ~ palette_5cols[5]))

data_letters <- data_pdp %>% 
  select(territory, category) %>% 
  distinct() %>% 
  mutate(category = factor(category, levels = c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"))) %>% 
  arrange(territory, category) %>% 
  group_by(territory) %>% 
  mutate(letter = as.character(row_number()),
         letter = case_when(letter == "1" ~ "A",
                            letter == "2" ~ "B",
                            letter == "3" ~ "C",
                            letter == "4" ~ "D")) %>% 
  ungroup() %>% 
  mutate(category = as.character(category))

data_pdp <- left_join(data_pdp, data_letters) %>% 
  mutate(text_title = glue("**{letter}.**<span style='color:{color}'> {category}</span>"))

### 4.2.2 Create the function ----

plot_pdp_territory <- function(territory_i){
  
  data_pdp_i <- data_pdp %>% 
    filter(territory == territory_i)
  
  plot_list <- map(unique(data_pdp_i$category), ~plot_pdp_category(category_i = ., data_pdp = data_pdp_i))
  
  if(length(unique(data_pdp_i$category)) == 4){
    
    plot_i <- wrap_plots(plot_list[[2]], plot_list[[1]], plot_list[[3]], plot_list[[4]], ncol = 1)
    
    ggsave(filename = paste0("figs/02_part-2/fig-8/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 12, width = 4, dpi = 600)
    
  }else if(length(unique(data_pdp_i$category)) == 3){
    
    plot_i <- wrap_plots(plot_list[[2]], plot_list[[1]], plot_list[[3]], ncol = 1)
    
    ggsave(filename = paste0("figs/02_part-2/fig-8/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 9, width = 4, dpi = 600)
    
  }else if(length(unique(data_pdp_i$category)) == 2){
    
    plot_i <- wrap_plots(plot_list[[1]], plot_list[[2]], ncol = 1)
    
    ggsave(filename = paste0("figs/02_part-2/fig-8/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 4, dpi = 600)
    
  }else if(length(unique(data_pdp_i$category)) == 1){
    
    plot_i <- wrap_plots(plot_list[[1]], ncol = 1)
    
    ggsave(filename = paste0("figs/02_part-2/fig-8/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 3, width = 4, dpi = 600)
    
  }
  
}

### 4.2.3 Map over the function ----

map(unique(data_pdp$territory), ~plot_pdp_territory(territory_i = .))

rm(data_letters, data_pdp, plot_i, plot_list, plot_pdp_category, plot_pdp_territory)

# 5. Model performance ----

## 5.1 Model metrics ----

testing_training <- data_results$result_pred_obs %>% 
  filter(iteration == 1) %>% 
  group_by(category) %>% 
  count() %>% 
  rename(testing = n) %>% 
  mutate(training = testing*3)

## 5.2 Model hyper parameters ----

hyperparams <- data_results$model_hyperparams %>% 
  select(-iteration) %>% 
  distinct()

## 5.3 Model performance ----

model_perf <- data_results$model_performance %>% 
  select(-iteration) %>% 
  group_by(category) %>% 
  summarise(rmse = mean(rmse),
            rsq = mean(rsq)) %>% 
  ungroup()

## 5.4 Combine and export ----

model_metrics <- left_join(testing_training, hyperparams) %>% 
  left_join(., model_perf) %>% 
  relocate(training, .before = testing) %>% 
  openxlsx::write.xlsx(., file = "figs/03_methods/table-2.xlsx")

rm(testing_training, hyperparams, model_perf, model_metrics)

# 6. Distribution of residuals ----

## 6.1 Create the function ----

distri_residuals <- function(category_i, territory = TRUE){
  
  data_i <- data_results$result_pred_obs %>% 
    mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                             category == "Coralline algae" ~ palette_5cols[3],
                             category == "Macroalgae" ~ palette_5cols[4],
                             category == "Turf algae" ~ palette_5cols[5]))
  
  if(territory == TRUE){
    
    data_i <- data_i %>% 
      filter(iteration == 1 & category == category_i)
      
    plot_i <- ggplot(data = data_i, aes(x = residual, fill = color)) + 
      geom_histogram(aes(y = after_stat(count / sum(count))*100),
                     alpha = 0.5) +
      geom_vline(xintercept = 0) +
      scale_fill_identity() +
      theme_graph() +
      theme(panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
            strip.background = element_rect(color = NA, fill = NA),
            axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
            strip.text = element_text(face = "bold", color = unique(data_i$color))) +
      facet_wrap(~territory, scales = "free", ncol = 3) +
      labs(x = "Residuals (obs. - pred.)", y = "Percentage of observations") +
      lims(x = c(-100, 100))
    
    ggsave(filename = paste0("figs/05_additional/02_model/01_distri-residuals_",
                             str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
           plot = plot_i, width = 8.5, height = 11.5, dpi = 600)  
    
  }else{
    
    data_i <- data_i %>% 
      filter(iteration == 1)
    
    plot_i <- ggplot(data = data_i, aes(x = residual, fill = color)) + 
      geom_histogram(aes(y = after_stat(count / sum(count))*100),
                     alpha = 0.5) +
      geom_vline(xintercept = 0) +
      scale_fill_identity() +
      theme_graph() +
      theme(panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
            strip.background = element_rect(color = NA, fill = NA),
            axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
            strip.text = element_text(face = "bold")) +
      facet_wrap(~category, scales = "free", ncol = 2) +
      labs(x = "Residuals (obs. - pred.)", y = "Percentage of observations") +
      lims(x = c(-100, 100))
    
    ggsave(filename = "figs/05_additional/02_model/01_distri-residuals_all.png",
           plot = plot_i, width = 8, height = 5, dpi = 600)
    
  }
  
}

## 6.2 Map over the function (one facet per territory) ----

map(unique(data_results$result_pred_obs$category), ~distri_residuals(category_i = ., territory = TRUE))

## 6.3 All territories combined ----

distri_residuals(territory = FALSE)

# 7. Predicted vs observed ----

## 7.1 Create the function ----

pred_obs <- function(category_i, territory = TRUE){
  
  data_i <- data_results$result_pred_obs %>% 
    mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                             category == "Coralline algae" ~ palette_5cols[3],
                             category == "Macroalgae" ~ palette_5cols[4],
                             category == "Turf algae" ~ palette_5cols[5]))
  
  if(territory == TRUE){
    
    data_i <- data_i %>% 
      filter(iteration == 1 & category == category_i)
    
    data_abline <- data_i %>% 
      select(observed, predicted, territory) %>% 
      group_by(territory) %>% 
      nest() %>% 
      mutate(model = map(data, ~lm(predicted ~ observed, data = .)),
             coefficients = map(model, coefficients),
             intercept = map_dbl(coefficients, 1),
             slope = map_dbl(coefficients, 2)) %>% 
      select(territory, intercept, slope)
    
    plot_i <- ggplot(data = data_i, aes(x = observed, y = predicted, color = color)) +
      geom_point(alpha = 0.5) +
      scale_color_identity() +
      geom_abline(slope = 1) +
      geom_abline(data = data_abline, aes(intercept = intercept, slope = slope), col = "red") +
      theme_graph() +
      theme(panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
            strip.background = element_rect(color = NA, fill = NA),
            axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
            strip.text = element_text(face = "bold", color = unique(data_i$color))) +
      facet_wrap(~territory, scales = "free", ncol = 3) +
      labs(x = "Observed", y = "Predicted") +
      lims(x = c(0, 100), y = c(0, 100))
    
    ggsave(filename = paste0("figs/05_additional/02_model/02_pred-vs-obs_",
                             str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
           plot = plot_i, width = 8.5, height = 11.5, dpi = 600)  
  
  }else{
    
    data_i <- data_i %>% 
      filter(iteration == 1)
    
    data_abline <- data_i %>% 
      select(category, observed, predicted) %>% 
      group_by(category) %>% 
      nest() %>% 
      mutate(model = map(data, ~lm(predicted ~ observed, data = .)),
             coefficients = map(model, coefficients),
             intercept = map_dbl(coefficients, 1),
             slope = map_dbl(coefficients, 2)) %>% 
      select(intercept, slope)
    
    plot_i <- ggplot(data = data_i, aes(x = observed, y = predicted, color = color)) +
      geom_point(alpha = 0.5) +
      scale_color_identity() +
      geom_abline(slope = 1) +
      geom_abline(data = data_abline, aes(intercept = intercept, slope = slope), col = "red") +
      theme_graph() +
      theme(panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
            strip.background = element_rect(color = NA, fill = NA),
            axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
            strip.text = element_text(face = "bold")) +
      facet_wrap(~category, scales = "free", ncol = 2) +
      labs(x = "Observed", y = "Predicted") +
      lims(x = c(0, 100), y = c(0, 100))
    
    ggsave(filename = "figs/05_additional/02_model/02_pred-vs-obs_all.png",
           plot = plot_i, width = 8, height = 5, dpi = 600)
  
  }
  
}

## 7.2 Map over the function (one facet per territory) ----

map(unique(data_results$result_pred_obs$category), ~pred_obs(category_i = ., territory = TRUE))

## 7.3 All territories combined ----

pred_obs(territory = FALSE)

# 8. Variable importance ----

## 8.1 Create the function ----

variable_importance <- function(category_i){
  
  data_imp_summary <- data_results$result_vip %>% 
    mutate(importance = importance*100) %>% 
    group_by(predictor, category) %>% 
    summarise(mean_imp = mean(importance),
              sd_imp = sd(importance)) %>% 
    ungroup() %>% 
    group_by(category) %>% 
    mutate(predictor = fct_reorder(predictor, mean_imp)) %>% 
    ungroup() %>% 
    mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                             category == "Coralline algae" ~ palette_5cols[3],
                             category == "Macroalgae" ~ palette_5cols[4],
                             category == "Turf algae" ~ palette_5cols[5])) %>% 
    filter(category == category_i)
  
  data_imp_raw <- left_join(data_results$result_vip %>% 
                              filter(category == category_i) %>% 
                              mutate(importance = importance*100),
                            data_imp_summary) %>% 
    mutate(predictor = fct_reorder(predictor, mean_imp))
  
  plot_i <- ggplot() +
    geom_jitter(data = data_imp_raw, aes(x = predictor, y = importance, col = color),
                alpha = 0.075, width = 0.1) +
    geom_linerange(data = data_imp_summary, aes(x = predictor, 
                                                ymin = mean_imp - sd_imp,
                                                ymax = mean_imp + sd_imp),
                   col = "black", linewidth = 0.7) +
    geom_point(data = data_imp_summary, aes(x = predictor, y = mean_imp, fill = color),
               shape = 21, size = 3.25, col = "black") +
    scale_color_identity() +
    scale_fill_identity() +
    coord_flip() +
    labs(y = "Importance (%)", x = NULL) +
    theme_graph()
  
  ggsave(filename = paste0("figs/04_supp/fig-3_",
                           str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_i, height = 9, width = 8, dpi = 600)
  
}

## 8.2 Map over the function ----

map(unique(data_results$result_vip$category), ~variable_importance(category_i = .))

# 9. Comparison of benthic trends between territories ----

## 9.1 Create the function ----

plot_pdp_category <- function(category_i){
  
  plot_i <- data_results$result_pdp_territory %>% 
    group_by(x, category, territory) %>% 
    summarise(y_pred_min = min(y_pred),
              y_pred_max = max(y_pred),
              y_pred_mean = mean(y_pred)) %>%
    ungroup() %>% 
    mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                             category == "Coralline algae" ~ palette_5cols[3],
                             category == "Macroalgae" ~ palette_5cols[4],
                             category == "Turf algae" ~ palette_5cols[5])) %>% 
    filter(category == category_i) %>% 
    ggplot(data = .) +
    geom_ribbon(aes(ymin = y_pred_min, ymax = y_pred_max, x = x, fill = color), alpha = 0.35) +
    geom_line(aes(x = x, y = y_pred_mean, color = color)) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(~territory) +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
    labs(x = "Year", y = "Cover (%)")
  
  ggsave(filename = paste0("figs/05_additional/03_results/01_pdp_",
                           str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_i, height = 12, width = 20, dpi = 600)

}

## 9.2 Map over the function ----

map(unique(data_results$result_pdp_territory$category), ~plot_pdp_category(category_i = .))

# 10. Comparison of statistical methods ----

## 10.1 Load and transform benthic raw data ----

### 10.1.1 Load gcrmndb_benthos data ----

load("data/04_data-benthic.RData")

### 10.1.2 Transform NCRMP data ----

data_benthic_ncrmp <- data_benthic %>% 
  filter(datasetID %in% c("0011", "0012", "0013", "0014")) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category, subcategory) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
           decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category, subcategory) %>% 
  summarise(measurementValue = mean(measurementValue)) %>% 
  ungroup()

### 10.1.3 Transform data ----

data_benthic <- data_benthic %>% 
  filter(!(datasetID %in% c("0011", "0012", "0013", "0014"))) %>% 
  bind_rows(., data_benthic_ncrmp) %>% 
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Cyanobacteria" ~ "Cyanobacteria",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Coralline algae", "Turf algae")) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat,
           parentEventID, eventID, decimalLatitude, decimalLongitude, verbatimDepth,
           year, month, day, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>%
  filter(measurementValue <= 100) %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                           category == "Coralline algae" ~ palette_5cols[3],
                           category == "Macroalgae" ~ palette_5cols[4],
                           category == "Turf algae" ~ palette_5cols[5]))

## 10.2 Plots ----

### 10.2.1 Raw data points ----

plot_a <- ggplot(data = data_benthic, aes(x = year, y = measurementValue, color = color)) +
  geom_point(alpha = 0.25) +
  scale_color_identity() +
  facet_wrap(~category, ncol = 1) +
  lims(y = c(0, 100), x = c(1980, 2023)) +
  labs(x = "Year", y = "Cover (%)", title = "Raw data")

### 10.2.2 Point range ----

plot_b <- data_benthic %>% 
  group_by(year, category, color) %>% 
  summarise(mean = mean(measurementValue),
            sd_lower = mean(measurementValue) - sd(measurementValue),
            sd_upper = mean(measurementValue) + sd(measurementValue)) %>% 
  ungroup() %>% 
  mutate(sd_lower = if_else(sd_lower < 0, 0, sd_lower)) %>% 
  ggplot(data = .) +
    geom_pointrange(aes(x = year, y = mean, ymin = sd_lower, ymax = sd_upper, color = color)) +
    scale_color_identity() +
    facet_wrap(~category, ncol = 1) +
    lims(y = c(0, 100), x = c(1980, 2023)) +
    labs(x = "Year", y = NULL, title = "Point range")

### 10.2.3 Loess ----

plot_c <- ggplot(data = data_benthic, aes(x = year, y = measurementValue, color = color)) +
  geom_smooth() +
  scale_color_identity() +
  facet_wrap(~category, ncol = 1) +
  lims(y = c(0, 100), x = c(1980, 2023)) +
  labs(x = "Year", y = "Cover (%)", title = "Loess")

### 10.2.4 Partial Dependence Plots (y fixed) ----

data_pdp <- data_results$result_pdp_region %>% 
  group_by(x, category) %>% 
  summarise(y_pred_min = min(y_pred),
            y_pred_max = max(y_pred),
            y_pred_mean = mean(y_pred)) %>%
  ungroup() %>% 
  mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                           category == "Coralline algae" ~ palette_5cols[3],
                           category == "Macroalgae" ~ palette_5cols[4],
                           category == "Turf algae" ~ palette_5cols[5]))

plot_d <- ggplot(data = data_pdp) +
  geom_ribbon(aes(ymin = y_pred_min, ymax = y_pred_max, x = x, fill = color), alpha = 0.35) +
  geom_line(aes(x = x, y = y_pred_mean, color = color)) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
  facet_wrap(~category, ncol = 1) +
  lims(y = c(0, 100), x = c(1980, 2023)) +
  labs(x = "Year", y = NULL, title = "PDP")

### 10.2.5 GCRMN 2020 ----

plot_e <- read.csv("data/ModelledTrends.all.sum_gcrmn-2020.csv") %>% 
  filter(GCRMN_region == "Pacific" & Var == "Hard Coral Cover") %>% 
  mutate(Var = "Hard coral") %>% 
  bind_rows(., tibble(Var = c("Coralline algae", "Macroalgae", "Turf algae"))) %>% 
  ggplot(data = .) +
    geom_line(aes(x = Year, y = value), color = palette_5cols[2]) +
    geom_ribbon(aes(ymin = .lower_0.8, ymax = .upper_0.8, x = Year), fill = palette_5cols[2], alpha = 0.4) +
    geom_ribbon(aes(ymin = .lower_0.95, ymax = .upper_0.95, x = Year), fill = palette_5cols[2], alpha = 0.2) +
    facet_wrap(~Var, ncol = 1, drop = FALSE) +
    labs(x = "Year", y = NULL, title = "GCRMN 2020") +
    lims(y = c(0, 100), x = c(1980, 2023))

### 10.2.6 Combine plots ----

plot_combined <- plot_a + plot_b + plot_c + plot_d + plot_e +
  plot_layout(ncol = 5) & 
  theme(strip.background = element_blank())

### 10.2.7 Save the plot ----

ggsave(filename = "figs/05_additional/03_results/02_raw-pointrange-pdp.png",
       plot = plot_combined, height = 12, width = 25, dpi = 600)
