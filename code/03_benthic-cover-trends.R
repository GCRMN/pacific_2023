# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/summarise_cover.R")
theme_set(theme_graph())

# 3. Load data ----

load("data/04_data-benthic.RData")

# 4. Benthic cover trends ----

# 4.1 Create function to plot trends --

plot_trends <- function(category, territory, territory_i){
  
  color <- ifelse(category == "Hard coral", "#d64541",
                  ifelse(category == "Macroalgae", "#16a085",
                         ifelse(category == "Coralline algae", "#825e5c",
                                ifelse(category == "Turf algae", "#91b496", NA))))
  
  if(territory == FALSE){
    
    data_benthic_i <- data_benthic %>% 
      mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                                  subcategory == "Turf algae" ~ "Turf algae",
                                  subcategory == "Coralline algae" ~ "Coralline algae",
                                  TRUE ~ category)) %>% 
      # Filter and summarize data
      summarise_cover(., category_i = category)
  
  # Raw data
  
  plot_a <- ggplot(data_benthic_i, aes(x = year, y = measurementValue)) +
    geom_point(alpha = 0.25, shape = 21, fill = color, color = color) +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "Raw data")
  
  # Pointrange
  
  plot_b <- ggplot(data_benthic_i, aes(x = year, y = measurementValue)) +
    geom_pointrange(stat = "summary",
                    fun.min = function(x) mean(x) - sd(x),
                    fun.max = function(x) mean(x) + sd(x),
                    fun = mean, color = color) +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "Pointrange")
  
  # Loess
  
  plot_c <- ggplot(data_benthic_i, aes(x = year, y = measurementValue)) +
    geom_smooth(color = color) +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "Loess")
  
  # XGBoost (raw)
  
  load(paste0("data/16_model-results/results-model_",
              str_replace(str_to_lower(category), " ", "-"), ".RData"))
  
  plot_d <- ggplot() +
    geom_line(data = data_results$result_pdp_region,
              aes(x = x, y = y_pred, group = iteration), color = color) +
    geom_rug(data = data_benthic_i, aes(x = year, y = measurementValue),
             sides = "t", color = "grey") +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "XGboost")
  
  result <- plot_a + plot_b + plot_c + plot_d + plot_layout(ncol = 4)

}else{
  
  data_benthic_i <- data_benthic %>% 
    filter(territory == territory_i) %>% 
    mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                                subcategory == "Turf algae" ~ "Turf algae",
                                subcategory == "Coralline algae" ~ "Coralline algae",
                                TRUE ~ category)) %>% 
    # Filter and summarize data
    summarise_cover(., category_i = category)
  
  # Raw data
  
  plot_a <- ggplot(data_benthic_i, aes(x = year, y = measurementValue)) +
    geom_point(alpha = 0.25, shape = 21, fill = color, color = color) +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "Raw data")
  
  # Pointrange
  
  plot_b <- ggplot(data_benthic_i, aes(x = year, y = measurementValue)) +
    geom_pointrange(stat = "summary",
                    fun.min = function(x) mean(x) - sd(x),
                    fun.max = function(x) mean(x) + sd(x),
                    fun = mean, color = color) +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "Pointrange")
  
  # Loess
  
  plot_c <- ggplot(data_benthic_i, aes(x = year, y = measurementValue)) +
    geom_smooth(color = color) +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "Loess")
  
  # XGBoost (raw)
  
  load(paste0("data/16_model-results/results-model_",
              str_replace(str_to_lower(category), " ", "-"), ".RData"))
  
  plot_d <- ggplot() +
    geom_line(data = data_results$result_pdp_territory %>% 
                filter(territory == territory_i),
              aes(x = x, y = y_pred, group = iteration), color = color) +
    geom_rug(data = data_benthic_i, aes(x = year, y = measurementValue),
             sides = "t", color = "grey") +
    lims(x = c(1987, 2023), y = c(0, 100)) +
    labs(x = "Year", y = paste0(category, " cover (%)"), title = "XGboost")
  
  result <- plot_a + plot_b + plot_c + plot_d + plot_layout(ncol = 4)
  
}
  
  return(result)

}

# 4.2 Create function to combine plots --

combine_plot_trends <- function(territory, all){
  
  if(all == TRUE){
    
    plots_hc <- plot_trends(category = "Hard coral", territory = TRUE, territory_i = territory) 
    plots_ma <- plot_trends(category = "Macroalgae", territory = TRUE, territory_i = territory)
    plots_ca <- plot_trends(category = "Coralline algae", territory = TRUE, territory_i = territory)
    
    plots_hc[[1]] + plots_hc[[2]] + plots_hc[[3]] + plots_hc[[4]] + 
      plots_ma[[1]] + plots_ma[[2]] + plots_ma[[3]] + plots_ma[[4]] + 
      plots_ca[[1]] + plots_ca[[2]] + plots_ca[[3]] + plots_ca[[4]] + 
      plot_layout(ncol = 4)
    
    ggsave(filename = paste0("figs/territories_fig-5/",
                             str_replace_all(str_to_lower(territory), " ", "-"), ".png"),
           height = 10, width = 14, dpi = 600)
  
  }else{
    
    plots_hc <- plot_trends(category = "Hard coral", territory = TRUE, territory_i = territory)
    plots_ma <- plot_trends(category = "Macroalgae", territory = TRUE, territory_i = territory)
    plots_ca <- plot_trends(category = "Coralline algae", territory = TRUE, territory_i = territory)
    
    (plots_hc[[4]] + labs(title = NULL)) + (plots_ma[[4]] + labs(title = NULL)) + 
      (plots_ca[[4]] + labs(title = NULL)) + plot_layout(ncol = 3)
    
    ggsave(filename = paste0("figs/territories_fig-5b/",
                             str_replace_all(str_to_lower(territory), " ", "-"), ".png"),
           height = 3, width = 10, dpi = 600)
  
  }
  
}

# 4.3 Export trends for territories --

# 4.3.1 All plots (raw data, pointrange, loess, and xgboost) --

map(unique(data_benthic$territory), ~combine_plot_trends(territory = ., all = TRUE))

# 4.3.2 Only xgboost trends --

map(unique(data_benthic$territory), ~combine_plot_trends(territory = ., all = FALSE))

# 4.4 Export trends for the Pacific --

# 4.4.1 All plots (raw data, pointrange, loess, and xgboost) --

plots_hc <- plot_trends(category = "Hard coral", territory = FALSE) 
plots_ma <- plot_trends(category = "Macroalgae", territory = FALSE)
plots_ca <- plot_trends(category = "Coralline algae", territory = FALSE)

plots_combined <- plots_hc[[1]] + plots_hc[[2]] + plots_hc[[3]] + plots_hc[[4]] + 
  plots_ma[[1]] + plots_ma[[2]] + plots_ma[[3]] + plots_ma[[4]] + 
  plots_ca[[1]] + plots_ca[[2]] + plots_ca[[3]] + plots_ca[[4]] + 
  plot_layout(ncol = 4)

ggsave(filename = paste0("figs/benthic_cover_trends_all.png"), plots_combined,
       height = 10, width = 14, dpi = 600)

# 4.4.2 Only xgboost trends --

plots_hc <- plot_trends(category = "Hard coral", territory = FALSE)
plots_ma <- plot_trends(category = "Macroalgae", territory = FALSE)
plots_ca <- plot_trends(category = "Coralline algae", territory = FALSE)

plots_combined <- (plots_hc[[4]] + labs(title = NULL)) + (plots_ma[[4]] + labs(title = NULL)) + 
  (plots_ca[[4]] + labs(title = NULL)) + plot_layout(ncol = 3)

ggsave(filename = paste0("figs/benthic_cover_trends_trends.png"), plots_combined,
       height = 3, width = 10, dpi = 600)

# 5. Variable importance ----

# 5.1 Create a function to plot variable importance --

var_imp <- function(category){
  
  color <- ifelse(category == "Hard coral", "#d64541",
                  ifelse(category == "Macroalgae", "#16a085",
                         ifelse(category == "Coralline algae", "#825e5c",
                                ifelse(category == "Turf algae", "#91b496", NA))))
  
  load(paste0("data/16_model-results/results-model_",
              str_replace(str_to_lower(category), " ", "-"), ".RData"))
  
  # 1. Extract data --
  
  data_imp_raw <- data_results %>% 
    .$result_vip %>% 
    mutate(importance = importance*100)
  
  # 2. Calculate mean and sd of all iteration --
  
  data_imp_summary <- data_imp_raw %>% 
    group_by(predictor) %>% 
    summarise(mean_imp = mean(importance),
              sd_imp = sd(importance)) %>% 
    ungroup() %>% 
    mutate(predictor = fct_reorder(predictor, mean_imp))
  
  # 3. Add mean to order factor --
  
  data_imp_raw <- left_join(data_imp_raw, data_imp_summary) %>% 
    mutate(predictor = fct_reorder(predictor, mean_imp))
  
  # 4. Make the plot --
  
  ggplot() +
    geom_jitter(data = data_imp_raw, aes(x = predictor, y = importance),
                alpha = 0.075, col = "#446CB3", width = 0.1) +
    geom_linerange(data = data_imp_summary, aes(x = predictor, 
                                                ymin = mean_imp - sd_imp,
                                                ymax = mean_imp + sd_imp),
                   col = "black", linewidth = 0.7) +
    geom_point(data = data_imp_summary, aes(x = predictor, y = mean_imp),
               fill = color, shape = 21, size = 3.25, col = "black") +
    coord_flip() +
    labs(y = "Importance (%)", x = NULL)
  
  # 5. Save the plot --
  
  ggsave(filename = paste0("figs/supp_fig-2/",
                           str_replace_all(str_to_lower(category), " ", "-"), ".png"),
         height = 10, width = 8, dpi = 600)
  
}

# 5.2 Map over the function for all benthic groups --

map(c("Hard coral", "Coralline algae", "Macroalgae"), ~var_imp(category = .))
