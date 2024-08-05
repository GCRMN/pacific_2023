# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(scales)
library(zoo)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

# 3. Load and combine model results ----

## 3.1 Load data ----

load("data/12_model-output/model_tuning.RData")

load("data/12_model-output/model_results_hard-coral.RData")
model_results_coral <- model_results

load("data/12_model-output/model_results_macroalgae.RData")
model_results_macroalgae <- model_results

load("data/12_model-output/model_results_turf-algae.RData")
model_results_turf <- model_results

load("data/12_model-output/model_results_coralline-algae.RData")
model_results_cca <- model_results

load("data/12_model-output/model_results_acroporidae.RData")
model_results_acro <- model_results

load("data/12_model-output/model_results_pocilloporidae.RData")
model_results_poci <- model_results

load("data/12_model-output/model_results_poritidae.RData")
model_results_pori <- model_results

## 3.2 Combine data ----

model_results <- lst(model_results_coral, model_results_macroalgae,
                     model_results_turf, model_results_cca, model_results_acro,
                     model_results_poci, model_results_pori) %>% 
  map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
  map(., bind_rows)

## 3.3 Export combined data ----

save(model_results, file = "data/12_model-output/model_results_all.RData")

## 3.4 Create the function to add the color ----

add_colors <- function(data){
  
  data <- data %>% 
    mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                             category == "Coralline algae" ~ palette_second[3],
                             category == "Macroalgae" ~ palette_second[4],
                             category == "Turf algae" ~ palette_second[5],
                             category == "Acroporidae" ~ palette_second[3],
                             category == "Pocilloporidae" ~ palette_second[4],
                             category == "Poritidae" ~ palette_second[5]),
           text_title = case_when(category == "Hard coral" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Coralline algae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Macroalgae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>"),
                                  category == "Turf algae" ~ 
                                    glue("**D.**<span style='color:{color}'> {category}</span>"),
                                  category == "Acroporidae" ~ 
                                    glue("**A.**<span style='color:{color}'> {category}</span>"),
                                  category == "Pocilloporidae" ~ 
                                    glue("**B.**<span style='color:{color}'> {category}</span>"),
                                  category == "Poritidae" ~ 
                                    glue("**C.**<span style='color:{color}'> {category}</span>")))
  
  return(data)
  
}

## 3.5 Add colors ----

model_results <- model_results %>% 
  map(., ~ .x %>% add_colors)

tuning_results <- tuning_results %>% 
  map(., ~ .x %>% add_colors)

## 3.6 Remove useless objects ----

rm(model_results_coral, model_results_macroalgae,
   model_results_turf, model_results_cca, model_results_acro,
   model_results_poci, model_results_pori, add_colors)

# 4. Benthic cover trends ----

## 4.1 Transform data ----

### 4.1.1 Calculate confidence intervals ----

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

#### 4.1.2 Long-term average ----

data_trends %>% 
  filter(territory == "All") %>% 
  group_by(category) %>% 
  summarise(across(c(mean, lower_ci_80, upper_ci_80), ~mean(.x, na.rm = TRUE)))

#### 4.1.3 Long-term trend ----

extract_mannkendall <- function(data, var_y){
  
  require(Kendall)
  
  data_vector <- data %>% 
    pull(var_y)
  
  model <- MannKendall(data_vector)
  
  results <- tibble(tau = model$tau,
                    p_value = model$sl) %>% 
    mutate(trend = case_when(p_value > 0.05 ~ "No trend",
                             p_value <= 0.05 & tau < 0 ~ "Negative trend",
                             p_value <= 0.05 & tau > 0 ~ "Positive trend"))
  
  return(results)
  
}

data_kendall <- data_trends %>% 
  filter(territory == "All") %>% 
  group_by(category) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

data_trends %>% 
  filter(territory == "All") %>% 
  left_join(., data_kendall) %>% 
  mutate(title = paste0(category, "\n", trend, "\n (tau = ", round(tau, 3), ", p-value = ", round(p_value, 8), ")")) %>% 
  ggplot(data = ., aes(x = year, y = mean)) +
    geom_point() +
    geom_smooth(method = "lm") +
    facet_wrap(~title, scales = "free_y", nrow = 2)

ggsave("figs/04_supp/02_model/long-term-trends.png", width = 15, height = 7, dpi = fig_resolution)

### 4.1.2 Smooth trends using a two-years moving average ----

data_trends <- data_trends %>% 
  group_by(category, region, territory, color, text_title) %>% 
  mutate(across(c("mean", "lower_ci_95", "lower_ci_80", "upper_ci_95", "upper_ci_80"),
                ~rollmean(.x, k = 2, fill = NA, align = "right"))) %>% 
  ungroup() %>% 
  # Add first year data
  filter(year != 1980) %>% 
  bind_rows(., data_trends %>% 
              filter(year == 1980)) %>% 
  arrange(category, region, territory, year)

### 4.1.3 Add "data" variable ----

load("data/11_model-data/data_benthic_prepared.RData")

data_benthic_obs <- data_benthic %>% 
  select(year, territory, category) %>% 
  distinct() %>% 
  mutate(region = "Pacific",
         data_obs = 1)

data_benthic_obs <- data_benthic %>% 
  select(year, category) %>% 
  distinct() %>% 
  mutate(region = "Pacific",
         territory = "All",
         data_obs = 1) %>% 
  bind_rows(., data_benthic_obs)

data_trends <- left_join(data_trends, data_benthic_obs) %>% 
  mutate(data_obs = replace_na(data_obs, 0))

rm(data_benthic, data_benthic_obs)

### 4.1.4 Export the data ----

data_trends %>% 
  select(-color, -text_title) %>% 
  write.csv(., "data/12_model-output/model_results_trends.csv", row.names = FALSE)
  
## 4.2 Plot the trends ----

### 4.2.1 Create the function to make a plot ----

plot_trends <- function(category_i, data_trends_i, show_obs_data = "none"){
  
  data_trends_j <- data_trends_i %>% 
    filter(category == category_i)
  
  if(show_obs_data == "none"){
    
    plot_j <- ggplot(data = data_trends_j) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
      geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
      labs(x = NULL, y = "Cover (%)", title = unique(data_trends_j$text_title)) +
      theme(plot.title = element_markdown())
    
  }else if(show_obs_data == "rug"){
    
    plot_j <- ggplot(data = data_trends_j) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
      geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
      scale_fill_identity() +
      scale_color_identity() +
      scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
      labs(x = NULL, y = "Cover (%)", title = unique(data_trends_j$text_title)) +
      theme(plot.title = element_markdown()) +
      geom_rug(data = data_trends_j %>% filter(data_obs != 0), aes(x = year), sides = "b")
    
  }else if(show_obs_data == "area"){
    
    cm <- data_trends_j %>%
      mutate(rleid = with(rle(data_obs), rep(seq_along(lengths), lengths)),
             group = as.integer(rleid))
    
    cm1 <- cm %>% 
      ungroup %>% 
      mutate(d = 3) %>%
      uncount(d, .id = "A") %>%
      mutate_at(vars(year, mean, lower_ci_95, lower_ci_80, upper_ci_95, upper_ci_80),
                function(x=.) ifelse(.$A == 1,(x + lag(x))/2,
                                     ifelse(.$A == 3, (x + lead(x))/2, x))) %>%
      group_by_at(group_vars(cm)) %>%
      filter(row_number()!= 1, row_number() !=n()) %>% 
      ungroup() %>% 
      select(-A, -rleid)
    
    plot_j <- ggplot(data = cm1) +
      geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = as.factor(data_obs), group = group), 
                  alpha = 0.25, show.legend = FALSE) +
      geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = as.factor(data_obs), group = group), 
                  alpha = 0.5, show.legend = FALSE) +
      geom_line(aes(x = year, y = mean, color = as.factor(data_obs), group = group), 
                linewidth = 1, show.legend = FALSE) +
      scale_fill_manual(breaks = c("0", "1"), values = c("grey", unique(data_trends_j$color))) +
      scale_color_manual(breaks = c("0", "1"), values = c("grey", unique(data_trends_j$color))) +
      scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
      labs(x = NULL, y = "Cover (%)", title = unique(data_trends_j$text_title)) +
      theme(plot.title = element_markdown())
    
  }else{
    
    stop("show_obs_data argument can only take 'none', 'rug', or 'area'")
    
  }
  
  return(plot_j)
  
}

### 4.2.2 Create the function to combine the plots ----

combine_plot_trends <- function(territory_i, categ_type){
  
  if(categ_type == "categories"){
    
    data_trends_i <- data_trends %>% 
      filter(territory == territory_i) %>% 
      filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
             category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae", "Turf algae"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "area"))
    
    plot_i <- wrap_plots(plot_list, ncol = 1)
    
    if(territory_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-13.png", plot = plot_i, height = 12, width = 5, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
             plot = plot_i, height = 12, width = 5, dpi = fig_resolution)
      
    }
    
  }else if(categ_type == "families"){
    
    data_trends_i <- data_trends %>% 
      filter(territory == territory_i) %>% 
      filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Acroporidae", "Pocilloporidae", "Poritidae"),
             category = fct_relevel(category, "Acroporidae", "Pocilloporidae", "Poritidae"))
    
    plot_list <- map(levels(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "area"))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(territory_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-14.png", plot = plot_i, height = 4, width = 12, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-7/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
             plot = plot_i, height = 4, width = 12, dpi = fig_resolution)
      
    }
    
  }else{
    
    stop("categ_type argument can only take 'categories' or 'families'")
    
  }
}

### 4.2.3 Map over the function ----

map(unique(data_trends$territory), ~combine_plot_trends(territory_i = ., categ_type = "categories"))

map(unique(data_trends$territory), ~combine_plot_trends(territory_i = ., categ_type = "families"))

## 4.3 Remove useless objects ----

rm(data_trends, plot_trends, combine_plot_trends)

# 5. Variable importance ----

## 5.1 Transform data ----

data_imp_summary <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
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
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  left_join(data_imp_summary, .) %>% 
  group_by(category)

## 5.2 Create the function to make the plot ----

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
    labs(x = NULL, y = "Relative importance (%)") +
    lims(y = c(0, NA))
  
  ggsave(filename = paste0("figs/04_supp/02_model/04_vimp_", str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_i, height = 8, width = 5, dpi = fig_resolution)
  
}

## 5.3 Map over the function ----

map(unique(data_imp_summary$category), ~plot_vimp(category_i = .))

## 5.4 Remove useless objects ----

rm(data_imp_raw, data_imp_summary, plot_vimp)

# 6. Model characteristics ----

## 6.1 Training and testing ----

### 6.1.1 Transform the data ----

data_traintest <- tuning_results[["model_hyperparams"]] %>% 
  select(category, nb_training, nb_testing) %>% 
  mutate(nb_training = format(nb_training, big.mark = ","),
         nb_testing = format(nb_testing, big.mark = ","))

### 6.1.2 Export the table in .tex format ----

writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|C{5cm}|C{5cm}|}",
             "\\hline",
             "\\rowcolor{firstcolor}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{}} & \\textcolor{white}{Training} & \\textcolor{white}{Testing} \\\\ \\hline",
             "\\rowcolor{secondcolor}",
             paste0("\\multicolumn{2}{|l}{Benthic categories} & & \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[1, "category"], " &", data_traintest[1, "nb_training"], "&",
                    data_traintest[1, "nb_testing"], " \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[2, "category"], " &", data_traintest[2, "nb_training"], "&",
                    data_traintest[2, "nb_testing"], " \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[3, "category"], " &", data_traintest[3, "nb_training"], "&",
                    data_traintest[3, "nb_testing"], " \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[4, "category"], " &", data_traintest[4, "nb_training"], "&",
                    data_traintest[4, "nb_testing"], " \\\\ \\hline"),
             "\\rowcolor{secondcolor}",
             paste0("\\multicolumn{2}{|l}{Hard coral families} & & \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[5, "category"], " &", data_traintest[5, "nb_training"], "&",
                    data_traintest[5, "nb_testing"], " \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[6, "category"], " &", data_traintest[6, "nb_training"], "&",
                    data_traintest[6, "nb_testing"], " \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_traintest[7, "category"], " &", data_traintest[7, "nb_training"], "&",
                    data_traintest[7, "nb_testing"], " \\\\ \\hline"),
             "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/03_methods/table-3.tex"))

## 6.2 Hyper-parameters ----

### 6.2.1 Transform the data ----

data_hyper <- tuning_results[["model_hyperparams"]] %>% 
  select(category, learn_rate, trees, tree_depth, min_n) %>% 
  mutate(learn_rate = format(round(learn_rate, 5), scientific = FALSE),
         trees = format(trees, big.mark = ","))

### 6.2.2 Export the table in .tex format ----
  
writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|C{2.5cm}|C{2.5cm}|C{2.5cm}|C{2.5cm}|}",
             "\\hline",
             "\\rowcolor{firstcolor}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{}} & \\textcolor{white}{Learning rate} & \\textcolor{white}{Nb trees} & \\textcolor{white}{Tree depth}  & \\textcolor{white}{Min. obs.} \\\\ \\hline",
             "\\rowcolor{secondcolor}",
             paste0("\\multicolumn{2}{|l}{Benthic categories} & & & & \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[1, "category"], " &", data_hyper[1, "learn_rate"], "&",
                    data_hyper[1, "trees"], "&", data_hyper[1, "tree_depth"], "&", data_hyper[1, "min_n"]," \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[2, "category"], " &", data_hyper[2, "learn_rate"], "&",
                    data_hyper[2, "trees"], "&", data_hyper[2, "tree_depth"], "&", data_hyper[2, "min_n"]," \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[3, "category"], " &", data_hyper[3, "learn_rate"], "&",
                    data_hyper[3, "trees"], "&", data_hyper[3, "tree_depth"], "&", data_hyper[3, "min_n"]," \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[4, "category"], " &", data_hyper[4, "learn_rate"], "&",
                    data_hyper[4, "trees"], "&", data_hyper[4, "tree_depth"], "&", data_hyper[4, "min_n"]," \\\\ \\hline"),
             "\\rowcolor{secondcolor}",
             paste0("\\multicolumn{2}{|l}{Hard coral families} & & & & \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[5, "category"], " &", data_hyper[5, "learn_rate"], "&",
                    data_hyper[5, "trees"], "&", data_hyper[5, "tree_depth"], "&", data_hyper[5, "min_n"]," \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[6, "category"], " &", data_hyper[6, "learn_rate"], "&",
                    data_hyper[6, "trees"], "&", data_hyper[6, "tree_depth"], "&", data_hyper[6, "min_n"]," \\\\ \\hline"),
             "\\rowcolor{white}",
             paste0("\\multicolumn{1}{|l}{} & ", data_hyper[7, "category"], " &", data_hyper[7, "learn_rate"], "&",
                    data_hyper[7, "trees"], "&", data_hyper[7, "tree_depth"], "&", data_hyper[7, "min_n"]," \\\\ \\hline"),
             "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/03_methods/table-4.tex"))

## 6.3 Remove useless objects ----

rm(data_hyper, data_traintest)

# 7. RMSE and R2 ----

## 7.1 Transform data ----

data_perf <- tuning_results$result_pred_obs %>% 
  group_by(category, text_title, color) %>% 
  mutate(residual = yhat - y,
         res = sum((y - yhat)^2),
         tot = sum((y - mean(y))^2),
         rsq_global = 1 - (res/tot),
         rmse_global = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  group_by(category, territory, rsq_global,
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

## 7.2  Export the table in .tex format ----


## 7.3 Make the plots ----

### 7.3.1 RMSE ----

ggplot(data = data_perf, aes(x = reorder(territory, desc(territory)), color = color, y = rmse)) +
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

ggsave("figs/04_supp/02_model/01_perf_rmse.png", width = 14, height = 8, dpi = fig_resolution)

### 7.3.2 R2 ----

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

ggsave("figs/04_supp/02_model/01_perf_rsq.png", width = 14, height = 8, dpi = fig_resolution)

### 7.4 Remove useless objects ----

rm(data_perf, data_perf_mean_all, data_perf_mean_ter)

# 8. Predicted vs observed ----

## 8.1 Create the function to make the plot ----

plot_pred_obs <- function(category_i, all = FALSE){
  
    if(all == TRUE){
      
      plot_i <- tuning_results$result_pred_obs %>%
        filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
        ggplot(data = ., aes(x = y, y = yhat, color= color)) +
        geom_point(alpha = 0.1) +
        geom_abline(slope = 1, linewidth = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
        scale_color_identity() +
        facet_wrap(~text_title, scales = "free") +
        labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
        theme(strip.text = element_markdown(hjust = 0),
              strip.background = element_blank())
      
      ggsave(plot_i, filename = "figs/04_supp/02_model/02_pred-vs-obs_all_cat.png",
             dpi = fig_resolution, height = 6, width = 8)
      
      plot_i <- tuning_results$result_pred_obs %>%
        filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
        ggplot(data = ., aes(x = y, y = yhat, color= color)) +
        geom_point(alpha = 0.1) +
        geom_abline(slope = 1, linewidth = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
        scale_color_identity() +
        facet_wrap(~text_title, scales = "free") +
        labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
        theme(strip.text = element_markdown(hjust = 0),
              strip.background = element_blank())
      
      ggsave(plot_i, filename = "figs/04_supp/02_model/02_pred-vs-obs_all_fam.png",
             dpi = fig_resolution, height = 4, width = 10)
      
    }else{
      
      data_i <- tuning_results$result_pred_obs %>%
        filter(category == category_i)
      
      plot_i <- ggplot(data = data_i, aes(x = y, y = yhat, color= color)) +
        geom_point(alpha = 0.2) +
        geom_abline(slope = 1, linewidth = 0.5) +
        geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", linewidth = 0.5) +
        scale_color_identity() +
        facet_wrap(~territory, scales = "free", ncol = 5) +
        labs(x = "Observed value (y)", y = "Predicted value (ŷ)") +
        theme(strip.text = element_markdown(hjust = 0.5),
              strip.background = element_blank())
      
      ggsave(plot_i, filename = paste0("figs/04_supp/02_model/02_pred-vs-obs_", 
                                       str_replace_all(str_to_lower(category_i), " ", "-"),
                                       ".png"),
             width = 15, height = 12, dpi = fig_resolution)
      
    }
}

## 8.2 Map over the function ----

plot_pred_obs(all = TRUE)

map(unique(tuning_results$result_pred_obs$category), ~plot_pred_obs(category_i = .))

## 8.3 Remove useless objects ----

rm(plot_pred_obs)

# 9. Distribution of residuals ----

## 9.1 Create the function to make the plot ----

plot_residuals <- function(category_i, all = FALSE){
  
  if(all == TRUE){
    
    plot_i <- tuning_results$result_pred_obs %>%
      filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
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
    
    ggsave(plot_i, filename = "figs/04_supp/02_model/03_distri-residuals_all_cat.png",
           dpi = fig_resolution, height = 6, width = 8)
    
    plot_i <- tuning_results$result_pred_obs %>%
      filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
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
    
    ggsave(plot_i, filename = "figs/04_supp/02_model/03_distri-residuals_all_fam.png",
           dpi = fig_resolution, height = 4, width = 10)
    
  }else{
    
    data_i <- tuning_results$result_pred_obs %>%
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
           width = 15, height = 12, dpi = fig_resolution)
    
  }
  
}

## 9.2 Map over the function ----

plot_residuals(all = TRUE)

map(unique(tuning_results$result_pred_obs$category), ~plot_residuals(category_i = .))

## 9.3 Remove useless objects ----

rm(plot_residuals)
