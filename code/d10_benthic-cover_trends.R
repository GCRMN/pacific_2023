# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)
library(scales)
library(zoo)
library(Kendall)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/combine_model_data.R")
source("code/function/extract_mannkendall.R")
source("code/function/plot_vimp.R")
source("code/function/plot_pdp.R")
source("code/function/plot_trends.R")
source("code/function/plot_residuals.R")
source("code/function/plot_pred_obs.R")
source("code/function/combine_plot_trends.R")
source("code/function/plot_prediction_map.R")

theme_set(theme_graph())

# 3. Data preparation ----

## 3.1 Combine model results ----

model_results <- combine_model_data(model = "xgb")

## 3.2 Confidence intervals ----

raw_trends <- model_results$result_trends %>% 
  # Calculate mean and confidence interval
  rename(cover = mean) %>% 
  group_by(category, region, territory, year, color, text_title) %>% 
  summarise(mean = mean(cover),
            lower_ci_95 = quantile(cover, 0.05),
            lower_ci_80 = quantile(cover, 0.20),
            upper_ci_95 = quantile(cover, 0.95),
            upper_ci_80 = quantile(cover, 0.80)) %>% 
  ungroup()

## 3.3 Long-term average ----

long_term_average <- raw_trends %>% 
  group_by(category, territory) %>% 
  summarise(across(c(mean, lower_ci_80, upper_ci_80), ~mean(.x, na.rm = TRUE))) %>% 
  ungroup()

## 3.4 Long-term trend ----

### 3.4.1 Region ----

data_kendall <- raw_trends %>% 
  filter(territory == "All" & year >= 1990 & year <= 2022) %>% 
  group_by(category) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

raw_trends %>% 
  filter(territory == "All" & year >= 1990 & year <= 2022) %>% 
  left_join(., data_kendall) %>% 
  mutate(title = paste0(category, "\n", trend, "\n (tau = ", round(tau, 3), ", p-value = ", round(p_value, 8), ")")) %>% 
  ggplot(data = ., aes(x = year, y = mean)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~title, scales = "free_y", nrow = 2)

ggsave("figs/04_supp/02_model/long-term-trends.png", width = 15, height = 7, dpi = fig_resolution)

### 3.4.2 Territory ----

long_term_trend <- raw_trends %>% 
  group_by(category, territory) %>% 
  group_modify(~extract_mannkendall(data = .x, var_y = "mean")) %>% 
  ungroup()

## 3.5 Add "obs_data" variable ----

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

raw_trends <- left_join(raw_trends, data_benthic_obs) %>% 
  mutate(data_obs = replace_na(data_obs, 0))

## 3.6 Smooth moving average ----

smoothed_trends <- raw_trends %>% 
  group_by(category, region, territory, color, text_title) %>% 
  # Two years moving average
  mutate(across(c("mean", "lower_ci_95", "lower_ci_80", "upper_ci_95", "upper_ci_80"),
                ~rollmean(.x, k = 2, fill = NA, align = "center"))) %>% 
  ungroup() %>% 
  # Add first year data
  filter(year != 1980) %>% 
  bind_rows(., raw_trends %>% 
              filter(year == 1980)) %>% 
  arrange(category, region, territory, year)

## 3.7 Combine into a list ----

data_trends <- lst(smoothed_trends, raw_trends, long_term_average, long_term_trend)

rm(smoothed_trends, raw_trends, long_term_average,
   long_term_trend, data_benthic, data_benthic_obs)

## 3.8 Export the data ----



# 4. Model evaluation ----

## 4.1 Hyper-parameters ----

model_results$model_description %>% 
  select(-model, -color, -text_title, -grid_size)

## 4.2 Performance ----

### 4.2.1 Region ----

model_results$model_performance %>% 
  group_by(category) %>% 
  summarise(across(c(rmse, rsq), ~mean(.x)))

### 4.2.2 Country and territory ----

model_results$model_pred_obs %>% 
  group_by(category) %>% 
  mutate(residual = yhat - y,
         res = sum((y - yhat)^2),
         tot = sum((y - mean(y))^2),
         rsq_global = 1 - (res/tot),
         rmse_global = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  group_by(category, territory, rsq_global,
           rmse_global) %>% 
  summarise(res = sum((y - yhat)^2),
            tot = sum((y - mean(y))^2),
            rsq = 1 - (res/tot),
            rmse = sqrt(sum(residual^2/n()))) %>% 
  ungroup() %>% 
  select(territory, category, rmse) %>% 
  mutate(rmse = round(rmse, 2)) %>% 
  pivot_wider(names_from = category, values_from = rmse) %>% 
  arrange(territory) %>% 
  select(territory, `Hard coral`, `Coralline algae`, `Macroalgae`, `Turf algae`) %>% 
  write.csv2(., "figs/04_supp/rmse-territories_benthic-categories.csv", row.names = FALSE)

## 4.3 Predicted vs observed ----

plot_pred_obs(all = TRUE)

map(unique(model_results$model_pred_obs$category), ~plot_pred_obs(category_i = .))

## 4.4 Residuals ----

plot_residuals(all = TRUE)

map(unique(model_results$result_pred_obs$category), ~plot_residuals(category_i = .))

## 4.5 VIMP ----

### 4.5.1 Transform data ----

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
  ungroup() %>% 
  arrange(category, -mean)

data_imp_raw <- model_results$result_vip %>% 
  mutate(importance = importance*100,
         predictor = str_remove_all(predictor, "pred_"),
         predictor = str_replace_all(predictor, "ID_X", "ID_")) %>% 
  left_join(data_imp_summary, .) %>% 
  group_by(category)

### 4.5.2 Map over the function ----

map(unique(data_imp_summary$category), ~plot_vimp(category_i = .))

rm(data_imp_raw)

## 4.6 PDP ----

### 4.6.1 Transform data ----

data_pdp <- model_results$result_pdp %>% 
  group_by(category, predictor, x, color, text_title) %>% 
  summarise(mean = mean(y_pred, na.rm = TRUE),
            upper_ci = quantile(y_pred, probs = 0.975),
            lower_ci = quantile(y_pred, probs = 0.025)) %>% 
  ungroup()

### 4.6.2 Map over the function ----

map(unique(data_pdp$category), ~plot_pdp(category_i = .x))

# 5. Temporal trends ----

## 5.1 For major benthic categories ----

### 5.1.1 Combined plots ----

map(unique(data_trends$smoothed_trends$territory),
    ~combine_plot_trends(territory_i = ., categ_type = "categories"))

### 5.1.2 Individual plots ----

#### 5.1.2.1 Hard coral ----

plot_hard_coral <- plot_trends(category_i = "Hard coral",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-13_a.png", plot = plot_hard_coral, height = 4, width = 6, dpi = fig_resolution)  

#### 5.1.2.2 Coralline algae ----

plot_trends(category_i = "Coralline algae",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-13_b.png", height = 4, width = 6, dpi = fig_resolution)  

#### 5.1.2.3 Macroalgae ----

plot_trends(category_i = "Macroalgae",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-13_c.png", height = 4, width = 6, dpi = fig_resolution)  

#### 5.1.2.4 Turf algae ----

plot_trends(category_i = "Turf algae",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-13_d.png", height = 4, width = 6, dpi = fig_resolution)  

## 5.2 For hard coral families ----

### 5.2.1 Combined plots ----

map(unique(data_trends$smoothed_trends$territory),
    ~combine_plot_trends(territory_i = ., categ_type = "families"))

### 5.2.2 Individual plots ----

plot_trends(category_i = "Acroporidae",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-14_a.png", height = 4, width = 6, dpi = fig_resolution)  

plot_trends(category_i = "Pocilloporidae",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-14_b.png", height = 4, width = 6, dpi = fig_resolution)  

plot_trends(category_i = "Poritidae",
            data_trends_i = data_trends$smoothed_trends %>% 
              filter(year >= 1990 & year <= 2022) %>% 
              filter(territory == "All"), max_y = NA) +
  labs(title = NULL) + 
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."),
                     limits = c(NA, NA))

ggsave(filename = "figs/01_part-1/fig-14_c.png", height = 4, width = 6, dpi = fig_resolution)

## 5.3 Raw data (for writing) ----

if(FALSE){
  
  A <- data_trends$smoothed_trends %>%
    filter(year >= 1990 & year <= 2022 & territory == "All" & category == "Coralline algae") %>%
    select("year", "mean", "lower_ci_80", "upper_ci_80")
  
  A <- data_trends$smoothed_trends %>%
    filter(year >= 1990 & year <= 2022 & territory == "All" & category == "Pocilloporidae") %>%
    select("mean", "lower_ci_80", "upper_ci_80") %>% 
    summarise(across(c("mean", "lower_ci_80", "upper_ci_80"), ~mean(.x)))
  
}

## 5.4 Plots for PPT ----

### 5.4.1 ----

data_ppt <- data_trends$smoothed_trends %>% 
  filter(year >= 1990 & year <= 2022) %>% 
  filter(category %in% c("Turf algae", "Coralline algae", "Macroalgae")) %>% 
  filter(territory == "All") %>% 
  mutate(color = case_when(category == "Turf algae" ~ "#89B296",
                           category == "Macroalgae" ~ "#04A1AE",
                           category == "Coralline algae" ~ "#F07C88"))

ggplot(data = data_ppt) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
  geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
  geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1990, NA)) +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~category, scales = "free") +
  labs(x = NULL, y = "Cover (%)") +
  theme(plot.title = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        panel.spacing = unit(6, "lines"))

ggsave(filename = "figs/00_misc/ppt_algae.png", height = 3.5, width = 12, dpi = fig_resolution)

#####

data_ppt <- data_trends$smoothed_trends %>% 
  filter(year >= 1990 & year <= 2022) %>% 
  filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
  filter(territory == "All")

ggplot(data = data_ppt) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
  geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
  geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1990, NA)) +
  scale_y_continuous(limits = c(0, 20)) +
  facet_wrap(~category, scales = "free") +
  labs(x = NULL, y = "Cover (%)") +
  theme(plot.title = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        panel.spacing = unit(6, "lines"))

ggsave(filename = "figs/00_misc/ppt_families.png", height = 3.5, width = 12, dpi = fig_resolution)



### 5.4.2 Hard coral cover diverging trajectories ----

data_ppt <- data_trends$smoothed_trends %>% 
  filter(year >= 1990 & year <= 2022) %>% 
  filter(category == "Hard coral") %>% 
  filter(territory %in% c("Niue", "Palau", "New Caledonia")) %>% 
  mutate(color = case_when(territory == "Palau" ~ "#156082",
                           territory == "New Caledonia" ~ "#A6A6A6",
                           territory == "Niue" ~ "#D64541"),
         territory = as.factor(territory),
         territory = fct_relevel(territory, "Palau", "New Caledonia", "Niue"))

ggplot(data = data_ppt) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
  geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
  geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1990, NA)) +
  facet_wrap(~territory) +
  labs(x = NULL, y = "Cover (%)") +
  theme(plot.title = element_markdown(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16, face = "bold"),
        panel.spacing = unit(2, "lines"))

ggsave(filename = "figs/00_misc/ppt_trajectories.png", height = 4, width = 10, dpi = fig_resolution)

# 6. Figures for the Executive Summary ----

## 6.1 Hard coral cover ----

data_trends$smoothed_trends %>% 
  filter(category == "Hard coral" & year >= 1990 & year <= 2022 & territory == "All") %>% 
  ggplot(data = .) +
  geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = palette_first[2]), alpha = 0.35) +
  #geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
  geom_line(aes(x = year, y = mean, color = palette_first[2]), linewidth = 1) +
  # 1998 bleaching event
  annotate("segment", x = 2000, xend = 2000, y = 27, yend = 29.75, linewidth = 0.45) +
  annotate("point", x = 2000, y = 27, size = 2, shape = 21, fill = "white", color = "black") +
  geom_textbox(data = tibble(x = 2000, y = 31, label = "<b>1998</b><br>bleaching event<br>- 2.4%"),
               aes(x = x, y = y, label = label), hjust = 0.5, fill = "transparent",
               box.colour = "transparent", halign = 0.5, size = 3.5, family = font_choose_graph) +
  # 2014-2017 bleaching event
  annotate("segment", x = 2016.5, xend = 2016.5, y = 27, yend = 29.75, linewidth = 0.45) +
  annotate("point", x = 2016.5, y = 27, size = 2, shape = 21, fill = "white", color = "black") +
  geom_textbox(data = tibble(x = 2016.5, y = 31, label = "<b>2014 - 2017</b><br>bleaching events<br>- 3.7%"),
               aes(x = x, y = y, label = label), hjust = 0.5, fill = "transparent",
               box.colour = "transparent", halign = 0.5, size = 3.5, family = font_choose_graph) +
  scale_fill_identity() +
  scale_color_identity() +
  scale_x_continuous(expand = c(0, 0), limits = c(1990, NA)) +
  scale_y_continuous(limits = c(NA, 33)) +
  labs(title = paste0("Modeled changes in <span style = 'color: ",
                      palette_first[2],
                      "'>hard coral cover</span> in the<br>Pacific between 1990 and 2022"),
       x = "Year", y = "Hard coral cover (%)",
       subtitle = "<br><span style = 'color: #24252a'>The bold line represents the average,
       the ribbon<br>represents the confidence interval of 95%</span>") + 
  theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
        plot.subtitle = element_markdown(size = 12))

ggsave("figs/00_misc/exe-summ_1.png", height = 5.3, width = 7.2, dpi = fig_resolution)

## 6.2 Algae cover ----

data_trends$smoothed_trends %>% 
  filter(category %in% c("Coralline algae", "Macroalgae", "Turf algae") & 
           year >= 1990 & year <= 2022 & territory == "All") %>% 
  mutate(color = case_when(category == "Coralline algae" ~ "#f1828d",
                           category == "Macroalgae" ~ "#16a085",
                           category == "Turf algae" ~ "#91b496")) %>% 
  ggplot(data = .) +
  #geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.35) +
  geom_line(aes(x = year, y = mean, group = category, color = color), linewidth = 1) +
  scale_x_continuous(expand = c(0, 0), limits = c(1990, NA)) +
  scale_y_continuous(limits = c(7, 16)) +
  labs(title = paste0("Modeled changes in 
                      <span style = 'color: ", "#f1828d", "'>coralline algae</span>", ",
                      <span style = 'color: ", "#16a085", "'>macroalgae</span>", ",
                      and <span style = 'color: ", "#91b496", "'><br>turf algae</span> cover", ",
                      in the Pacific between 1990 and 2022"),
       x = "Year", y = "Benthic cover (%)",
       subtitle = "<br><span style = 'color: #24252a'>The bold lines represents the average,<br>
       confidence intervals are not represented") + 
  scale_fill_identity() +
  scale_color_identity() +
  theme(plot.title = element_markdown(size = 17, face = "bold", family = "Open Sans Semibold"),
        plot.subtitle = element_markdown(size = 12)) +
  geom_textbox(data = tibble(x = 2015, y = 9.5, label = "<b>Macroalgae</b>"),
               aes(x = x, y = y, label = label), hjust = 0.5, fill = "transparent", color = "#16a085",
               box.colour = "transparent", halign = 0.5, size = 3.5, family = font_choose_graph) +
  geom_textbox(data = tibble(x = 2003.5, y = 12.5, label = "<b>Coralline algae</b>"),
               aes(x = x, y = y, label = label), hjust = 0.5, fill = "transparent", color = "#f1828d",
               box.colour = "transparent", halign = 0.5, size = 3.5, family = font_choose_graph) +
  geom_textbox(data = tibble(x = 1995, y = 15.5, label = "<b>Turf algae</b>"), color = "#91b496",
               aes(x = x, y = y, label = label), hjust = 0.5, fill = "transparent",
               box.colour = "transparent", halign = 0.5, size = 3.5, family = font_choose_graph)

ggsave("figs/00_misc/exe-summ_2.png", height = 5.3, width = 7.2, dpi = fig_resolution)
