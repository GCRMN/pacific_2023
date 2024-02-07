# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(glue)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
theme_set(theme_graph())

# 3. Load model results ----

load("data/16_model-data/raw-results_all.RData")

# 4. Temporal trends of benthic cover (territory) ----

## 4.1 Calculate confidence interval ----

data_trends <- results_all_raw$results_region %>% 
  mutate(territory = "Pacific") %>% 
  bind_rows(., results_all_raw$results_territory) %>% 
  rename(cover = mean) %>% 
  group_by(category, territory, year) %>% 
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

save(data_trends, file = "data/16_model-data/final-results.RData")

## 4.2 Add the colors ----

data_trends <- data_trends %>% 
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
                                  glue("**D.**<span style='color:{color}'> {category}</span>")),
         category = as.factor(category),
         category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae", "Turf algae"))

## 4.3 Create the function to make a plot ----

plot_trends <- function(category_i, data_trends_i){
  
  data_trends_j <- data_trends_i %>% 
    filter(category == category_i)
  
  plot_i <- ggplot(data = data_trends_j) +
    geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
    geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
    geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
    labs(x = "Year", y = "Cover (%)", title = unique(data_trends_j$text_title)) +
    theme(plot.title = element_markdown(size = 12))
  
  plot_i
  
}

## 4.4 Create the function to combine the plots ----

combine_plot_trends <- function(territory_i){
  
  data_trends_i <- data_trends %>% 
    filter(territory == territory_i)
  
  plot_list <- map(levels(data_trends_i$category),
                   ~plot_trends(category_i = ., data_trends_i = data_trends_i))
  
  plot_i <- wrap_plots(plot_list, ncol = 1)
  
  if(territory_i == "Pacific"){
    
    ggsave(filename = "figs/01_part-1/fig-12.png", plot = plot_i, height = 12, width = 4, dpi = 600)  
    
  }else{
    
    ggsave(filename = paste0("figs/02_part-2/fig-8/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 12, width = 4, dpi = 600)
    
  }
  
}

## 4.5 Map over the function ----

map(unique(data_trends$territory), ~combine_plot_trends(territory_i = .))

# 5. Temporal trends of benthic cover (category) ----

## 5.1 Create the function ----

plot_trends <- function(category_i){
  
  data_trends_i <- data_trends %>% 
    filter(category == category_i) %>% 
    filter(territory != "Pacific")
  
  plot_i <- ggplot(data = data_trends_i) +
    geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
    geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
    geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(~territory) +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA)) +
    labs(x = "Year", y = "Cover (%)") +
    theme(strip.background = element_blank())
  
  ggsave(filename = paste0("figs/05_additional/03_results/comparison-territories_", 
                           str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
         plot = plot_i, height = 15, width = 20)
  
}

## 5.2 Map over the function ----

map(unique(data_trends$category), ~plot_trends(category_i = .))

# 6. Variable importance ----

## 6.1 Create the function to make the plot ----

plot_vip <- function(category_i){
  
  data_vip <- results_all_raw$result_vip %>% 
    mutate(color = case_when(category == "Hard coral" ~ palette_5cols[2],
                             category == "Coralline algae" ~ palette_5cols[3],
                             category == "Macroalgae" ~ palette_5cols[4],
                             category == "Turf algae" ~ palette_5cols[5])) %>% 
    filter(category == category_i) %>% 
    mutate(importance = importance*100)
  
  data_vip_summary <- data_vip %>% 
    group_by(predictor, color) %>% 
    summarise(mean = mean(importance),
              sd = sd(importance),
              n = length(importance)) %>% 
    ungroup() %>% 
    mutate(t_score_95 = qt(p = 0.05/2, df = n, lower.tail = FALSE),
           lower_ci_95 = mean - (t_score_95*(sd/sqrt(n))),
           upper_ci_95 = mean + (t_score_95*(sd/sqrt(n))),
           t_score_80 = qt(p = 0.2/2, df = n, lower.tail = FALSE),
           lower_ci_80 = mean - (t_score_80*(sd/sqrt(n))),
           upper_ci_80 = mean + (t_score_80*(sd/sqrt(n)))) %>% 
    select(-sd, -n, -t_score_95, -t_score_80) %>% 
    arrange(desc(mean)) %>% 
    filter(row_number() %in% 1:15)
  
  data_vip_raw <- data_vip %>% 
    filter(predictor %in% unique(data_vip_summary$predictor)) %>% 
    left_join(., data_vip_summary) %>% 
    mutate(predictor = fct_reorder(predictor, mean))
  
  plot_i <- ggplot() +
    geom_jitter(data = data_vip_raw, aes(x = predictor, y = importance, col = color),
                alpha = 0.1, width = 0.1) +
    geom_linerange(data = data_vip_summary, aes(x = predictor, 
                                                ymin = mean - lower_ci_95,
                                                ymax = mean + upper_ci_95,
                                                col = color), linewidth = 1.5) +
   geom_linerange(data = data_vip_summary, aes(x = predictor, 
                                               ymin = mean - lower_ci_80,
                                               ymax = mean + upper_ci_80,
                                               col = color), linewidth = 1) +
    geom_point(data = data_vip_summary, aes(x = predictor, y = mean, color = color),
               shape = 21, size = 4, fill = "white") +
    scale_color_identity() +
    scale_fill_identity() +
    coord_flip() +
    labs(y = "Importance (%)", x = NULL) +
    theme_graph()
  
 ggsave(filename = paste0("figs/04_supp/fig-3_",
                          str_replace_all(str_to_lower(category_i), " ", "-"), ".png"),
        plot = plot_i, height = 8, width = 5, dpi = 600)
 
}

## 6.2 Map over the function ----

map(unique(results_all_raw$result_vip$category), ~plot_vip(category_i = .))



















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

plot_d <- data_trends %>% 
  filter(territory == "Pacific") %>% 
  mutate(category = as.character(category)) %>% 
  ggplot(data = .) +
    geom_ribbon(aes(x = year, ymin = lower_ci_95, ymax = upper_ci_95, fill = color), alpha = 0.3) +
    geom_ribbon(aes(x = year, ymin = lower_ci_80, ymax = upper_ci_80, fill = color), alpha = 0.4) +
    geom_line(aes(x = year, y = mean, color = color), linewidth = 1) +
    scale_fill_identity() +
    scale_color_identity() +
    facet_wrap(~category, ncol = 1) +
    lims(y = c(0, 100), x = c(1980, 2023)) +
    labs(x = "Year", y = "Cover (%)", title = "XGBoost")

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
