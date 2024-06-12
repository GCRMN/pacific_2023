# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
theme_set(theme_bw())

# 3. Taxonomy ----

## 3.1 Load data ----

load("data/09_misc/data-benthic.RData")

## 3.2 Taxonomic levels ----

### 3.2.1 Create the function ----

taxonomic_levels <- function(data){
  
  data %>% 
    mutate(tax_lvl = case_when(!(is.na(scientificName)) ~ "Species",
                               !(is.na(genus)) ~ "Genus",
                               !(is.na(family)) ~ "Family",
                               !(is.na(order)) ~ "Order",
                               !(is.na(class)) ~ "Class",
                               !(is.na(phylum)) ~ "Phylum",
                               !(is.na(subcategory)) ~ "Subcategory",
                               !(is.na(category)) ~ "Category")) %>% 
    group_by(tax_lvl) %>% 
    summarise(abs = n()) %>% 
    ungroup() %>% 
    mutate(rel = (abs*100)/sum(abs),
           tax_lvl = as_factor(tax_lvl),
           tax_lvl = fct_expand(tax_lvl, "Category", "Subcategory", "Phylum", "Class",
                                "Order", "Family", "Genus", "Species"),
           tax_lvl = fct_relevel(tax_lvl, "Species", "Genus", "Family", "Order", "Class",
                                 "Phylum", "Subcategory", "Category")) %>% 
    ggplot(data = ., aes(x = tax_lvl, y = rel)) +
    geom_bar(stat = "identity", fill = "#446cb3") +
    lims(y = c(0, 100)) +
    scale_x_discrete(drop = FALSE) +
    coord_flip() +
    labs(x = NULL, y = "Percentage of rows")
  
}

### 3.2.2 For the entire dataset ----

data_benthic %>% 
  taxonomic_levels() +
  labs(title = "All data")

ggsave("figs/04_supp/01_data-explo/01_taxonomic-levels_all.png", dpi = 600, height = 4, width = 8)

### 3.2.3 For hards corals ----

data_benthic %>% 
  filter(category == "Hard coral") %>% 
  taxonomic_levels() +
  labs(title = "Hard coral")

ggsave("figs/04_supp/01_data-explo/01_taxonomic-levels_hard-coral.png", dpi = 600, height = 4, width = 8)

### 3.2.4 For algae ----

data_benthic %>% 
  filter(category == "Algae") %>% 
  taxonomic_levels() +
  labs(title = "Algae")

ggsave("figs/04_supp/01_data-explo/01_taxonomic-levels_algae.png", dpi = 600, height = 4, width = 8)

## 3.3 Taxonomic frequency ----

### 3.3.1 Create the function ----

arrange_factor <- function(data){
  
  data %>% 
    mutate(category = str_replace_all(category, c("Algae" = "**Algae (ALL)**",
                                                  "Abiotic" = "**Abiotic (ALL)**",
                                                  "Other fauna" = "**Other fauna (ALL)**",
                                                  "Hard coral" = "**Hard coral**",
                                                  "Seagrass" = "**Seagrass**")),
           category = as_factor(category),
           category = fct_relevel(category, 
                                  "**Abiotic (ALL)**", "Rock", "Rubble", "Sand", "Silt",
                                  "**Algae (ALL)**", "Coralline algae", "Cyanobacteria", "Macroalgae", "Turf algae",
                                  "**Hard coral**", "**Other fauna (ALL)**", "**Seagrass**"),
           cat = case_when(category %in% c("**Abiotic (ALL)**", "Rock", "Rubble", "Sand", "Silt") ~ "Abiotic",
                           category %in% c("**Algae (ALL)**", "Coralline algae",
                                           "Cyanobacteria", "Macroalgae", "Turf algae") ~ "Algae",
                           category == "**Hard coral**" ~ "Hard coral",
                           category == "**Other fauna (ALL)**" ~ "Other fauna",
                           category == "**Seagrass**" ~ "Seagrass"))
  
}

### 3.3.2 Plot for percentage of dataset ----

data_cat_a <- data_benthic %>% 
  group_by(datasetID, category) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot_units = n_distinct(datasetID)) %>% 
  group_by(category, tot_units) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/tot_units) %>% 
  select(-tot_units)

data_cat_b <- data_benthic %>% 
  group_by(datasetID, subcategory) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot_units = n_distinct(datasetID)) %>% 
  group_by(subcategory, tot_units) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/tot_units) %>% 
  select(-tot_units) %>% 
  drop_na(subcategory) %>% 
  rename(category = subcategory)

data_cat_c <- bind_rows(data_cat_a, data_cat_b) %>% 
  arrange_factor()

plot_a <- ggplot(data = data_cat_c, aes(x = category, y = rel, fill = cat)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
  lims(y = c(0, 100)) +
  labs(y = "Datasets (%)", x = NULL) +
  coord_flip() +
  theme(axis.text.y = element_markdown()) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = palette_second)

### 3.3.3 Plot for percentage of territories ----

data_cat_a <- data_benthic %>% 
  select(territory, category) %>% 
  distinct() %>% 
  group_by(territory, category) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot_units = n_distinct(territory)) %>% 
  group_by(category, tot_units) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/tot_units) %>% 
  select(-tot_units)

data_cat_b <- data_benthic %>% 
  select(territory, subcategory) %>% 
  distinct() %>% 
  group_by(territory, subcategory) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot_units = n_distinct(territory)) %>% 
  group_by(subcategory, tot_units) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/tot_units) %>% 
  select(-tot_units) %>% 
  drop_na(subcategory) %>% 
  rename(category = subcategory)

data_cat_c <- bind_rows(data_cat_a, data_cat_b) %>% 
  arrange_factor()

plot_b <- ggplot(data = data_cat_c, aes(x = category, y = rel, fill = cat)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
  lims(y = c(0, 100)) +
  labs(y = "Territories (%)", x = NULL) +
  coord_flip() +
  theme(axis.text.y = element_markdown()) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = palette_second)

### 3.3.4 Plot for percentage of surveys ----

data_cat_a <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, eventDate, category) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, year, eventDate, category) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot_units = nrow(.)) %>% 
  group_by(category, tot_units) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/tot_units) %>% 
  select(-tot_units)

data_cat_b <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, eventDate, subcategory) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, year, eventDate, subcategory) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(tot_units = nrow(.)) %>% 
  group_by(subcategory, tot_units) %>% 
  summarise(abs = n()) %>% 
  ungroup() %>% 
  mutate(rel = (abs*100)/tot_units) %>% 
  select(-tot_units) %>% 
  drop_na(subcategory) %>% 
  rename(category = subcategory)

data_cat_c <- bind_rows(data_cat_a, data_cat_b) %>% 
  arrange_factor()

plot_c <- ggplot(data = data_cat_c, aes(x = category, y = rel, fill = cat)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8) +
  lims(y = c(0, 100)) +
  labs(y = "Surveys (%)", x = NULL) +
  coord_flip() +
  theme(axis.text.y = element_markdown()) +
  scale_x_discrete(limits = rev) +
  scale_fill_manual(values = palette_second)

### 3.3.5 Combine plots ----

plot_a + plot_b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank()) +
  plot_c + theme(axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())

### 3.3.6 Export the plot ----

ggsave("figs/04_supp/01_data-explo/01_taxonomic-frequency.png", dpi = 600, height = 6, width = 12)

# 4. Variable y (measurementValue) ----

## 4.1 Load data ----

load("data/11_model-data/data_benthic_prepared.RData")

## 4.2 measurementValue density and temporal ----

### 4.2.1 Create the function ----

plot_distribution <- function(data, i, type){
  
  data <- data %>% 
    mutate(category = as.factor(category),
           color = case_when(category == "Hard coral" ~ palette_second[2],
                             category == "Coralline algae" ~ palette_second[3],
                             category == "Macroalgae" ~ palette_second[4],
                             category == "Turf algae" ~ palette_second[5]))
  
  if(type == "dataset"){
    
    data_benthic_i <- data %>% 
      filter(datasetID == i)
    
    plot_a <- ggplot(data = data_benthic_i, aes(x = year, y = measurementValue, col = color)) +
      geom_point(alpha = 0.1, col = "grey") +
      geom_smooth(show.legend = FALSE) +
      scale_color_identity() +
      lims(y = c(0, 100), x = c(1980, 2023)) +
      labs(y = "Cover (%)", x = "Year") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_b <- ggplot(data = data_benthic_i, aes(x = measurementValue, fill = color)) +
      geom_density(alpha = 0.75, show.legend = FALSE) +
      scale_fill_identity() +
      lims(x = c(0, 100)) +
      labs(x = "Cover (%)", y = "Density") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_all <- plot_a + plot_b + 
      plot_annotation(title = paste0("datasetID = ", i),
                      theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_layout(ncol = 1)
    
    ggsave(paste0("figs/04_supp/01_data-explo/02_distribution-dataset_", i, ".png"),
           dpi = 600, height = 6, width = 10)
    
  }else if(type == "territory"){
    
    data_benthic_i <- data %>% 
      filter(territory == i)
    
    plot_a <- ggplot(data = data_benthic_i, aes(x = year, y = measurementValue, col = color)) +
      geom_point(alpha = 0.1, col = "grey") +
      scale_color_identity() +
      geom_smooth(show.legend = FALSE) +
      lims(y = c(0, 100), x = c(1980, 2023)) +
      labs(y = "Cover (%)", x = "Year") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_b <- ggplot(data = data_benthic_i, aes(x = measurementValue, fill = color)) +
      geom_density(alpha = 0.75, show.legend = FALSE) +
      scale_fill_identity() +
      lims(x = c(0, 100)) +
      labs(x = "Cover (%)", y = "Density") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_all <- plot_a + plot_b + 
      plot_annotation(title = paste0("territory = ", i),
                      theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_layout(ncol = 1)
    
    ggsave(paste0("figs/04_supp/01_data-explo/02_distribution-territory_",
                  str_replace_all(str_to_lower(i), " ", "-"), ".png"), 
           dpi = 600, height = 6, width = 10)
    
  }else if(type == "all"){

    plot_a <- ggplot(data = data, aes(x = year, y = measurementValue, col = color)) +
      geom_point(alpha = 0.1, col = "grey") +
      scale_color_identity() +
      geom_smooth(show.legend = FALSE) +
      lims(y = c(0, 100), x = c(1980, 2023)) +
      labs(y = "Cover (%)", x = "Year") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_b <- ggplot(data = data, aes(x = measurementValue, fill = color)) +
      geom_density(alpha = 0.75, show.legend = FALSE) +
      scale_fill_identity() +
      lims(x = c(0, 100)) +
      labs(x = "Cover (%)", y = "Density") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_all <- plot_a + plot_b + 
      plot_annotation(title = "All data",
                      theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_layout(ncol = 1)
    
    ggsave(paste0("figs/04_supp/01_data-explo/02_distribution-all.png"), 
           dpi = 600, height = 6, width = 10)
    
  }else{
    
    stop("The argument type must be dataset or territory")
    
  }
  
}

### 4.2.2 For all data ----

plot_distribution(data = data_benthic, type = "all")

### 4.2.3 Map over the function for territories ----

map(unique(data_benthic$territory), 
    ~plot_distribution(data = data_benthic, i = ., type = "territory"))

### 4.2.4 Map over the function for datasetID ----

map(unique(data_benthic$datasetID), 
    ~plot_distribution(data = data_benthic, i = ., type = "dataset"))

## 4.3 Number of 0 values ----

### 4.3.1 Per datasetID ----

data_nb_0_dataset <- data_benthic %>% 
  select(datasetID, measurementValue) %>% 
  group_by(datasetID) %>% 
  summarise(nb_0 = sum(measurementValue == 0))

### 4.3.2 Per territory ----

data_nb_0_territory <- data_benthic %>% 
  select(territory, measurementValue) %>% 
  group_by(territory) %>% 
  summarise(nb_0 = sum(measurementValue == 0))

# 5. Predictors ----

## 5.1 Percentage of NA per predictor ----

load("data/11_model-data/data_predictors_pred.RData")

data_pred_na <- data_predictors_pred %>% 
  select(-datasetID, -month, -verbatimDepth, -parentEventID, -eventID) %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_pred),
         percent = (na*100)/n)

data_obs_na <- data_benthic %>% 
  summarise(across(1:ncol(.), ~sum(is.na(.x)))) %>% 
  pivot_longer(1:ncol(.), names_to = "predictor", values_to = "na") %>% 
  mutate(n = nrow(data_predictors_pred),
         percent = (na*100)/n)

## 5.2 Distribution ----

### 5.2.1 Create the function ----

plot_distri <- function(variable){
  
    result <- ggplot(data = data_predictors_pred, aes(x = get(variable, data_predictors_pred))) +
      geom_density(fill = palette_first[2], alpha = 0.5) +
      labs(x = variable)
    
    return(result)
  
}

### 5.2.2 Map over the function ----

data_pred_distri <- map(data_predictors_pred %>% 
                          select(-territory, -datasetID, -month, -verbatimDepth, -parentEventID, -eventID) %>% 
                          colnames(.),
         ~plot_distri(variable = .))

### 5.2.3 Combine plots ----

data_pred_distri <- wrap_plots(data_pred_distri, ncol = 5)

### 5.2.4 Export the plots ----

ggsave(plot = data_pred_distri,
       filename = "figs/04_supp/01_data-explo/03_predictors-distribution.png",
       dpi = 600, height = 12, width = 15)

## 5.3 Correlation between predictors ----

data_correlation <- round(cor(data_predictors_pred %>% 
                                select(-territory, -datasetID, -month, -day,
                                       -verbatimDepth, -parentEventID, -eventID),
                                use = "complete.obs"), 2) %>% # "complete.obs" to not use NA
  as_tibble() %>% 
  mutate(predictor_a = colnames(.)) %>% 
  pivot_longer(1:ncol(.)-1, names_to = "predictor_b", values_to = "coefficient") %>% 
  filter(predictor_a != predictor_b) %>% 
  arrange(coefficient)
