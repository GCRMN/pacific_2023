# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
theme_set(theme_bw())

# 3. Load gcrmndb_benthos data ----

load("data/04_data-benthic.RData")

# 4. Transform data ----

data_benthic_cat <- data_benthic %>% 
  filter(!(datasetID %in% c("0011", "0012", "0013", "0014", "0020"))) %>% 
  mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                              subcategory == "Coralline algae" ~ "Coralline algae",
                              subcategory == "Turf algae" ~ "Turf algae",
                              subcategory == "Cyanobacteria" ~ "Cyanobacteria",
                              TRUE ~ category)) %>% 
  filter(category %in% c("Hard coral", "Macroalgae", "Coralline algae", "Turf algae", "Cyanobacteria", "Abiotic")) %>% 
  group_by(datasetID, higherGeography, country, territory, locality, habitat,
           parentEventID, eventID, decimalLatitude, decimalLongitude, verbatimDepth,
           year, month, day, category) %>% 
  summarise(measurementValue = sum(measurementValue)) %>% 
  ungroup() %>%
  mutate(category = as_factor(category))

# 5. 'measurementValue' density and temporal ----

## 5.1 Entire Pacific region ----

# 1. Temporal

plot_a <- ggplot(data = data_benthic_cat, aes(x = measurementValue, fill = category)) +
  geom_density(alpha = 0.75, show.legend = FALSE) +
  geom_vline(xintercept = c(0, 100)) +
  labs(x = "Cover (%)", y = "Density") +
  facet_wrap(~category, nrow = 1)

# 2. Density

plot_b <- ggplot(data = data_benthic_cat, aes(x = year, y = measurementValue, col = category)) +
  geom_smooth(show.legend = FALSE) +
  lims(y = c(0, 100)) +
  labs(y = "Cover (%)", x = "Year") +
  facet_wrap(~category, nrow = 1)

# 3. Combine plots

plot_a + plot_b + plot_layout(nrow = 2)

# 4. Export the plot

ggsave("figs/05_additional/01_data-explo/01_overall.png",
       dpi = 600, height = 8, width = 14)

## 5.2 Comparison plots ----

# 1. Density

# 1.1 Per datasetID

ggplot(data = data_benthic_cat, aes(x = measurementValue, fill = category)) +
  geom_density(alpha = 0.75, show.legend = FALSE) +
  geom_vline(xintercept = c(0, 100)) +
  labs(x = "Benthic cover (%)", y = "Density") +
  facet_grid(category~datasetID, scales = "free_y")

ggsave("figs/05_additional/01_data-explo/02_comparison_density-dataset.png",
       dpi = 600, height = 15, width = 40)

# 1.2 Per territory 

ggplot(data = data_benthic_cat, aes(x = measurementValue, fill = category)) +
  geom_density(alpha = 0.75, show.legend = FALSE) +
  geom_vline(xintercept = c(0, 100)) +
  labs(x = "Benthic cover (%)", y = "Density") +
  facet_grid(category~territory, scales = "free_y")

ggsave("figs/05_additional/01_data-explo/02_comparison_density-territory.png",
       dpi = 600, height = 15, width = 40)

# 2. Temporal

# 2.1 Per datasetID

ggplot(data = data_benthic_cat, aes(x = year, y = measurementValue, col = category)) +
  geom_smooth(show.legend = FALSE) +
  lims(y = c(0, 100)) +
  labs(y = "Benthic cover (%)", x = "Year") +
  facet_grid(category~datasetID)

ggsave("figs/05_additional/01_data-explo/02_comparison_temporal-dataset.png",
       dpi = 600, height = 15, width = 35)

# 2.2 Per territory

ggplot(data = data_benthic_cat, aes(x = year, y = measurementValue, col = category)) +
  geom_smooth(show.legend = FALSE) +
  lims(y = c(0, 100)) +
  labs(y = "Benthic cover (%)", x = "Year") +
  facet_grid(category~territory)

ggsave("figs/05_additional/01_data-explo/02_comparison_temporal-territory.png",
       dpi = 600, height = 15, width = 35)

## 5.3 Individual plots ----

# 1. Create the function

data_explo <- function(data, i, type){
  
  if(type == "dataset"){
    
    data_benthic_cat_i <- data %>% 
      filter(datasetID == i)
    
    plot_a <- ggplot(data = data_benthic_cat_i, aes(x = year, y = measurementValue, col = category)) +
      geom_point(alpha = 0.1, col = "grey") +
      geom_smooth(show.legend = FALSE) +
      lims(y = c(0, 100), x = c(1980, 2023)) +
      labs(y = "Cover (%)", x = "Year") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_b <- ggplot(data = data_benthic_cat_i, aes(x = measurementValue, fill = category)) +
      geom_density(alpha = 0.75, show.legend = FALSE) +
      lims(x = c(0, 100)) +
      labs(x = "Cover (%)", y = "Density") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_all <- plot_a + plot_b + 
      plot_annotation(title = paste0("datasetID = ", i),
                      theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_layout(ncol = 1)
    
    ggsave(paste0("figs/05_additional/01_data-explo/03_temporal-dataset_", i, ".png"),
           dpi = 600, height = 6, width = 10)
    
  }else if(type == "territory"){
    
    data_benthic_cat_i <- data %>% 
      filter(territory == i)
    
    plot_a <- ggplot(data = data_benthic_cat_i, aes(x = year, y = measurementValue, col = category)) +
      geom_point(alpha = 0.1, col = "grey") +
      geom_smooth(show.legend = FALSE) +
      lims(y = c(0, 100), x = c(1980, 2023)) +
      labs(y = "Cover (%)", x = "Year") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_b <- ggplot(data = data_benthic_cat_i, aes(x = measurementValue, fill = category)) +
      geom_density(alpha = 0.75, show.legend = FALSE) +
      lims(x = c(0, 100)) +
      labs(x = "Cover (%)", y = "Density") +
      facet_wrap(~category, nrow = 1, drop = FALSE)
    
    plot_all <- plot_a + plot_b + 
      plot_annotation(title = paste0("territory = ", i),
                      theme = theme(plot.title = element_text(hjust = 0.5))) +
      plot_layout(ncol = 1)
    
    ggsave(paste0("figs/05_additional/01_data-explo/04_temporal-territory_",
                  str_replace_all(str_to_lower(i), " ", "-"), ".png"), 
           dpi = 600, height = 6, width = 10)
    
  }else{
    
    stop("The argument type must be dataset or territory")
    
  }
  
}

# 2. Map over the function for territories 

map(unique(data_benthic_cat$territory), 
    ~data_explo(data = data_benthic_cat, i = ., type = "territory"))

# 3. Map over the function for datasetID 

map(unique(data_benthic_cat$datasetID), 
    ~data_explo(data = data_benthic_cat, i = ., type = "dataset"))

# 6. Benthic categories ----

## 6.1 Taxonomic levels ----

# Create the function

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

# 6.1.1 For the entire dataset

data_benthic %>% 
  taxonomic_levels()

# 6.1.2 For hards corals

data_benthic %>% 
  filter(category == "Hard coral") %>% 
  taxonomic_levels()

# 6.1.3 For algae

data_benthic %>% 
  filter(category == "Algae") %>% 
  taxonomic_levels()

## 6.2 Taxonomic frequency ----

# 6.2.1 Create a function

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

# 6.2.2 Plot for percentage of dataset

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
  scale_fill_manual(values = palette_5cols)

# 6.2.3 Plot for percentage of territories

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
  scale_fill_manual(values = palette_5cols)

# 6.2.4 Plot for percentage of surveys

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
  scale_fill_manual(values = palette_5cols)

# 6.2.5 Combine plots 

plot_a + plot_b + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank()) +
  plot_c + theme(axis.text.y = element_blank(),
                 axis.ticks.y = element_blank())

# 6.2.6 Export the plot

ggsave("figs/05_additional/01_data-explo/05_taxonomic-frequency.png", dpi = 600, height = 6, width = 12)
