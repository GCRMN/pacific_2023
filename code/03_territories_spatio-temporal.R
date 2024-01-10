# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(patchwork)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/data_descriptors.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")
load("data/01_background-shp/02_princeton/data_land.RData")
load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")

# 4. Map of spatio-temporal distribution of monitoring sites ----

# 4.1 Transform benthic data --

load("data/04_data-benthic.RData")

data_benthic_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, territory) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, territory) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_transform(crs = crs_selected) %>% 
  rename(TERRITORY1 = territory)

# 4.2 Create the function --

map_eez <- function(territory){
  
  if(territory %in% c("Fiji", "Wallis and Futuna", "Hawaii", "Tuvalu", "Gilbert Islands")){
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory) %>% 
      st_transform(., crs = 3460)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory) %>% 
      st_transform(., crs = 3460)
    
    data_bathy_i <- data_bathy %>% 
      filter(TERRITORY1 == territory) %>% 
      st_transform(., crs = 3460)
    
    data_benthic_sites_i <- data_benthic_sites %>% 
      filter(TERRITORY1 == territory) %>% 
      st_transform(., crs = 3460)
    
    ggplot() +
      # Bathymetry
      geom_sf(data = data_bathy_i %>% filter(depth == 0), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 200), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 1000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 2000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 3000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 4000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 5000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 6000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 7000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 8000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 9000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 10000), aes(fill = fill_color), color = NA) +
      scale_fill_identity() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Benthic data
      geom_sf(data = data_benthic_sites_i %>% arrange(interval_class), aes(color = interval_class)) +
      scale_color_manual(values = palette_5cols,
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE, name = "Number of years with data") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            text = element_text(family = font_choose_map)) +
      guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4)))
    
  }else{
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory)
    
    data_bathy_i <- data_bathy %>% 
      filter(TERRITORY1 == territory)
    
    data_benthic_sites_i <- data_benthic_sites %>% 
      filter(TERRITORY1 == territory)
    
    ggplot() +
      # Bathymetry
      geom_sf(data = data_bathy_i %>% filter(depth == 0), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 200), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 1000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 2000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 3000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 4000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 5000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 6000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 7000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 8000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 9000), aes(fill = fill_color), color = NA) +
      geom_sf(data = data_bathy_i %>% filter(depth == 10000), aes(fill = fill_color), color = NA) +
      scale_fill_identity() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Benthic data
      geom_sf(data = data_benthic_sites_i %>% arrange(interval_class), aes(color = interval_class)) +
      scale_color_manual(values = palette_5cols,
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE, name = "Number of years with data") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            text = element_text(family = font_choose_map)) +
      guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4)))
    
  }
  
  ggsave(filename = paste0("figs/territories_fig-9/",
                           str_replace_all(str_to_lower(territory), " ", "-"), ".png"), dpi = 600)
  
}

# 4.3 Map over the function --

map(unique(data_eez$TERRITORY1), ~map_eez(territory = .))

# 5. Plots of number of sites per interval class ----

## 5.1 Create the function ----

map_site_interval <- function(territory_i){
  
  plot_i <- data_benthic_sites %>% 
    st_drop_geometry() %>% 
    filter(TERRITORY1 == territory_i) %>% 
    group_by(interval_class) %>% 
    count() %>% 
    ungroup() %>% 
    complete(interval_class, fill = list(n = 0)) %>% 
    mutate(percent = n*100/sum(n)) %>% 
    ggplot(data = ., aes(x = reorder(interval_class, desc(interval_class)),
                         y = percent, fill = interval_class)) +
      geom_bar(stat = "identity", color = NA, show.legend = FALSE, width = 0.65) +
      scale_fill_manual(values = palette_5cols,
                        labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                        drop = FALSE, name = "Number of years with data") +
      labs(x = NULL, y = "Sites (%)") +
      coord_flip(clip = "off") +
      theme_graph() +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 100))

  ggsave(filename = paste0("figs/02_part-2/fig-8/",
                           str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 4, dpi = 600)
  
}

## 5.2 Map over the function ----

map(unique(data_benthic$territory), ~map_site_interval(territory_i = .))

# 6. Plots of number of surveys per year ----

## 6.1 Transform the data ----

load("data/04_data-benthic.RData")

data_surveys <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year, territory) %>% 
  count() %>% 
  ungroup() %>% 
  complete(year, fill = list(n = 0)) %>% 
  group_by(territory) %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ungroup()

## 6.2 Create the function ----

map_survey_years <- function(territory_i){
  
  plot_i <- data_surveys %>% 
    filter(territory == territory_i) %>% 
    ggplot(data = ., aes(x = year, y = percent)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.8, fill = palette_5cols[5]) +
    labs(x = "Year", y = "Surveys (%)") +
    theme_graph() +
    coord_cartesian(clip = "off") +
    scale_x_continuous(expand = c(0, 0), limits = c(1980, NA))
  
  ggsave(filename = paste0("figs/02_part-2/fig-9/",
                           str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 4, dpi = 600)
  
}

## 6.3 Map over the function ----

map(unique(data_benthic$territory), ~map_survey_years(territory_i = .))

# 7. Extract descriptors ----

load("data/04_data-benthic.RData")

## 7.1 Add subterritories ----

monitoring_descriptors <- data_benthic %>% 
  group_by(territory) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  # Add missing territories (those with no data)
  left_join(st_read("data/01_background-shp/03_eez/data_eez.shp") %>%
              select(TERRITORY1) %>% 
              st_drop_geometry() %>% 
              rename(territory = TERRITORY1),
            .) %>% 
  mutate(across(c("nb_sites", "nb_surveys", "nb_datasets"), .fns = ~replace_na(.,0))) %>% 
  # Add subterritory
  mutate(subterritory = territory,
         territory = case_when(subterritory %in% c("Line Group", "Phoenix Group", "Gilbert Islands") ~ "Kiribati",
                               subterritory %in% c("Jarvis Island", "Johnston Atoll", 
                                                   "Wake Island", "Howland and Baker Islands",
                                                   "Palmyra Atoll") ~ "Pacific Remote Island Area",
                               TRUE ~ subterritory),
         subterritory = if_else(subterritory == territory, NA, subterritory)) %>% 
  arrange(territory, subterritory) %>% 
  relocate(subterritory, .after = territory)

## 7.2 Add total ----

monitoring_descriptors <- data_benthic %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(territory = "Entire Pacific region") %>% 
  bind_rows(monitoring_descriptors, .)

## 7.3 Add total for two territories ----

monitoring_descriptors <- data_benthic %>% 
  mutate(territory = case_when(territory %in% c("Line Group", "Phoenix Group", 
                                                "Gilbert Islands") ~ "Kiribati",
                               territory %in% c("Jarvis Island", "Johnston Atoll", 
                                                "Wake Island", "Howland and Baker Islands",
                                                "Palmyra Atoll") ~ "Pacific Remote Island Area",
                               TRUE ~ territory)) %>% 
  filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
  group_by(territory) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(subterritory = "All") %>% 
  bind_rows(monitoring_descriptors, .) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Entire Pacific region")

## 7.4 Reformat the data and export the table ----

monitoring_descriptors %>% 
  mutate(nb_sites = as.character(format(nb_sites, big.mark = ",", scientific = FALSE)),
         nb_surveys = as.character(format(nb_surveys, big.mark = ",", scientific = FALSE))) %>% 
  openxlsx::write.xlsx(., file = "figs/01_part-1/table-4.xlsx")
