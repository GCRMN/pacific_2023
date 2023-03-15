# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")
load("data/01_background-shp/02_princeton/data_land.RData")
load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")

# 4. Map of spatio-temporal distribution of monitoring sites -----------------------------------------------------

# 4.1 Transform benthic data --

load("data/04_data-benthic.RData")

data_benthic <- data_benthic %>% 
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
    
    data_benthic_i <- data_benthic %>% 
      filter(TERRITORY1 == territory) %>% 
      st_transform(., crs = 3460)
    
    ggplot() +
      # Bathymetry
      geom_sf(data = data_bathy_i, aes(fill = color), color = NA) +
      scale_fill_identity() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Benthic data
      geom_sf(data = data_benthic_i, aes(color = interval_class)) +
      scale_color_manual(values = palette_5cols,
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE, name = "Number of years with data") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }else{
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory)
    
    data_bathy_i <- data_bathy %>% 
      filter(TERRITORY1 == territory)
    
    data_benthic_i <- data_benthic %>% 
      filter(TERRITORY1 == territory)
    
    ggplot() +
      # Bathymetry
      geom_sf(data = data_bathy_i, aes(fill = color), color = NA) +
      scale_fill_identity() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Benthic data
      geom_sf(data = data_benthic_i, aes(color = interval_class)) +
      scale_color_manual(values = palette_5cols,
                         labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                         drop = FALSE, name = "Number of years with data") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }
  
  ggsave(filename = paste0("figs/territories_fig-4/", str_replace_all(str_to_lower(territory), " ", "-"), ".png"), dpi = 600)
  
}

# 4.3 Map over the function --

map(unique(data_eez$TERRITORY1), ~map_eez(territory = .))

# 5. Extract descriptors -----------------------------------------------------

load("data/04_data-benthic.RData")

# 5.1 Number of monitoring sites --

data_sites <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count(name = "n_sites")

# 5.2 Number of surveys --

data_surveys <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year, month, day) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count(name = "n_surveys")

# 5.3 Number of datasets --

data_datasets <- data_benthic %>% 
  select(territory, datasetID) %>% 
  group_by(territory) %>% 
  distinct() %>% 
  count(name = "n_datasets")

# 5.4 Combine datasets --

data_descriptors <- left_join(data_sites, data_surveys) %>% 
  left_join(., data_datasets)

rm(data_sites, data_surveys, data_datasets)

# 6. Plot of number of surveys per year -----------------------------------------------------

# 6.1 Select data --

data_benthic <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  distinct()

# 6.2 Create the function --

map_survey_years <- function(territory_i){
  
  # 1. Filter data ----
  
  data_benthic_i <- data_benthic %>% 
    filter(territory == territory_i)
  
  # 2. Make the plot ----
  
  ggplot(data = data_benthic_i, aes(x = year)) +
    # By default its density, width*density*100 gives percentage
    geom_histogram(binwidth = 1, aes(y = after_stat(width*density*100)),
                   color = "black", fill = "#5c97bf", color = "#446cb3") +
    lims(x = c(1970, 2024)) +
    labs(x = "Year", y = "Surveys (%)", title = "B") +
    theme_graph() +
    theme(plot.title = element_text(size = 20))
  
  # 3. Export the plot ----
  
  ggsave(filename = paste0("figs/territories_fig-4-b/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 4, height = 3, dpi = 600)
  
}

# 6.3 Map over the function --

map(unique(data_benthic$territory), ~map_survey_years(territory_i = .))
