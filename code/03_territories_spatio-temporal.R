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
      geom_sf(data = data_bathy_i, aes(fill = color), color = NA) +
      scale_fill_identity() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Benthic data
      geom_sf(data = data_benthic_sites_i, aes(color = interval_class)) +
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
    
    data_benthic_sites_i <- data_benthic_sites %>% 
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
      geom_sf(data = data_benthic_sites_i, aes(color = interval_class)) +
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

# 5. Plots of number of surveys per year ----

# 5.1 Select data --

load("data/04_data-benthic.RData")

data_benthic <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  distinct()

# 5.2 Create the function --

map_survey_years <- function(territory_i){
  
  # 1. Plot of percentage of sites per interval_class ----
  
  plot_b <- data_benthic_sites %>% 
    st_drop_geometry() %>% 
    filter(TERRITORY1 == territory_i) %>% 
    group_by(interval_class) %>% 
    count() %>% 
    ungroup() %>% 
    complete(interval_class, fill = list(n = 0)) %>% 
    mutate(percent = n*100/sum(n)) %>% 
    ggplot(data = ., aes(x = interval_class, y = percent, fill = interval_class)) +
      geom_bar(stat = "identity", color = "black", show.legend = FALSE, width = 0.75) +
      scale_fill_manual(values = palette_5cols,
                        labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                        drop = FALSE, name = "Number of years with data") +
      labs(x = "Duration", y = "Sites (%)", title = "B") +
      lims(y = c(0, 100)) +
      theme_graph() +
      theme(plot.title = element_text(size = 20),
            axis.text.x = element_text(size = 7))
  
  # 2. Plot of number of surveys per year ----
  
  plot_c <- data_benthic %>% 
    filter(territory == territory_i) %>% 
    ggplot(data = ., aes(x = year)) +
    # By default its density, width*density*100 gives percentage
    geom_histogram(binwidth = 1, aes(y = after_stat(width*density*100)),
                   color = "black", fill = "#5c97bf") +
    lims(x = c(1970, 2024)) +
    labs(x = "Year", y = "Surveys (%)", title = "C") +
    theme_graph() +
    theme(plot.title = element_text(size = 20))
  
  # 3. Combine plots ----
  
  plot_b + plot_c + plot_layout(ncol = 1)
  
  # 4. Export the plot ----
  
  ggsave(filename = paste0("figs/territories_fig-4-b/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 4, height = 6, dpi = 600)
  
}

# 5.3 Map over the function --

map(unique(data_benthic$territory), ~map_survey_years(territory_i = .))

# 6. Extract descriptors ----

load("data/04_data-benthic.RData")

monitoring_descriptors <- data_benthic %>% 
  group_by(territory) %>% 
  data_descriptors()
