# 1. Required packages ----

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

# 4. Create the function ----

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
    
    ggplot() +
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
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
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
    
    ggplot() +
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
      geom_sf(data = data_eez_i, color = "black", alpha = 0.75) +
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }
  
  ggsave(filename = paste0("figs/territories_fig-1/", str_replace_all(str_to_lower(territory), " ", "-"), ".png"), dpi = 600)
  
}

# 5. Map over the function ----

map(unique(data_eez$TERRITORY1), ~map_eez(territory = .))
