# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp")
load("data/01_background-shp/03_eez/data_eez.RData")
load("data/01_background-shp/02_princeton/data_land.RData")
load("data/05_cyclones/02_cyclones_extracted.RData")
load("data/05_cyclones/01_cyclones_lines.RData")
load("data/05_cyclones/01_cyclones_points.RData")

data_cyclones <- data_cyclones %>% 
  mutate(time = as_date(time),
         ts_dist = round(ts_dist, 1))

# 4. Plot of cyclones occurrence over time ----

# 4.1 Create the function --

map_cyclone_plot <- function(territory_i){
  
  ggplot(data = data_cyclones %>% filter(territory == territory_i), aes(x = time, y = wind_speed)) +
    geom_point(color = "white", shape = 21, fill = "red", size = 4) +
    geom_text(aes(y = wind_speed + 22, label = name), size = 3, family = font_choose_graph, col = "#d91e18") +
    geom_text(aes(y = wind_speed + 12, label = paste0(ts_dist, " km")), size = 2, family = font_choose_graph, col = "#2e3131") +
    lims(x = c(ymd("1970-01-01"), ymd("2030-01-01"))) +
    labs(x = "Time (years)", y = bquote("Wind speed (km."~h^-1*")"), title = "B") +
    coord_cartesian(expand = FALSE, clip = "on") +
    scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(-20, 300)) +
    theme(text = element_text(family = font_choose_graph),
          panel.grid.major.y = element_line(color = "lightgrey", linewidth = 0.3),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.length = unit(4, "pt"),
          axis.line.x = element_line(color = "black", linewidth = 0.5),
          panel.background = element_blank(),
          plot.margin = margin(t = 5, r = 15, b = 5, l = 5, unit = "pt"))
  
  ggsave(filename = paste0("figs/territories_fig-3/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 4, dpi = 600)
  
}

# 4.2 Map over the function --

map(unique(data_cyclones$territory), ~map_cyclone_plot(territory_i = .))

# 5. Map of cyclone occurrence over EEZ ----

# 5.1 Create the function --

map_cyclone_map <- function(territory_i){
  
  if(territory_i %in% c("Fiji", "Wallis and Futuna", "Hawaii", "Tuvalu", "Gilbert Islands")){
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)

    ggplot() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", fill = "#89c4f4", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }else{
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory_i)
    
    data_cyclones_i <- data_cyclones %>% 
      filter(territory == territory_i)
    
    data_ts_lines_i <- left_join(data_cyclones_i, data_ts_lines) %>% 
      st_as_sf() %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 4326))
    
    data_ts_points_i <- data_ts_points %>% 
      filter(ts_id %in% unique(data_cyclones_i$ts_id)) %>% 
      st_as_sf() %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 4326))
    
    ggplot() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", fill = "#89c4f4", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Cyclone path
      geom_sf(data = data_ts_lines_i) +
      # Cyclone points
      geom_sf(data = data_ts_points_i, aes(fill = wind_speed), shape = 21, color = "black") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }
  
  ggsave(filename = paste0("figs/territories_fig-3-b/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"), dpi = 600)
  
}

# 5.2 Map over the function --

map(unique(data_cyclones$territory), ~map_cyclone_map(territory_i = .))
