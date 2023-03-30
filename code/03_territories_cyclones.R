# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(sf)
sf_use_s2(FALSE)
library(ggrepel)
library(glue)
library(scales)

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

# 4. Transform data ----

n_cyclones <- 10 # Number of cyclones label to show on plots

data_cyclones <- data_cyclones %>% 
  group_by(territory) %>% 
  arrange(desc(wind_speed)) %>% 
  mutate(pos = row_number()) %>% 
  ungroup() %>% 
  mutate(time = as_date(time),
         ts_dist = round(ts_dist, 1),
         # 13 is the maximum number of labels to show
         name_a = if_else(pos <= n_cyclones, glue('"{name}" * phantom(" ({ts_dist} km)")'), NA),
         name_b = if_else(pos <= n_cyclones, glue('phantom("{name}") * " ({ts_dist} km)"'), NA))

# NOTE - A combination of geom_text_repel() with geom_richtext() doesn't exist
# I use the solution proposed on https://github.com/slowkow/ggrepel/issues/145

# 5. Plot of cyclones occurrence over time ----

# 5.1 Create the function --

map_cyclone_plot <- function(territory_i){
  
  ggplot(data = data_cyclones %>% filter(territory == territory_i), 
         aes(x = time, y = wind_speed, fill = ifelse(pos <= n_cyclones, "#d91e18", "darkgrey"))) +
    geom_text_repel(aes(y = wind_speed, label = name_a), size = 2,
                    family = font_choose_graph, col = "#d91e18",
                    min.segment.length = 0, segment.size = 0.25,
                    force = 50, seed = 1, parse = TRUE) +
    geom_text_repel(aes(y = wind_speed, label = name_b), size = 2,
                    family = font_choose_graph, col = "black",
                    min.segment.length = 0, segment.size = 0.25,
                    force = 50, seed = 1, parse = TRUE) +
    geom_point(color = "white", shape = 21, size = 3, show.legend = FALSE) +
    lims(x = c(ymd("1970-01-01"), ymd("2030-01-01"))) +
    labs(x = "Year", y = bquote("Wind speed (km."~h^-1*")")) +
    scale_fill_identity() +
    scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(-20, 300)) +
    scale_x_date(breaks = c(as.Date(c("1970-01-01", "1980-01-01", "1990-01-01", "2000-01-01", "2010-01-01", "2020-01-01", "2030-01-01"))),
                 labels=date_format("%Y"),
                 limits = as.Date(c("1970-01-01", "2030-01-01"))) +
    theme_graph()
  
  ggsave(filename = paste0("figs/territories_fig-3/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 4, dpi = 600)
  
}

# 5.2 Map over the function --

map(unique(data_cyclones$territory), ~map_cyclone_plot(territory_i = .))

# 6. Map of cyclone occurrence over EEZ ----

# 6.1 Create the function --

map_cyclone_map <- function(territory_i){
  
  if(territory_i %in% c("Fiji", "Wallis and Futuna", "Hawaii", "Tuvalu", "Gilbert Islands")){
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)
    
    data_cyclones_i <- data_cyclones %>% 
      filter(territory == territory_i)
    
    data_ts_lines_i <- left_join(data_cyclones_i, data_ts_lines) %>% 
      st_as_sf() %>% 
      st_transform(., crs = 3460) %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 3460))

    ggplot() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", fill = "#89c4f4", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Cyclone path (those > n_cyclones)
      geom_sf(data = data_ts_lines_i %>% filter(pos > n_cyclones), col = "darkgrey") +
      # Cyclone path (those <= n_cyclones)
      geom_sf(data = data_ts_lines_i %>% filter(pos <= n_cyclones), col = "#d91e18") +
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
    
    ggplot() +
      # EEZ borders
      geom_sf(data = data_eez_i, color = "black", fill = "#89c4f4", alpha = 0.75) +
      # Lands
      geom_sf(data = data_land_i, fill = "#363737", col = "grey") +
      # Cyclone path (those > n_cyclones)
      geom_sf(data = data_ts_lines_i %>% filter(pos > n_cyclones), col = "darkgrey") +
      # Cyclone path (those <= n_cyclones)
      geom_sf(data = data_ts_lines_i %>% filter(pos <= n_cyclones), col = "#d91e18") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }
  
  ggsave(filename = paste0("figs/territories_fig-3-b/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"), dpi = 600)
  
}

# 6.2 Map over the function --

map(unique(data_cyclones$territory), ~map_cyclone_map(territory_i = .))
