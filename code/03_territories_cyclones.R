# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggrepel)
library(glue)
library(scales)
library(lubridate)
library(scico)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")
load("data/01_background-shp/02_princeton/data_land.RData")
load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")
load("data/05_cyclones/02_cyclones_extracted.RData")
load("data/05_cyclones/01_cyclones_lines.RData")
load("data/05_cyclones/01_cyclones_points.RData")

# 4. Map of cyclones trajectories ----

## 4.1 Create the function ----

map_cyclone <- function(territory_i){
  
  if(territory_i %in% c("Fiji", "Wallis and Futuna", "Hawaii", "Tuvalu", "Gilbert Islands")){
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)
    
    data_bathy_i <- data_bathy %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(., crs = 3460)
    
    data_cyclones_i <- data_cyclones %>% 
      filter(territory == territory_i) %>% 
      group_by(saffir) %>% 
      mutate(max_saffir = max(saffir)) %>% 
      ungroup() %>% 
      filter(max_saffir >= 1)
    
    if (nrow(data_cyclones_i) == 0) {
      return(NULL)
    }
    
    data_cyclones_i <- data_cyclones_i %>%  
      mutate(time_range = case_when(time > as_date("1980-01-01") & time <= as_date("1989-12-31") ~ "1980 - 1989",
                                    time > as_date("1990-01-01") & time <= as_date("1999-12-31") ~ "1990 - 1999",
                                    time > as_date("2000-01-01") & time <= as_date("2009-12-31") ~ "2000 - 2009",
                                    time > as_date("2010-01-01") & time <= as_date("2023-12-31") ~ "2010 - 2023"),
             time_range = as.factor(time_range),
             time_range = fct_expand(time_range, "1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2023"),
             time_range = fct_relevel(time_range, c("1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2023")),
             name = str_to_sentence(name)) %>% 
      bind_cols(., color = sample(scico(nrow(.), begin = 0.3, end = 1, palette = "lajolla")))
    
    data_ts_lines_i <- left_join(data_cyclones_i, data_ts_lines) %>% 
      st_as_sf() %>% 
      st_transform(., crs = 3460) %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 3460))
    
    data_ts_points_i <- left_join(data_cyclones_i, data_ts_points) %>% 
      st_as_sf() %>% 
      st_transform(., crs = 3460) %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 3460))
    
    plot_a <- ggplot() +
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
      geom_sf(data = data_ts_lines_i, aes(col = color), show.legend = FALSE) +
      geom_sf(data = data_ts_points_i, aes(col = color), show.legend = FALSE, size = 0.75) +
      geom_sf_label(data = data_ts_lines_i, aes(col = color, label = name), show.legend = FALSE,
                    size = 1.75, label.size = 0, alpha = 0.85, label.r = unit(0.4, "lines")) +
      scale_color_identity() +
      facet_wrap(~time_range, ncol = 2, drop = FALSE) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            strip.text.x = element_text(size = 12, family = font_choose_graph, face = "bold")) +
      labs(x = NULL, y = NULL)
    
    ggsave(filename = paste0("figs/territories_fig-7/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_a, height = 10, width = 6, dpi = 600)
    
    plot_b <- ggplot(data = data_cyclones_i, aes(x = time, y = wind_speed)) +
      geom_point(aes(fill = color), color = "white", shape = 21, size = 4, show.legend = FALSE) +
      geom_label_repel(aes(label = name, color = color), fill = "white", alpha = 0.9,
                       show.legend = FALSE, label.r = unit(0.4, "lines")) +
      scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(0, 300)) +
      coord_cartesian(ylim = c(14.25, 310)) +
      scale_fill_identity() +
      scale_color_identity() +
      lims(x = c(ymd("1975-01-01"), ymd("2025-01-01"))) +
      labs(x = "Year", y = bquote("Wind speed (km."~h^-1*")")) +
      theme_graph()
    
    ggsave(filename = paste0("figs/territories_fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_b, height = 5, width = 8, dpi = 600)
    
    }else{
  
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i)
    
    data_land_i <- data_land %>% 
      filter(TERRITORY1 == territory_i)
    
    data_bathy_i <- data_bathy %>% 
      filter(TERRITORY1 == territory_i)
    
    data_cyclones_i <- data_cyclones %>% 
      filter(territory == territory_i) %>% 
      group_by(saffir) %>% 
      mutate(max_saffir = max(saffir)) %>% 
      ungroup() %>% 
      filter(max_saffir >= 1)
    
    if (nrow(data_cyclones_i) == 0) {
      return(NULL)
    }
    
    data_cyclones_i <- data_cyclones_i %>%  
      mutate(time_range = case_when(time > as_date("1980-01-01") & time <= as_date("1989-12-31") ~ "1980 - 1989",
                                    time > as_date("1990-01-01") & time <= as_date("1999-12-31") ~ "1990 - 1999",
                                    time > as_date("2000-01-01") & time <= as_date("2009-12-31") ~ "2000 - 2009",
                                    time > as_date("2010-01-01") & time <= as_date("2023-12-31") ~ "2010 - 2023"),
             time_range = as.factor(time_range),
             time_range = fct_expand(time_range, "1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2023"),
             time_range = fct_relevel(time_range, c("1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2023")),
             name = str_to_sentence(name)) %>% 
      bind_cols(., color = sample(scico(nrow(.), begin = 0.3, end = 1, palette = "lajolla")))
             
    data_ts_lines_i <- left_join(data_cyclones_i, data_ts_lines) %>% 
      st_as_sf() %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 4326))
    
    data_ts_points_i <- left_join(data_cyclones_i, data_ts_points) %>% 
      st_as_sf() %>% 
      st_intersection(., data_eez_i %>% st_transform(crs = 4326))

    plot_a <- ggplot() +
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
      geom_sf(data = data_ts_lines_i, aes(col = color), show.legend = FALSE) +
      geom_sf(data = data_ts_points_i, aes(col = color), show.legend = FALSE, size = 0.75) +
      geom_sf_label(data = data_ts_lines_i, aes(col = color, label = name), show.legend = FALSE,
                   size = 1.75, label.size = 0, alpha = 0.85, label.r = unit(0.3, "lines")) +
      scale_color_identity() +
      facet_wrap(~time_range, ncol = 2, drop = FALSE) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            strip.text.x = element_text(size = 12, family = font_choose_graph, face = "bold")) +
      labs(x = NULL, y = NULL)
    
    ggsave(filename = paste0("figs/territories_fig-7/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_a, height = 10, width = 6, dpi = 600)
    
    }
    
}

## 4.2 Map over the function ----

map(unique(data_cyclones$territory), ~map_cyclone(territory_i = .))

# 5. Plots of cyclone maximum wind speed over time ----

## 5.1 Create the function ----

load("data/05_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1) %>% 
  mutate(time_range = case_when(time > as_date("1980-01-01") & time <= as_date("1989-12-31") ~ "1980 - 1989",
                                time > as_date("1990-01-01") & time <= as_date("1999-12-31") ~ "1990 - 1999",
                                time > as_date("2000-01-01") & time <= as_date("2009-12-31") ~ "2000 - 2009",
                                time > as_date("2010-01-01") & time <= as_date("2019-12-31") ~ "2010 - 2020",
                                time > as_date("2020-01-01") & time <= as_date("2023-12-31") ~ "2020 - 2023"),
         time_range = as.factor(time_range),
         time_range = fct_expand(time_range, "1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2023"),
         time_range = fct_relevel(time_range, c("1980 - 1989", "1990 - 1999", "2000 - 2009", "2010 - 2023")),
         name = str_to_sentence(name),
         max_saffir = as.factor(max_saffir))

## 5.2 Create the function ----

map_cyclone_plot <- function(territory_i){
  
  # 1. Filter
  
  data_cyclones_i <- data_cyclones %>% 
    filter(territory == territory_i)
  
  # 2. Make the plot 
  
  plot_i <- ggplot(data = data_cyclones_i, aes(x = time, y = wind_speed)) +
    geom_point(aes(fill = max_saffir), color = "white", shape = 21, size = 4.5) +
    geom_label_repel(aes(label = name, color = max_saffir), fill = "white", alpha = 0.9,
                     label.r = unit(0.4, "lines"), show.legend = FALSE) +
    scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(0, 300)) +
    scale_x_date(expand = c(0, 0), limits = c(ymd("1980-01-01"), ymd("2030-01-01"))) +
    coord_cartesian(ylim = c(14.25, 310)) +
    scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                      labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                      values = c(palette_5cols[2:5], "black"),
                      name = "Saffir-Simpson category",
                      drop = FALSE) +
    scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                       labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                       values = c(palette_5cols[2:5], "black"),
                       name = "Saffir-Simpson category",
                       drop = FALSE) +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4))) +
    labs(x = "Year", y = bquote("Wind speed (km."~h^-1*")")) +
    theme_graph() 
  
  # 3. Save the plot
  
  ggsave(filename = paste0("figs/02_part-2/fig-7/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         plot = plot_i, height = 5, width = 10, dpi = 600)
  
}

## 5.3 Map over the function ----

map(unique(data_cyclones$territory), ~map_cyclone_plot(territory_i = .))

# 6. Extract indicators of tropical storms per territory ----

## 6.1 Number of tropical storms ----

data_cyclone_a <- data_cyclones %>% 
  filter(saffir >= 1) %>% 
  group_by(territory) %>% 
  summarise(n_ts = n_distinct(ts_id)) %>% 
  ungroup()

## 6.2 Number of tropical storms with wind > 100 km.h ----

data_cyclone_b <- data_cyclones %>% 
  filter(saffir >= 1 & wind_speed >= 100) %>% 
  group_by(territory) %>% 
  summarise(n_ts_100 = n_distinct(ts_id)) %>% 
  ungroup()

## 6.3 Tropical storm with the highest wind speed ----

data_cyclone_summary <- data_cyclones %>% 
  group_by(territory) %>% 
  filter(wind_speed == max(wind_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(territory, ts_id, name, time, wind_speed, saffir, dist) %>% 
  left_join(., data_cyclone_a) %>% 
  left_join(., data_cyclone_b)
