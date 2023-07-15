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

# 4. Create the function ----

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
    
    ggsave(filename = paste0("figs/territories_fig-3-a/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
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
    
    ggsave(filename = paste0("figs/territories_fig-3-b/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
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
    
    ggsave(filename = paste0("figs/territories_fig-3-a/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
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
    
    ggsave(filename = paste0("figs/territories_fig-3-b/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_b, height = 5, width = 8, dpi = 600)

    }
    
}

# 6.2 Map over the function --

map(unique(data_cyclones$territory), ~map_cyclone(territory_i = .))
