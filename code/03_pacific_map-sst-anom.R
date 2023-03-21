# 1. Load packages ----

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(raster)
library(RColorBrewer)
library(patchwork)
library(extrafont)

# 2. Load data ----

# 2.1 Background map --

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")

# 2.2 EEZ --

data_eez <- read_sf("data/01_background-shp/03_eez/data_eez.shp")

# 2.3 Get path of SST anomalies raster ----

data_path <- list.files("data/08_sst-anom-region/", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(year = str_sub(value, 25, 28))

# 3. Select the CRS ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 4. Create the function ----

map_sst_anom <- function(year_i){
  
  data_path_i <- data_path %>% 
    filter(year == year_i)
  
  raster_a <- rast(as.character(data_path_i[1,1]))*0.01
  
  raster_b <- rast(as.character(data_path_i[2,1]))*0.01
  
  ggplot() +
    geom_spatraster(data = raster_a) +
    geom_spatraster(data = raster_b) +
    geom_sf(data = data_eez, fill = NA) +
    geom_sf(data = data_map, fill = "#363737", col = "grey") +
    scale_fill_gradientn(colors = rev(brewer.pal(n = 11, name = "RdBu")), 
                         breaks = seq(-5, 5, 1), 
                         limits = c(-5, 5),
                         name = "SST anomaly (°C)",
                         guide = guide_colourbar(direction = "horizontal", 
                                                 title.position = "top", 
                                                 title.hjust = 0.5, 
                                                 ticks.colour = "black",
                                                 frame.colour = "black")) +
    coord_sf(crs = crs_selected,
             ylim = c(-4000000, 4000000), 
             xlim = c(-3500000, 11000000), expand = FALSE) +
    labs(title = year_i) +
    theme(text = element_text(family = "Open Sans"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(family = "Open Sans"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.2, "cm"),
          legend.position = "bottom")
  
}

# 5. Map over the function ----

plots <- map(c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), ~map_sst_anom(year_i = .))

# 6. Combine plots ----

combined_plots <- wrap_plots(plots) + 
  plot_layout(guides = "collect", ncol = 2) & 
  theme(legend.position = "bottom")

# 7. Save the plot ----

ggsave(filename = "figs/01_pacific_sst-anomaly.png", combined_plots, dpi = 600)
