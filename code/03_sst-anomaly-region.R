# 1. Load packages ----

library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(RColorBrewer)
library(patchwork)
library(extrafont)

source("code/function/plot_sst_region.R")

# 2. Load shapefiles ----

# 2.1 Background map --

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")

# 2.2 EEZ --

data_eez <- read_sf("data/01_background-shp/03_eez/data_eez.shp")

# 3. Get path of raster ----

raster_paths <- tibble(path = list.files("data/08_sst-anom-region/", full.names = TRUE)) %>% 
  mutate(year = str_sub(path, -8, -5))

# 4. Create the plots ----

plot_list <- map2(raster_paths$path, raster_paths$year, ~plot_sst_region(raster_path = .x, raster_year = .y))

# 5. Assemble the plots ----

wrap_plots(plot_list, ncol = 2) + plot_layout(guides = "collect")

# 6. Export the plot ----

ggsave(filename = "figs/02_sst-anom-region.png", width = 10, height = 8)
