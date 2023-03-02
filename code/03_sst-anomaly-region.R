library(tidyverse)
library(sf)
library(terra)
library(RColorBrewer)

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")


A <- rast("data/sst_anomaly_2016.tif")


A <- A*0.01
the_palette_fc <- leaflet::colorNumeric(palette = "RdBu", 
                                        domain = c(-4, 4),
                                        reverse = TRUE)

the_colors <- the_palette_fc(seq(min(A[], na.rm = TRUE), max(A[], na.rm = TRUE), length.out = 50))


terra::plot(A, axes = FALSE, col = the_colors)
terra::plot(data_map, add = TRUE, col = "darkgrey")
