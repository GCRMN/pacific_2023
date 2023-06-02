# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
library(tidyterra)
library(RColorBrewer)
library(extrafont)
library(patchwork)

# 2. Load data ----

# 2.1 Background map --

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")

# 2.2 EEZ --

data_eez <- read_sf("data/01_background-shp/03_eez/data_eez.shp")

# 3. Select the CRS ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 4. Create the function ----

map_dhw <- function(year_i){

  raster <- rast(paste0("data/11_max-dhw/raster_", year_i, ".tif"))
  
  ggplot() +
    geom_spatraster(data = raster) +
    geom_sf(data = data_eez, fill = NA) +
    geom_sf(data = data_map, fill = "#363737", col = "grey") +
    scale_fill_gradientn(colours = c("white", "#e4f1fe", "#89c4f4", "#59abe3", "#fffc7f", 
                                     "#f1d693", "#f9b42d", "#e67e22", "#ec644b",
                                     "#d64541", "#96281b", "#663399", "#5a228b"),
                         limits = c(0, 50),
                         name = "Maximum DHW (°C-weeks)",
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
          plot.title = element_text(face = "bold", family = "Open Sans", hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.2, "cm"),
          legend.position = "bottom")
  
}

# 5. Map over the function ----

#map_dhw(year_i = 1985)

plots <- map(c(1985, 1990, 1995, 1998, 2007, 2016, 2017, 2018), ~map_dhw(year_i = .))

# 6. Combine plots ----

combined_plots <- wrap_plots(plots) + 
  plot_layout(guides = "collect", ncol = 2) & 
  theme(legend.position = "bottom")
  
ggsave(filename = "figs/01_pacific_dhw.png", height = 10, combined_plots, dpi = 600)
