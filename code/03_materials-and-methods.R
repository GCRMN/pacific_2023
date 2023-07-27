# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(patchwork)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

# 3.1 Coral reef distribution --

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  filter(TERRITORY1 == "Marshall Islands") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3.2 EEZ --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid() %>% 
  filter(TERRITORY1 == "Marshall Islands")

# 3.3 Coral reef distribution 100 km buffer --

data_reef_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid() %>% 
  st_intersection(., data_eez) %>% 
  select(TERRITORY1) %>% 
  filter(TERRITORY1 == "Marshall Islands")

# 3.4 Cyclones lines --

load("data/05_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326) %>% 
  filter(ts_id %in% c("1997333N06194", "2009238N12189"))

data_ts_lines <- st_intersection(data_ts_lines, data_eez)

# 3.5 Cyclones points --

load("data/05_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326) %>% 
  filter(ts_id %in% c("1997333N06194", "2009238N12189"))

data_ts_points <- st_intersection(data_ts_points, data_eez)

# 4. Make the plots ----

# 4.1 Plot A --

plot_a <- ggplot() +
  geom_sf(data = data_eez, fill = "#ecf0f1") +
  geom_sf(data = data_reef_buffer, fill = "#f1a9a0") +
  geom_sf(data = data_reef, col = "#2c82c9") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph),
        panel.background = element_rect(colour = "black", linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**1.** Create 100 km coral reef buffer")

# 4.2 Plot B --

plot_b <- ggplot() +
  geom_sf(data = data_eez, fill = "#ecf0f1") +
  geom_sf(data = data_reef_buffer) +
  geom_sf(data = data_reef, col = "#2c82c9") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "1997333N06194"), col = "#d64541") +
  geom_sf(data = data_ts_points %>% filter(ts_id == "1997333N06194"), shape = 21, 
          col = "#d64541", fill = "#d64541") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "2009238N12189")) +
  geom_sf(data = data_ts_points %>% filter(ts_id == "2009238N12189"), shape = 21, fill = "white") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph),
        panel.background = element_rect(colour = "black", linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**2.** Filter cyclone crossing the buffer")

# 4.3 Plot C --

plot_c <- ggplot() +
  geom_sf(data = data_eez, fill = "#ecf0f1") +
  geom_sf(data = data_reef_buffer) +
  geom_sf(data = data_reef, col = "#2c82c9") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "1997333N06194")) +
  geom_sf(data = data_ts_points %>% filter(ts_id == "1997333N06194"), shape = 21, fill = "white") +
  geom_sf(data = data_ts_points %>% 
            filter(ts_id == "1997333N06194") %>% 
            filter(row_number() == 11), shape = 21, fill = "#d64541", size = 2) +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph),
        panel.background = element_rect(colour = "black", linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**3.** Extract cyclone distance and wind speed")

# 5. Combine the plots ----

plot_a + plot_b + plot_c + plot_layout(ncol = 3)

# 6. Export the plot ----

ggsave("figs/materials-methods_cyclones.png", height = 5, width = 15)
