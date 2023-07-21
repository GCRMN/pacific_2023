# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(stringr)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")

# 3. Define changed CRS ----

# 3.1 Define the CRS --

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 3.2 Define the offset --

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

# 3.3 Define a long and slim polygon that overlaps the meridian line --

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# 4. Load background maps ----

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 5. Economic Exclusive Zones ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

# 6. Country boundaries ----

data_countries <- read_sf("data/01_background-shp/01_ne/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs_selected)

# 7. Create the tropics ----

data_tropics <- tibble(long = c(-180, 180, -180, 180, -180, 180), 
                       lat = c(0, 0, 23.43656, 23.43656, -23.43656, -23.43656), 
                       tropic = c("Equator", "Equator", "Tropic of Cancer", "Tropic of Cancer",
                                  "Tropic of Capricorne", "Tropic of Capricorne")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(tropic) %>% 
  summarise() %>% 
  st_cast("LINESTRING") %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

data_tropics_no_eez <- st_intersection(data_tropics, data_eez)

data_tropics_no_eez$var <- "true"

data_tropics_no_eez <- st_difference(data_tropics, st_union(st_geometry(data_tropics_no_eez)))

# 8. Create text annotation ----

# 8.1 Tropics --

data_text_tropics <- tibble(long = c(-105, -104, -119, -128),
                            lat = c(-21.43, -25.43, 2, 25.43),
                            text = c("Tropic of", "Capricorn", "Equator", "Tropic of Cancer")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 8.2 Pacific Ocean --

data_text_pacific <- tibble(long = c(-130),
                            lat = c(13),
                            text = c("Pacific Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 8.3 Australia --

data_text_australia <- tibble(long = c(140),
                              lat = c(-25),
                              text = c("AUSTRALIA")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 8.4 Labels --

data_text_labels <- data_eez %>%
  st_drop_geometry() %>% 
  select(number, lat, long) %>% 
  drop_na(lat) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 9. Make the map ----

plot_map <- ggplot() +
  # Tropics
  #geom_sf(data = data_tropics, linetype = "dashed", color = "#363737", linewidth = 0.25) +
  geom_sf(data = data_tropics_no_eez, linetype = "dashed", color = "#363737", linewidth = 0.25) +
  # EEZ
  geom_sf(data = data_eez, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
  # Background map
  geom_sf(data = data_map, fill = "#363737", col = "grey") +
  # Country boundaries
  geom_sf(data = data_countries, fill = "#363737", col = "grey") +
  # Annotation (legend)
  geom_sf_text(data = data_text_australia, aes(label = text), 
               color = "darkgrey", size = 2.5, family = font_choose_map) +
  geom_sf_text(data = data_text_tropics, aes(label = text), hjust = 1,
               color = "#363737", size = 2.5, family = font_choose_map, fontface = "italic") +
  geom_sf_text(data = data_text_pacific, aes(label = text), 
               color = "#5c97bf", fontface = "italic", size = 3, family = font_choose_map) +
  # Annotation (labels EEZ)
  geom_sf(data = data_text_labels, size = 4, color = "#5c97bf") +
  geom_sf_text(data = data_text_labels, aes(label = number), color = "white", size = 2) +
  # Graphical aspects
  coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE) +
  scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120)) +
  theme(text = element_text(family = font_choose_map),
        panel.background = element_rect(fill = "#ebf5fd"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        axis.title = element_blank(),
        axis.text.x.top = element_text())

# 10. Create legend of EEZ names ----

# 10.1 Transform data --

data_eez_text <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, number) %>% 
  mutate(y_pos = -number,
         x_pos = case_when(number %in% 1:11 ~ 1,
                           number %in% 12:21 ~ 3,
                           number %in% 22:31 ~ 5),
         y_pos = case_when(number %in% 1:11 ~ y_pos,
                           number %in% 12:21 ~ y_pos+11,
                           number %in% 22:31 ~ y_pos+21))

# 10.2 Make the plot --

plot_eez <- ggplot() +
  # Points
  geom_point(data = tibble(x = 1, y = -1:-11), aes(x = x, y = y), size = 6, color = "#5c97bf") +
  geom_point(data = tibble(x = 3, y = -1:-10), aes(x = x, y = y), size = 6, color = "#5c97bf") +
  geom_point(data = tibble(x = 5, y = -1:-10), aes(x = x, y = y), size = 6, color = "#5c97bf") +
  # Text inside points
  geom_text(aes(x = 1, y = -1:-11, label = as.character(1:11)), 
            color = "white", vjust = 0.5, size = 2.5, family = font_choose_map) +
  geom_text(aes(x = 3, y = -1:-10, label = as.character(12:21)), 
            color = "white", vjust = 0.5, size = 2.5, family = font_choose_map) +
  geom_text(aes(x = 5, y = -1:-10, label = as.character(22:31)), 
            color = "white", vjust = 0.5, size = 2.5, family = font_choose_map) +
  # Names of territories
  geom_text(data = data_eez_text, aes(x = x_pos, y = y_pos, label = TERRITORY1),
            hjust = 0, size = 3, nudge_x = 0.25, family = font_choose_map) +
  # Graphical aspects
  lims(x = c(0.75, 6.25), y = c(-12, 0)) +
  coord_cartesian(expand = FALSE) +
  theme(text = element_text(family = font_choose_map), 
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

# 11. Combine the plots ----

plot_map + plot_eez + plot_layout(ncol = 1, heights =  c(1, 0.6))

# 12. Save the plot ----

ggsave(filename = "figs/01_pacific-map.png", width = 8, height = 7.75, dpi = 600)
