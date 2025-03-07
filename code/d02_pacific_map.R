# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
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

data_eez_disputed <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(GEONAME == "Overlapping claim Matthew and Hunter Islands: France / Vanuatu") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

data_eez <- bind_rows(data_eez, data_eez_disputed)

# 6. Country boundaries ----

data_countries <- read_sf("data/01_background-shp/01_ne/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs_selected)

# 7. Bathymetry ----

load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")

data_bathy <- data_bathy %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 8. Create the tropics ----

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

# 9. Create text annotation ----

# 9.1 Tropics --

data_text_tropics <- tibble(long = c(-105, -104, -119, -128),
                            lat = c(-21.43, -25.43, 2, 25.43),
                            text = c("Tropic of", "Capricorn", "Equator", "Tropic of Cancer")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 9.2 Pacific Ocean --

data_text_pacific <- tibble(long = c(-130),
                            lat = c(13),
                            text = c("Pacific Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 9.3 Australia --

data_text_australia <- tibble(long = c(140),
                              lat = c(-25),
                              text = c("Australia")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 9.4 Labels --

data_text_labels <- data_eez %>%
  st_drop_geometry() %>% 
  select(number, TERRITORY1, lat, long) %>% 
  drop_na(lat) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected) %>% 
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Howland and Baker Islands" = "Ho.",
                                                    "Palmyra Atoll" = "Pa.",
                                                    "Jarvis Island" = "Ja.",
                                                    "Johnston Atoll" = "Jo.",
                                                    "American Samoa" = "Am.\nSamoa",
                                                    "Samoa" = "Sa.",
                                                    "Wallis and Futuna" = "WF",
                                                    "Wake Island" = "Wa.",
                                                    "Nauru" = "Na.",
                                                    "Northern Mariana Islands" = "North.\nMariana\nIslands",
                                                    "Federated States of Micronesia" = "Fed. St. of Micronesia",
                                                    "Islands" = "Isl.",
                                                    "Marshall Isl." = "Marshall\nIsl.",
                                                    "French Polynesia" = "French\nPolynesia",
                                                    "Line Group" = "Line\nGroup",
                                                    "New Caledonia" = "New\nCaledonia",
                                                    "Phoenix Group" = "Phoenix\nGroup")))

# 10. Bbox for alpha on bathymetry ---

data_alpha <- tibble(lat = c(-4000000, 4000000),
                     lon = c(-3500000, 11000000)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_alpha <- data_eez %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_difference(data_alpha, .)

# 11. Make the map ----

plot_map_base <- ggplot() +
  geom_sf(data = data_bathy %>% filter(depth == 0), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 200), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 1000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 2000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 3000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 4000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 5000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 6000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 7000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 8000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 9000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 10000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  scale_fill_identity() +
  geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
  # Tropics
  geom_sf(data = data_tropics_no_eez, linetype = "dashed", color = "#363737", linewidth = 0.25) +
  # EEZ
  geom_sf(data = data_eez, color = "#363737", fill = "#e4e9ed", alpha = 0.2) +
  # Background map
  geom_sf(data = data_map, fill = "grey", col = "darkgrey") +
  # Country boundaries
  geom_sf(data = data_countries, fill = "grey", col = "darkgrey") +
  # Annotation (legend)
  geom_sf_text(data = data_text_australia, aes(label = text), 
               color = "#363737", size = 3, family = font_choose_map) +
  geom_sf_text(data = data_text_tropics, aes(label = text), hjust = 1,
               color = "#363737", size = 3.5, family = font_choose_map, fontface = "italic") +
  geom_sf_text(data = data_text_pacific, aes(label = text), 
               color = "#1e517b", fontface = "italic", size = 3.5, family = font_choose_map) +
  # Graphical aspects
  coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE) +
  scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120)) +
  theme_map()

# 12. Coral reef distribution ----

data_reefs <- read_sf("data/03_reefs-area_wri/clean/pacific_reef.shp")

plot <- plot_map_base +
  geom_sf(data = data_reefs, color = "red") +
  coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE)
  
ggsave(filename = "figs/01_part-1/fig-1b.png", width = 8, height = 4.75, dpi = 300)

# 13. EEZ ----

plot <- plot_map_base +
  # Annotation (labels EEZ)
  geom_sf_text(data = data_text_labels, aes(label = TERRITORY1),
               color = "black", size = 2.8, family = font_choose_map)

ggsave(filename = "figs/01_part-1/fig-1.png", width = 8, height = 4.75, dpi = 300)
