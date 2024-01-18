# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(stringr)
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")
source("code/function/theme_graph.R")

# 3. Map of cyclone distribution ----

## 3.1 Define changed CRS ----

### 3.1.1 Define the CRS ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

### 3.1.2 Define the offset ----

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

### 3.1.3 Define a long and slim polygon that overlaps the meridian line ----

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

## 3.2 Load background maps ----

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

## 3.3 Economic Exclusive Zones ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

data_eez_disputed <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(GEONAME == "Overlapping claim Matthew and Hunter Islands: France / Vanuatu") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

data_eez <- bind_rows(data_eez, data_eez_disputed)

## 3.4 Country boundaries ----

data_countries <- read_sf("data/01_background-shp/01_ne/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs_selected)

## 3.5 Bathymetry ----

load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")

data_bathy <- data_bathy %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

## 3.6 Create the tropics ----

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

## 3.7 Create text annotation ----

### 3.7.1 Tropics ----

data_text_tropics <- tibble(long = c(-105, -104, -119, -128),
                            lat = c(-21.43, -25.43, 2, 25.43),
                            text = c("Tropic of", "Capricorn", "Equator", "Tropic of Cancer")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

### 3.7.2 Pacific Ocean ----

data_text_pacific <- tibble(long = c(-115),
                            lat = c(-11),
                            text = c("Pacific Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

### 3.7.3 Australia ----

data_text_australia <- tibble(long = c(140),
                              lat = c(-25),
                              text = c("AUSTRALIA")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

### 3.7.4 Labels ----

data_text_labels <- data_eez %>%
  st_drop_geometry() %>% 
  select(number, lat, long) %>% 
  drop_na(lat) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

## 3.8 Bbox for alpha on bathymetry ----

data_alpha <- tibble(lat = c(-4000000, 4000000),
                     lon = c(-3500000, 11000000)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
  st_bbox() %>% 
  st_as_sfc()

data_alpha <- data_eez %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_difference(data_alpha, .)

## 3.9 Cyclones that passed within 100 km from a coral reef ----

load("data/05_cyclones/01_cyclones_lines.RData")
load("data/05_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  filter(saffir > 0) %>% 
  mutate(saffir = as.factor(saffir)) %>% 
  right_join(data_ts_lines, .)

## 3.10 Make the map ----

plot_map <- ggplot() +
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
  # Cyclones
  geom_sf(data = data_cyclones %>% arrange(saffir), aes(color = saffir),
          alpha = 0.75, linewidth = 0.5, show.legend = "line") +
  scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                     labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                     values = c(palette_5cols[2:5], "black"),
                     name = "Saffir-Simpson category",
                     drop = FALSE) +
  # Annotation (legend)
  geom_sf_text(data = data_text_australia, aes(label = text), 
               color = "#363737", size = 2.5, family = font_choose_map) +
  geom_sf_text(data = data_text_tropics, aes(label = text), hjust = 1,
               color = "#363737", size = 2.5, family = font_choose_map, fontface = "italic") +
  geom_sf_text(data = data_text_pacific, aes(label = text), 
               color = "#1e517b", fontface = "italic", size = 3, family = font_choose_map) +
  # Graphical aspects
  coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE) +
  scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120)) +
  theme_map() +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(linewidth = 1)))

## 3.11 Save the plot ----

ggsave(filename = "figs/01_part-1/fig-6.png", width = 8, height = 5, dpi = 600)

# 4. Comparison of cyclones occurrence ----

## 4.1 Transform data ----

data_cyclones <- data_cyclones %>% 
  st_drop_geometry() %>% 
  group_by(territory, saffir) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(territory) %>% 
  mutate(n_tot = sum(n)) %>% 
  ungroup() %>% 
  bind_rows(., tibble(territory = setdiff(data_eez$TERRITORY1, data_cyclones$territory),
                      n = rep(0, length(setdiff(data_eez$TERRITORY1, data_cyclones$territory))),
                      n_tot = n)) %>% 
  filter(territory != "Matthew and Hunter Islands")

## 4.2 Make the plot ----

ggplot(data = data_cyclones, aes(x = n, y = fct_reorder(territory, n_tot), fill = saffir)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                     labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                     values = c(palette_5cols[2:5], "black"),
                     name = "Saffir-Simpson category",
                     drop = FALSE) +
  theme_graph() +
  theme(legend.position = c(0.60, 0.25),
        legend.direction = "vertical",
        legend.background = element_blank()) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(0, 200)) +
  labs(x = "Number of cyclones", y = NULL)
  
## 4.3 Save the plot ----

ggsave(filename = "figs/01_part-1/fig-7.png", width = 6, height = 8, dpi = 600)
