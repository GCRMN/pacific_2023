# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(sf)
sf_use_s2()

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

load("data/04_data-benthic.RData")
load("data/01_background-shp/03_eez/data_eez.RData")

# 4. Extract descriptors ----

# 4.1 Number of monitoring sites --

data_sites <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count(name = "n_sites")

# 4.2 Number of surveys --

data_surveys <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year, month, day) %>% 
  distinct() %>% 
  group_by(territory) %>% 
  count(name = "n_surveys")

# 4.3 Number of datasets --

data_datasets <- data_benthic %>% 
  select(territory, datasetID) %>% 
  group_by(territory) %>% 
  distinct() %>% 
  count(name = "n_datasets")

# 4.4 Combine datasets --

data_descriptors <- left_join(data_sites, data_surveys) %>% 
  left_join(., data_datasets)

rm(data_sites, data_surveys, data_datasets)

# 5. Number of surveys per year ----

# 5.1 Select data --

data_benthic <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  distinct()

# 5.2 Make the plot --

ggplot(data = data_benthic, aes(x = year)) +
  # By default its density, width*density*100 gives percentage
  geom_histogram(binwidth = 1, aes(y = after_stat(width*density*100)),
                 color = "black", fill = "#5c97bf") +
  lims(x = c(1970, 2021)) +
  labs(x = "Year", y = "Surveys (%)") +
  theme_graph() +
  theme(plot.title = element_text(size = 20))

# 5.3 Save the plot --

ggsave(filename = "figs/01_surveys-by_year.png", dpi = 600)

# 6. Map of spatio-temporal distribution of monitoring sites ----

# 6.1 Load data --

# 6.1.1 Define the CRS --

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 6.1.2 Define the offset --

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

# 6.1.3 Define a long and slim polygon that overlaps the meridian line --

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# 6.1.4 Load background maps --

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 6.1.5 Economic Exclusive Zones ----

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

# 6.1.6 Country boundaries --

data_countries <- read_sf("data/01_background-shp/01_ne/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs_selected)

# 6.1.7 Create the tropics ----

data_tropics <- tibble(long = c(-180, 180, -180, 180, -180, 180), 
                       lat = c(0, 0, 23.43656, 23.43656, -23.43656, -23.43656), 
                       tropic = c("Equator", "Equator", "Tropic of Cancer", "Tropic of Cancer",
                                  "Tropic of Capricorne", "Tropic of Capricorne")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  group_by(tropic) %>% 
  summarise() %>% 
  st_cast("LINESTRING") %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs = crs_selected)

# 6.1.8 Create text annotation --

# Tropics --

data_text_tropics <- tibble(long = c(-105, -104, -119, -128),
                            lat = c(-21.43, -25.43, 2, 25.43),
                            text = c("Tropic of", "Capricorn", "Equator", "Tropic of Cancer")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# Pacific Ocean --

data_text_pacific <- tibble(long = c(-130),
                            lat = c(13),
                            text = c("Pacific Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# Australia --

data_text_australia <- tibble(long = c(140),
                              lat = c(-25),
                              text = c("AUSTRALIA")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 6.1.9 Select benthic data

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_transform(crs = crs_selected)

# 6.2 Make the plot --

ggplot() +
  # Tropics
  geom_sf(data = data_tropics, linetype = "dashed", color = "#363737", linewidth = 1) +
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
  # Benthic sites
  geom_sf(data = data_benthic, aes(color = interval_class)) +
  scale_color_manual(values = palette_5cols,
                     labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                     drop = FALSE, name = "Number of years with data") +
  # Graphical aspects
  coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE) +
  scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120)) +
  theme(text = element_text(family = font_choose_map),
        panel.background = element_rect(fill = "#ebf5fd"),
        panel.grid = element_blank(),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        axis.title = element_blank(),
        axis.text.x.top = element_text(),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.key = element_blank()) +
  guides(colour = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4)))
  
# 6.3 Save the plot --

ggsave(filename = "figs/01_pacific-map-sites.png", width = 8, height = 5, dpi = 600)
