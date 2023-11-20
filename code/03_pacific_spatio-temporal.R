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

# 3. Map of spatio-temporal distribution of monitoring sites ----

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

# 3.4. Load background maps --

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 3.5 Economic Exclusive Zones --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

data_eez_disputed <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(GEONAME == "Overlapping claim Matthew and Hunter Islands: France / Vanuatu") %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 3.6 Country boundaries --

data_countries <- read_sf("data/01_background-shp/01_ne/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>% 
  st_transform(crs_selected)

# 3.7 Create the tropics --

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

# 3.8 Create text annotation --

# 3.8.1 Tropics --

data_text_tropics <- tibble(long = c(-105, -104, -119, -128),
                            lat = c(-21.43, -25.43, 2, 25.43),
                            text = c("Tropic of", "Capricorn", "Equator", "Tropic of Cancer")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 3.8.2 Pacific Ocean --

data_text_pacific <- tibble(long = c(-130),
                            lat = c(13),
                            text = c("Pacific Ocean")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 3.8.3 Australia --

data_text_australia <- tibble(long = c(140),
                              lat = c(-25),
                              text = c("AUSTRALIA")) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs_selected)

# 3.9 Select benthic data --

load("data/04_data-benthic.RData")

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

# 3.10 Make the plot --

plot_a <- ggplot() +
  # Tropics
  #geom_sf(data = data_tropics, linetype = "dashed", color = "#363737", linewidth = 0.25) +
  geom_sf(data = data_tropics_no_eez, linetype = "dashed", color = "#363737", linewidth = 0.25) +
  # EEZ
  geom_sf(data = data_eez_disputed, color = "#363737", fill = "grey", alpha = 0.75) +
  geom_sf(data = data_eez, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
  # Background map
  geom_sf(data = data_map, fill = "#363737", col = "grey") +
  # Country boundaries
  geom_sf(data = data_countries, fill = "#363737", col = "grey") +
  # Benthic data
  geom_sf(data = data_benthic %>% arrange(interval_class), aes(color = interval_class)) +
  scale_color_manual(values = palette_5cols,
                     labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                     drop = FALSE, name = "Number of years with data") +
  # Annotation (legend)
  geom_sf_text(data = data_text_australia, aes(label = text), 
               color = "darkgrey", size = 2.5, family = font_choose_map) +
  geom_sf_text(data = data_text_tropics, aes(label = text), hjust = 1,
               color = "#363737", size = 2.5, family = font_choose_map, fontface = "italic") +
  geom_sf_text(data = data_text_pacific, aes(label = text), 
               color = "#5c97bf", fontface = "italic", size = 3, family = font_choose_map) +
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

# 3.11 Save the plot --

ggsave(filename = "figs/01_pacific-map-sites.png", width = 8, height = 5, dpi = 600)

# 4. Plot of percentage of sites per interval_class ----

plot_a <- data_benthic %>% 
  st_drop_geometry() %>% 
  group_by(interval_class) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ggplot(data = ., aes(x = interval_class, y = percent, fill = interval_class)) +
    geom_bar(stat = "identity", color = NA, show.legend = FALSE, width = 0.65) +
    scale_fill_manual(values = palette_5cols,
                      labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                      drop = FALSE, name = "Number of years with data") +
    labs(x = "Duration", y = "Sites (%)", title = "A") +
    lims(y = c(0, 100)) +
    theme_graph() +
    theme(plot.title = element_text(size = 15),
          axis.text.x = element_text(size = 7))

# 5. Plot of number of surveys per year ----

load("data/04_data-benthic.RData")

plot_b <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year) %>% 
  count() %>% 
  ungroup() %>% 
  complete(year, fill = list(n = 0)) %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ggplot(data = ., aes(x = year, y = percent)) +
    geom_bar(stat = "identity", show.legend = FALSE, width = 0.5, color = "#C9504B", fill = "#C9504B") +
    labs(x = "Year", y = "Surveys (%)", title = "B") +
    lims(x = c(1970, 2024)) +
    theme_graph() +
    theme(plot.title = element_text(size = 15),
          axis.text.x = element_text(size = 7))

# 6. Combine plots ----

# 6.1 Horizontal --

plot_a + plot_b + plot_layout(ncol = 2)

ggsave(filename = "figs/01_pacific_surveys_lds.png", width = 10, height = 4, dpi = 600)

# 6.2 Vertical --

plot_a + plot_b + plot_layout(ncol = 1)

ggsave(filename = "figs/01_pacific_surveys_prt.png", width = 5, height = 8, dpi = 600)
