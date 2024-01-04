# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Define CRS ----

# 3.1 Change CRS --

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

# 4. Load data ----

# 4.1 EEZ --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

data_eez_supp_a <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(TERRITORY1 == "Australia") %>% 
  st_transform(crs = 4326) %>% 
  nngeo::st_remove_holes(.) %>% 
  st_cast(., "POLYGON") %>% 
  mutate(row = 1:8) %>% 
  filter(row == 1)

data_eez_supp <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(TERRITORY1 %in% c("Indonesia", "Japan", "Philippines") | 
           GEONAME == "Overlapping claim Matthew and Hunter Islands: France / Vanuatu") %>% 
  st_transform(crs = 4326) %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(., data_eez_supp_a) %>% 
  st_transform(crs = crs_selected) 

# 4.2 Land --

load("data/01_background-shp/02_princeton/data_land.RData")

data_land <- data_land %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

list_shp <- list.files(path = "data/01_background-shp/04_princeton_additional",
                       pattern = ".shp$", full.names = TRUE, recursive = TRUE)

data_land_supp <- map_dfr(list_shp, ~st_read(.)) %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

rm(list_shp, data_eez_supp_a)

# 4.3 Regional capital --

data_place <- st_read("data/01_background-shp/01_ne/ne_10m_populated_places/ne_10m_populated_places.shp") %>% 
  st_transform(crs = crs_selected) %>% 
  st_filter(., data_eez) %>%  
  filter(FEATURECLA %in% c("Admin-0 region capital", "Admin-0 capital", "Admin-1 capital")) %>% 
  mutate(ADM0NAME = ifelse(ADM0NAME == "United States of America", ADM1NAME, ADM0NAME)) %>% 
  filter(!(ADM0NAME == "Papua New Guinea" & FEATURECLA == "Admin-1 capital"),
         !(ADM0NAME == "Palau" & FEATURECLA == "Admin-1 capital"))

# 4.4 Bathymetry --

load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")

data_bathy <- data_bathy %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

map_eez <- function(territory){
  
  # 1. Filter ----
  
  data_eez_i <- data_eez %>% 
    filter(TERRITORY1 == territory)
  
  data_place_i <- data_place %>% 
    filter(ADM0NAME == territory)
  
  # 2. Create the bbox ----
  
  x_min <- st_bbox(data_eez_i)["xmin"]
  x_max <- st_bbox(data_eez_i)["xmax"]
  y_min <- st_bbox(data_eez_i)["ymin"]
  y_max <- st_bbox(data_eez_i)["ymax"]
  
  percent_margin_ltr <- 10 # Margin in percentage for left, top, and right of plot
  percent_margin_b <- 20 # Margin in percentage for bottom of plot
  
  # 3. White polygon to put scale on ----
  
  poly_scale <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                               x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                       lat = c(y_min - ((y_max - y_min)*percent_margin_b/100),
                               y_min - ((y_max - y_min)*(percent_margin_b/1.5)/100))) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  # 4. Define plot limits with additional margins ----
  
  data_bbox <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                              x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                      lat = c(y_min - ((y_max - y_min)*percent_margin_b/100),
                              y_max + ((y_max - y_min)*percent_margin_ltr/100))) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  # 5. Layer to mask external zone of eez_i ----
  
  data_alpha <- st_difference(data_bbox, data_eez_i)
  
  # 6. Make the plot ----
  
  plot_i <- ggplot() +
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
    geom_sf(data = data_eez, color = "black", fill = NA) +
    geom_sf(data = data_eez_supp, color = "black", fill = NA) +
    geom_sf(data = data_land, fill = "#363737", col = "grey") +
    geom_sf(data = data_land_supp, fill = "#363737", col = "grey") +
    geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
    geom_sf(data = poly_scale, fill = "white", col = "white") +
    geom_sf(data = data_place_i, fill = "#d64541", color = "white", shape = 23, size = 2) +
    coord_sf(xlim = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                      x_max + ((x_max - x_min)*percent_margin_ltr/100)),
             ylim = c(y_min - ((y_max - y_min)*percent_margin_b/100),
                      y_max + ((y_max - y_min)*percent_margin_ltr/100)),
             expand = FALSE) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank()) +
    annotation_scale(location = "br", width_hint = 0.25, text_family = font_choose_graph, 
                     text_cex = 0.7, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))
  
  # 7. Export the plot ----
  
  ggsave(filename = paste0("figs/territories_fig-2/",
                           str_replace_all(str_to_lower(territory), " ", "-"), ".png"),
         plot = plot_i, dpi = 600)
  
}

# 5. Map over the function (except PRIA) ----

map(setdiff(unique(data_eez$TERRITORY1),
            c("Palmyra Atoll", "Johnston Atoll",
              "Wake Island", "Jarvis Island",
              "Howland and Baker Islands")), # PRIA territories
    ~map_eez(territory = .))

# 6. Map for Pacific Remote Islands Area (PRIA) ----

# 6.1 Filter --

data_eez_i <- data_eez %>% 
  filter(TERRITORY1 %in% c("Palmyra Atoll", "Johnston Atoll",
                           "Wake Island", "Jarvis Island",
                           "Howland and Baker Islands"))

# 6.2 Create the bbox --

x_min <- st_bbox(data_eez_i)["xmin"]
x_max <- st_bbox(data_eez_i)["xmax"]
y_min <- st_bbox(data_eez_i)["ymin"]
y_max <- st_bbox(data_eez_i)["ymax"]

percent_margin_ltr <- 10 # Margin in percentage for left, top, and right of plot
percent_margin_b <- 20 # Margin in percentage for bottom of plot

# 6.3 White polygon to put scale on --

poly_scale <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                             x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                     lat = c(y_min - ((y_max - y_min)*percent_margin_b/100),
                             y_min - ((y_max - y_min)*(percent_margin_b/1.5)/100))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
  st_bbox() %>% 
  st_as_sfc()

# 6.4 Define plot limits with additional margins --

data_bbox <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                            x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                    lat = c(y_min - ((y_max - y_min)*percent_margin_b/100),
                            y_max + ((y_max - y_min)*percent_margin_ltr/100))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
  st_bbox() %>% 
  st_as_sfc()

# 6.5 Layer to mask external zone of eez_i --

data_alpha <- data_eez_i %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_difference(data_bbox, .)

# 6.6 Make the plot --

plot_i <- ggplot() +
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
  geom_sf(data = data_eez, color = "black", fill = NA) +
  geom_sf(data = data_land, fill = "#363737", col = "grey") +
  geom_sf(data = data_bbox, fill = "white", alpha = 0.5) +
  geom_sf(data = data_eez_i, color = "black", fill = NA) +
  geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
  geom_sf(data = poly_scale, fill = "white", col = "white") +
  coord_sf(xlim = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                    x_max + ((x_max - x_min)*percent_margin_ltr/100)),
           ylim = c(y_min - ((y_max - y_min)*percent_margin_b/100),
                    y_max + ((y_max - y_min)*percent_margin_ltr/100)),
           expand = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank()) +
  annotation_scale(location = "br", width_hint = 0.25, text_family = font_choose_graph, 
                   text_cex = 0.7, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))

# 6.7 Export the plot --

ggsave(filename = paste0("figs/territories_fig-2/pria.png"),
       plot = plot_i, dpi = 600)
