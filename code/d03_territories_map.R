# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(s2)
library(sf)
sf_use_s2(TRUE)
library(ggspatial) # For annotation_scale function

# 2. Load functions ----

source("code/function/graphical_par.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez_supp <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(GEONAME == "Overlapping claim Matthew and Hunter Islands: France / Vanuatu") %>% 
  st_transform(crs = 4326) %>% 
  nngeo::st_remove_holes(.) %>% 
  st_transform(crs = "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

data_eez <- bind_rows(data_eez, data_eez_supp)

# 4. Create the hemisphere individual maps ----

# 4.1 Load and transform data --

g <- as_s2_geography(TRUE)
co <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")
oc <- s2_difference(g, s2_union_agg(co)) # oceans
b <- s2_buffer_cells(as_s2_geography("POINT(-175 0)"), 9800000) # visible half
i <- s2_intersection(b, oc) # visible ocean

# 4.2 Transform CRS --

i <- i %>% 
  st_as_sfc() %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

b <- b %>% 
  st_as_sfc() %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

land_eez <- st_intersection(data_eez %>% st_transform(crs = 4326), co %>% st_transform(crs = 4326)) %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

data_graticules <- st_read("data/01_background-shp/01_ne/ne_10m_graticules_15/ne_10m_graticules_15.shp")

data_graticules <- st_intersection(data_graticules %>% st_transform(crs = 4326),
                                   i %>% st_transform(crs = 4326) %>% st_make_valid()) %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

# 4.3 Change EEZ levels for Kiribati and Pacific Remote Island Area -- 

data_eez <- data_eez %>% 
  # Kiribati
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Phoenix Group" = "Kiribati",
                                                    "Gilbert Islands" = "Kiribati",
                                                    "Line Group" = "Kiribati"))) %>% 
  # Pacific Remote Island Area
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Jarvis Island" = "pria",
                                                    "Wake Island" = "pria",
                                                    "Howland and Baker Islands" = "pria",
                                                    "Johnston Atoll" = "pria",
                                                    "Palmyra Atoll" = "pria")))

# 4.4 Create the function --

map_sphere <- function(territory_i){
  
  data_eez_i <- data_eez %>% 
    filter(TERRITORY1 == territory_i) %>% 
    st_as_sfc() %>% 
    st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")
  
  data_eez_all <- data_eez %>% 
    filter(TERRITORY1 != territory_i) %>% 
    st_as_sfc() %>% 
    st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")
  
  ggplot() +
    geom_sf(data = b, fill = "#363737", col = "grey") +
    geom_sf(data = i, fill = "#ebf5fd") +
    geom_sf(data = data_eez %>% filter(TERRITORY1 == "Matthew and Hunter Islands"),
            color = "#5c97bf", fill = "#ebf5fd", alpha = 0.75) +
    geom_sf(data = data_eez %>% filter(TERRITORY1 != "Matthew and Hunter Islands"),
            color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
    geom_sf(data = data_eez_i, color = palette_second[4], fill = palette_second[3], alpha = 0.75) +
    geom_sf(data = land_eez, fill = "#363737", col = "grey") +
    theme_minimal()
  
  ggsave(filename = paste0("figs/02_part-2/fig-1/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 4, height = 4, dpi = 600)
  
}

# 4.5 Map over the function --

map(setdiff(unique(data_eez$TERRITORY1), "Matthew and Hunter Islands"), ~map_sphere(territory_i = .))
