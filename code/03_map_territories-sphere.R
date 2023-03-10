# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(sf)
library(ggspatial) # For annotation_scale function
library(s2)
sf_use_s2()

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/map_sphere.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")

# 4. Create the hemisphere individual maps ----

# 4.1 Load and transform data --

g <- as_s2_geography(TRUE)
co <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")
oc <- s2_difference(g, s2_union_agg(co)) # oceans
b <- s2_buffer_cells(as_s2_geography("POINT(-175 0)"), 9800000) # visible half
i <- s2_intersection(b, oc) # visible ocean

load("data/01_background-shp/03_eez/data_eez.RData")

# 4.2 Transform CRS --

i <- i %>% 
  st_as_sfc() %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

b <- b %>% 
  st_as_sfc() %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

# 4.3 Change EEZ levels for Kiribati and Pacific Remote Island Area -- 

data_eez <- data_eez %>% 
  # Kiribati
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Phoenix Group" = "Kiribati",
                                                    "Gilbert Islands" = "Kiribati",
                                                    "Line Group" = "Kiribati"))) %>% 
  # Pacific Remote Island Area
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Jarvis Island" = "Pacific Remote Island",
                                                     "Wake Island" = "Pacific Remote Island",
                                                     "Howland and Baker islands" = "Pacific Remote Island",
                                                     "Johnston Atoll" = "Pacific Remote Island",
                                                     "Palmyra Atoll" = "Pacific Remote Island")))

# 4.4 Produce all the maps --

map(unique(data_eez$TERRITORY1), ~map_sphere(territory_i = .))
