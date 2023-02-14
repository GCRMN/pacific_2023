# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(sf)
library(ggspatial) # For annotation_scale function
library(s2)
sf_use_s2()

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/map_eez.R")
source("code/function/map_ortho.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_parameters <- read.csv2("data/path_individual_maps.csv")

data_benthic <- load("C:/Users/jwicquart/Desktop/Recherche/03_projects/2022-02-10_gcrmndb_benthos/gcrmndb_benthos/data/09_gcrmndb_benthos.RData")

data_benthic <- synthetic_data %>% 
  filter(higherGeography == "Pacific") %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 4. Create the EEZ individual maps ----

map(unique(data_parameters$TERRITORY1), ~map_eez(territory_i = ., data_parameters = data_parameters))

# 5. Create the hemisphere individual maps ----

# 5.1 Load and transform data --

g <- as_s2_geography(TRUE)
co <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")
oc <- s2_difference(g, s2_union_agg(co)) # oceans
b <- s2_buffer_cells(as_s2_geography("POINT(-175 0)"), 9800000) # visible half
i <- s2_intersection(b, oc) # visible ocean

load("data/01_background-shp/03_eez/data_eez.RData")

# 5.2 Transform CRS --

i <- i %>% 
  st_as_sfc() %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

b <- b %>% 
  st_as_sfc() %>% 
  st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")

# 5.3 Produce all the maps --

map(unique(data_eez$TERRITORY1), ~map_ortho(territory_i = .))
