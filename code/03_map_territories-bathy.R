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

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_parameters <- read.csv2("data/path_individual_maps.csv")

load("data/04_data-benthic.RData")

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, territory) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 4. Create the EEZ individual maps ----

map(unique(data_parameters$TERRITORY1), ~map_eez(territory_i = ., data_parameters = data_parameters))
