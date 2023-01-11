# 1. Load packages ----

library(tidyverse)
library(sf)

# 2. Source functions ----

source("code/function/export_tabular.R")

# 2. Maritime area ----

load("data/02_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2)

# 3. Export 

map(unique(data_eez$TERRITORY1), ~export_tabular(territory_i = .))

