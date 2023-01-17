# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

# 2.1 Economic Exclusive Zones (EEZ) --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>% 
  st_make_valid()

ggplot() +
  geom_sf(data = data_eez) # Visual check

# 2.2 Reef distribution --

data_reef <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>% 
  st_make_valid()
  
ggplot() +
  geom_sf(data = data_reef) # Visual check

# 3. Make the intersection ---- 

data_reef <- st_intersection(data_reef, data_eez)

ggplot() +
  geom_sf(data = data_reef) # Visual check

# 4. Export the data ----

st_write(data_reef, "data/03_reefs-area_wri/clean/pacific_reef.shp") # Export for Earth Engine
