# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

## 2.1 EEZ ----

data_eez <- st_read("data/01_background-shp/03_eez/data_eez.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>% 
  st_make_valid()

## 2.2 Coral reefs distribution ----

data_reef <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>% 
  st_make_valid()

# 3. Extract sites with coral reefs within the Pacific ----

sites_coordinates <- st_intersection(data_reef, data_eez) %>% 
  select(TERRITORY1) %>% 
  rename(territory = TERRITORY1) %>% 
  st_cast(., "MULTIPOLYGON") %>% 
  st_cast(., "MULTIPOINT") %>% 
  st_cast(., "POINT") %>% 
  mutate(type = "pred",
         site_id = row_number())

# 4. Data visualization ----

ggplot() +
  geom_sf(data = sites_coordinates)

# 5. Join data with site coordinates with observed data ----

st_read("data/04_site-coords/site-coords_obs.shp") %>% 
  mutate(type = "obs") %>% 
  bind_rows(., sites_coordinates) %>% 
  st_write(., dsn = "data/04_site-coords/site-coords_all.shp", delete_dsn = TRUE)
