# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

data_eez <- st_read("data/01_background-shp/03_eez/data_eez.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = "WRAPDATELINE=YES") %>% 
  st_make_valid()

# 3. Transform data ----

data_obs <- st_read("data/04_site-coords/site-coords_obs.shp") %>% 
  st_intersection(., data_eez) %>% 
  select(TERRITORY1) %>% 
  mutate(type = "obs",
         site_id = row_number()) %>% 
  rename(territory = TERRITORY1)

data_pred <- st_read("data/04_site-coords/site-coords_pred.shp") %>% 
  st_intersection(., data_eez) %>% 
  select(TERRITORY1) %>% 
  mutate(type = "pred",
         site_id = row_number()) %>% 
  rename(territory = TERRITORY1)

# 4. Bind data and export ----

bind_rows(data_obs, data_pred) %>% 
  st_write(., dsn = "data/04_site-coords/site-coords_all.shp", delete_dsn = TRUE)
