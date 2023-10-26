# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

data_reefs <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp")

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

site_coords <- st_read("data/15_benthic-site-coords/benthic-site-coords.shp")

# 3. Create the grid ----

data_grid <- st_make_grid(data_reefs, cellsize = 0.5) %>% # Create grid
  st_as_sf() %>% 
  st_intersection(., data_eez) %>% # Add TERRITORY1
  mutate(grid_id = row_number()) %>% # Create ID for each tile
  select(grid_id, TERRITORY1)

# 4. Calculate coral reef extent for each tile ----

data_area <- st_intersection(data_reefs, data_grid) %>% 
  mutate(area = st_area(., by_element = TRUE),
         area = as.numeric(area)*1e-6, # Convert from m² to km²
         total_area = sum(area),
         weight = (area*100)/total_area) %>% 
  select(TERRITORY1, grid_id, area, weight) %>% 
  st_drop_geometry()

# 5. Join area and geometry ----

data_grid <- left_join(data_grid, data_area) %>% 
  drop_na(weight) %>% # Remove tiles without coral reefs
  st_as_sf()

# 6. Associate grid weight to each site ----

site_coords_weight <- st_intersection(site_coords, data_grid) %>% 
  select(weight) %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

site_coords_no_weight <- site_coords %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry() %>% 
  select(decimalLongitude, decimalLatitude)

data_weight <- left_join(site_coords_no_weight, site_coords_weight) %>% 
  # Correct sites falling at the intersection of two tiles (resulting in two weight for a same site)
  mutate(n = 1) %>% 
  group_by(decimalLongitude, decimalLatitude) %>% 
  mutate(n = sum(n),
         weight = if_else(n > 1, max(weight), weight)) %>% 
  ungroup() %>% 
  select(-n) %>% 
  distinct() %>% 
  # Add weight for sites outside grid tiles
  mutate(weight = if_else(is.na(weight), min(weight, na.rm = TRUE), weight))

# 7. Save the data ----

save(data_weight, file = "data/12_weight-model-benthic-cover.RData")
