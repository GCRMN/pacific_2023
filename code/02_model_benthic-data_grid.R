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

# 3. Create the grid ----

data_grid <- st_make_grid(data_reefs, cellsize = 1) %>% # Create grid
  st_as_sf() %>% 
  st_filter(., data_reefs) %>%  # Filter grid tiles containing coral reefs
  st_intersection(., data_eez) %>% # Add TERRITORY1
  mutate(grid_id = row_number()) %>% # Create ID for each tile
  select(grid_id, TERRITORY1)

# 4. Calculate coral reef extent for each tile ----

data_area <- st_intersection(data_reefs, data_grid) %>% 
  mutate(area = st_area(., by_element = TRUE),
         area = as.numeric(area)*1e-6, # Convert from m² to km²
         weight = (area*100/sum(area))/100) %>% 
  select(TERRITORY1, grid_id, area, weight) %>% 
  st_drop_geometry()

# 5. Join area and geometry ----

data_grid <- left_join(data_grid, data_area)

# 6. Save the data ----

save(data_grid, file = "data/12_grid-coral-reef-extent.RData")
