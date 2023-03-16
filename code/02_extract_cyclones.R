# 1. Load packages ----

library(tidyverse)
library(lubridate)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

# 2.1 Coral reef distribution --

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 2.2 Coral reef distribution buffer --

data_reef_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 2.3 Cyclones lines --

load("data/05_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326)

# 2.4 Cyclones points --

load("data/05_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326)

# 2.5 EEZ --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3. Attribute a EEZ name to reef buffer ----

data_reef_buffer <- st_intersection(data_eez, data_reef_buffer)

# 4. Visual check ----

ggplot() +
  geom_sf(data = data_eez, col = "blue") +
  geom_sf(data = data_reef_buffer, col = "red") +
  geom_sf(data = data_ts_lines, alpha = 0.25)

# 5. Extract cyclones that crosses coral reef buffer ----

# 5.1 Create the function --

map_cyclone <- function(territory_i){
  
  # 1. Extract the EEZ i ----
  
  data_reef_buffer_i <- data_reef_buffer %>% 
    filter(TERRITORY1 == territory_i)
  
  # 2. Filter TS lines crossing the coral reef distribution 100 km buffer ----
  
  data_ts_lines_i <- st_filter(data_ts_lines, data_reef_buffer_i, join = st_intersects) %>% 
    st_drop_geometry() %>% 
    mutate(TERRITORY1 = territory_i)
  
  # 3. Return the results ----
  
  return(data_ts_lines_i)
  
}

# 5.2 Map over the function --

data_ts_reef <- map_dfr(data_eez$TERRITORY1, ~map_cyclone(.))

# 6. Extract information's for each cyclone ----

# 6.1 Create the function --

map_event <- function(territory_i, ts_id_i){
  
  # 1. Extract data ----
  
  data_ts_lines_i <- data_ts_lines %>% 
    filter(ts_id == ts_id_i)
  
  data_ts_points_i <- data_ts_points %>% 
    filter(ts_id == ts_id_i)
  
  data_reef_i <- data_reef %>% 
    filter(TERRITORY1 == territory_i)
  
  # 2. Extract information's ----
  
  results <- st_join(data_reef_i, data_ts_points_i, join = st_nearest_feature) %>% # TS speed and wind at nearest point
    st_drop_geometry() %>% 
    mutate(ts_dist = as.numeric(st_distance(data_reef_i, data_ts_lines_i))) %>% # distance between site and TS path
    select(ts_id, name, time, storm_speed, wind_speed, max_windspeed, saffir, ts_dist) %>% 
    mutate(territory = territory_i,
           ts_dist = ts_dist/1000)
  
  # 3. Return the results ----
  
  return(results)
  
}

# 6.2 Map over the function --

data_cyclones <- pmap_dfr(list(territory_i = data_ts_reef$TERRITORY1, 
                         ts_id_i = data_ts_reef$ts_id),
                    map_event)

# 7. Save the data ----

save(data_cyclones, file = "data/05_cyclones/02_cyclones_extracted.RData")
