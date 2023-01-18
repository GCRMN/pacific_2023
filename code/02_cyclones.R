# 1. Load packages ----

library(tidyverse)
library(lubridate)
library(sf)
sf_use_s2(FALSE)
library(future) # Necessary for furrr package
library(furrr) # For parallelization using purrr function family
plan(multisession, workers = 6) # Set parallelization with 6 cores

# 2. Load functions ----

source("code/function/extract_ts_eez.R")
source("code/function/extract_ts_event.R")
source("code/function/map_cyclone.R")
source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# Set the CRS

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 3. Load data ----

# 3.1 Coral reef distribution --

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  st_transform(crs = crs_selected)

# 3.2 Coral reef distribution buffer --

data_reef_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = crs_selected)

# 3.3 Cyclones lines --

load("data/05_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = crs_selected)

# 3.4 Cyclones points --

load("data/05_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = crs_selected)

# 3.5 EEZ --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

# 4. Attribute a EEZ name to reef buffer ----

data_reef_buffer <- st_intersection(data_eez, data_reef_buffer)

# 5. Extract TS event for each EEZ ----

data_ts_event <- future_map_dfr(unique(data_eez$TERRITORY1), 
                                ~extract_ts_eez(territory_i = .), 
                                .options = furrr_options(seed = TRUE))

# 6. Modify data ----

data_ts_event <- data_ts_event %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, ts_id, name, time, storm_speed, wind_speed, max_windspeed, saffir, ts_dist) %>% 
  mutate(time = as_date(time),
         ts_dist = ts_dist/1000,
         ts_dist = paste0(round(ts_dist, 0), " km")) %>% 
  filter(wind_speed >= 100)

# 7. Create the EEZ individual maps ----

map(unique(data_ts_event$TERRITORY1), ~map_cyclone(territory_i = .))
