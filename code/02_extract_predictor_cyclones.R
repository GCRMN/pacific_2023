# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Load data ----

# 2.1 Site coordinates --

data_sites <- st_read("data/15_site-coords/site-coords_obs.shp") %>% 
  st_transform(crs = 4326)

# 2.2 Cyclones lines --

load("data/05_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326)

# 2.3 Cyclones points --

load("data/05_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326)

# 3. Create a function to extract cyclone within 100 km from sites ----

extract_ts_sites <- function(site_id_i){
  
  data_sites_i <- data_sites %>% 
    filter(site_id == site_id_i)
  
  data_results <- data_sites_i %>%  
    # Create the buffer around the site (1 degree ~ 111 km)
    st_buffer(dist = 1) %>% 
    st_wrap_dateline() %>% 
    st_make_valid() %>% 
    # Extract cyclone passing within 111 km from the site
    st_intersection(., data_ts_points) %>% 
    st_filter(., data_ts_points, join = st_filter) %>% 
    # Extract distance of cyclone position from the site
    mutate(dist = as.numeric(st_distance(data_sites_i, .))/1000) %>% 
    # Filter cyclone position within 100 km and minimal distance from the site
    group_by(ts_id) %>% 
    filter(dist <= 100) %>% 
    filter(dist == min(dist)) %>% 
    filter(wind_speed == max(wind_speed)) %>% 
    ungroup()
  
  return(data_results)
  
}

# 4. Map over the function ----

pred_cyclones <- map_dfr(data_sites$site_id, ~extract_ts_sites(site_id_i = .))

# 5. Export the results ----

save(pred_cyclones, file = "data/14_predictors/pred_cyclones.RData")
