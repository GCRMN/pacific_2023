# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(tidyterra)

# 2. Benthic cover sites ----

# 2.1 Import site coordinates --

data_benthic_sites <- st_read("data/15_benthic-site-coords/benthic-site-coords.shp") %>% 
  select(-FID) %>% 
  mutate(site_id = row_number())

# 2.2 Convert to sf and create buffer --

data_benthic_buffer <- data_benthic_sites %>% 
  # Create 50 km buffer around each site
  st_transform(crs = 7801) %>% 
  st_buffer(., dist = 50000) %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3. Coral reef extent data --

data_reefs <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

# 4. Visual check ----

ggplot() +
  geom_sf(data = data_reefs) +
  geom_sf(data = data_benthic_buffer, col = "red")

# 5. Extract coral reef extent within 50 km from the site ----

# 5.1 Create a function --

extract_reef_area <- function(site_id_i){
  
  data_benthic_buffer_i <- data_benthic_buffer %>% 
    filter(site_id == site_id_i)
  
  results <- st_intersection(data_benthic_buffer_i, data_reefs) %>% 
    st_area(.) %>% 
    as_tibble() %>% 
    rename(reef_area = 1) %>% 
    mutate(site_id = site_id_i,
           reef_area = as.numeric(reef_area)/1e+6) # Convert from meters to km square
  
  return(results)
  
}

# 5.2 Map over the function --

data_pred_reef <- map_dfr(unique(data_benthic_buffer$site_id), ~extract_reef_area(site_id_i = .)) %>% 
  left_join(data_benthic_sites, .) %>% 
  select(-site_id)

# 5.3 Export the data --

save(data_pred_reef, file = "data/14_predictors/reef_extent.RData")
