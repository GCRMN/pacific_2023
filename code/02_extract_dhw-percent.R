# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(tidyterra)

# 2. Get NCDF files ----

ncdf_files <- list.files("data/09_dhw/", full.names = TRUE)

# 3. Load EEZ data ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  select(TERRITORY1) %>% 
  st_transform(crs = 4326) %>% 
  terra::vect(.)

# Visual Check
#plot(rast(ncdf_files[1])$degree_heating_week)
#plot(data_reef, add = TRUE)

# 4. Create a function to extract values of cells ----

extract_dhw_percent <- function(ncdf_i){
  
  ncdf <- rast(ncdf_i)$degree_heating_week
  
  crs(ncdf) <- "epsg:4326"
  
  results <- terra::freq(x = ncdf, zones = data_reef) %>% 
    mutate(date = lubridate::date(unique(time(ncdf))))

  return(results)
  
}

# 5. Map over the function ----

data_dhw_percent <- map_dfr(ncdf_files[1:50], ~extract_dhw_percent(.)) %>% 
  rename(dhw = value) %>% 
  left_join(., tibble(territory = data_reef$TERRITORY1) %>%
              mutate(zone = row_number())) %>% 
  select(-layer, -zone) %>% 
  tidyr::complete(date, dhw, territory, fill = list(count = 0)) %>% 
  group_by(territory, date) %>% 
  mutate(freq = count*100/sum(count)) %>% 
  ungroup()

# 6. Export the data ----

save(data_dhw_percent, file = "data/10_data-dhw-percent.RData")
