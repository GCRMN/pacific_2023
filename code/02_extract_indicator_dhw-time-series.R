# 1. Required packages ----

library(terra)
library(tidyverse) # Core tidyverse packages
library(lubridate)
library(sf)
sf_use_s2(FALSE)
library(future)
library(furrr)

plan(multisession, workers = 2) # Set parallelization with 2 cores

# 2. Required functions ----

ncdf_extract <- function(ncdf_i, data_reef){
  
  ncdf <- rast(ncdf_i)$degree_heating_week
  
  data_reef <- terra::vect(data_reef)
  
  dhw_i <- terra::extract(x = ncdf, y = data_reef, fun = max, na.rm = TRUE) %>% 
    mutate(date = unique(time(ncdf)))
  
  return(dhw_i)
  
}

# 3. List of NetCDF4 files ----

ncdf_files <- list.files("data/09_dhw/", full.names = TRUE)  

# 4. Load EEZ data ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  select(TERRITORY1) %>% 
  st_transform(crs = 4326)

# 5. Extract the data for each NCDF file ----

data_dhw <- future_map_dfr(ncdf_files, ~ncdf_extract(ncdf_i = ., data_reef = data_reef)) %>% 
  rename(dhw = degree_heating_week) %>% 
  left_join(., tibble(territory = data_reef$TERRITORY1) %>%
              mutate(ID = row_number())) %>% 
  select(-ID)

# 6. Export the data ----

save(data_dhw, file = "data/09_data_dhw.RData")
