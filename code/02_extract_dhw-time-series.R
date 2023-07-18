# 1. Required packages ----

library(terra)
library(tidyverse) # Core tidyverse packages
library(lubridate)
library(sf)
sf_use_s2(FALSE)
library(future)
library(furrr)

plan(multisession, workers = 6) # Set parallelization with 6 cores

# 2. Required functions ----

ncdf_extract <- function(ncdf_i){
  
  ncdf <- rast(ncdf_i)$degree_heating_week

  dhw_i <- terra::extract(x = ncdf, y = data_reef, fun = max, na.rm = TRUE) %>% 
    mutate(date = unique(time(ncdf)))
  
  return(dhw_i)
  
}

# 3. List of NetCDF4 files ----

ncdf_files <- list.files("data/09_dhw/", full.names = TRUE)  

# 4. File of EEZ ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp")

# 6. Extract the data for each NCDF file ----

data_dhw <- future_map_dfr(ncdf_files[1:20], ~ncdf_extract(.)) %>% 
  rename(dhw = degree_heating_week) %>% 
  left_join(., data_reef %>% 
              st_drop_geometry() %>% 
              select(GEONAME) %>%
              mutate(ID = row_number())) %>% 
  select(-ID)

# 7. Export the data ----

save(data_dhw, file = "data/09_data_dhw.RData")
