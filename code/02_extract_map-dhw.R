# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
library(tidyterra)

# 1. Get NCDF files ----

ncdf_files <- list.files("data/09_dhw/", full.names = TRUE) %>% 
  as_tibble() %>% 
  mutate(year = str_sub(value, -11, -8))

# 2. Create function to get max DHW over a year ----

raster_max_dhw <- function(year_i){
  
  ncdf_files_i <- ncdf_files %>% 
    filter(year == year_i)
  
  data_raster <- rast(ncdf_files_i$value)
  
  data_raster <- max(data_raster)
  
  writeRaster(x = data_raster, filename = paste0("data/11_max-dhw/raster_", year_i, ".tif"), overwrite = TRUE)
  
}

# 3. Map over the function ----

map(unique(ncdf_files$year), ~raster_max_dhw(.))

raster_max_dhw(2009)
