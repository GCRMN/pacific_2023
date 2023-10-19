# 1. Required packages ----

library(terra)
library(tidyverse) # Core tidyverse packages
library(lubridate)
library(sf)
sf_use_s2(FALSE)
library(future)
library(furrr)

plan(multisession, workers = 6) # Set parallelization with 6 cores

# 2. List of NetCDF4 files ----

ncdf_files <- list.files(path = "F:/Recherche/03_projects/2020-07-30_disturbance/disturbance/data/011_sst_raw",
                         pattern = "\\.nc$", full.names = TRUE)

# 3. Check if files are missing ----

real_files_list <- str_remove_all(str_split_fixed(ncdf_files, "_", n = 5)[,5], "\\.nc")

theoric_files_list <- str_remove_all(seq(as.Date("1985-01-01"), as.Date("2020-12-31"), by = "days"), "-")

setdiff(theoric_files_list, real_files_list)

rm(theoric_files_list, real_files_list)

# 4. File of EEZ ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp")

# 5. Create function to extract SST ----

ncdf_extract <- function(ncdf_i){
  
  ncdf <- rast(ncdf_i)
  
  my_summary <- function(x) c(mean = mean(x, na.rm = TRUE),
                              min = min(x, na.rm = TRUE),
                              max = max(x, na.rm = TRUE),
                              sd = sd(x, na.rm = TRUE))
  
  sst_i <- terra::extract(x = ncdf, y = data_reef, fun = my_summary) %>% 
    select(-starts_with("sea")) %>% 
    mutate(date = unique(time(ncdf)))
  
  return(sst_i)
  
}

# 6. Map over the function ----

data_sst <- future_map_dfr(ncdf_files[1:10], ~ncdf_extract(.)) %>% 
  rename(sst = analysed_sst) %>% 
  left_join(., data_reef %>% 
              st_drop_geometry() %>% 
              select(GEONAME) %>%
              mutate(ID = row_number())) %>% 
  select(-ID)

# 7. Export the data ----

save(data_sst, file = "data/07_data_sst.RData")
