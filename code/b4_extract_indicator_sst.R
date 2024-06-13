# 1. Required packages ----

library(tidyverse)
library(terra)
library(sf)
sf_use_s2(FALSE)
library(future)
library(furrr)

plan(multisession, workers = 4) # Set parallelization with 4 cores

# 2. Load and transform coral reefs EEZ ----

## 2.1 Load file ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  select(TERRITORY1) %>% 
  mutate(TERRITORY1 = as.character(TERRITORY1))

## 2.2 Add Entire Pacific region ----

data_reef <- data_reef %>% 
  st_union() %>% 
  as_tibble() %>% 
  st_as_sf(crs = 4326) %>% 
  mutate(TERRITORY1 = "Entire Pacific region") %>% 
  bind_rows(data_reef, .)

## 2.3 Add Kiribati ----

data_reef <- data_reef %>% 
  filter(TERRITORY1 %in% c("Gilbert Islands", "Phoenix Group", "Line Group")) %>% 
  st_union() %>% 
  as_tibble() %>% 
  st_as_sf(crs = 4326) %>% 
  mutate(TERRITORY1 = "Kiribati") %>% 
  bind_rows(data_reef, .)

## 2.4 Add PRIA ----

data_reef <- data_reef %>% 
  filter(TERRITORY1 %in% c("Johnston Atoll", "Palmyra Atoll", "Wake Island",
                           "Howland and Baker Islands", "Jarvis Island")) %>% 
  st_union() %>% 
  as_tibble() %>% 
  st_as_sf(crs = 4326) %>% 
  mutate(TERRITORY1 = "Pacific Remote Island Area") %>% 
  bind_rows(data_reef, .)

# 3. List files of SST to download ----

list_url <- data.frame(date = seq(from = ymd("1985-01-01"), to = ymd("2023-12-31"), by = "1 day")) %>% 
  mutate(year = year(date),
         date = str_remove_all(date, "-"),
         url = paste0("https://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst/",
                      year,
                      "/coraltemp_v3.1_",
                      date,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,16])

# 4. Create the function to extract mean SST over territories' coral reefs ----

extract_sst <- function(row_nb, data_reef = data_reef){
  
  # 1. Download file
  
  list_url_i <- list_url %>% 
    filter(row_number(.) == row_nb)
  
  # Use mode "wb" for windows otherwise issue to read the file with terra
  download.file(url = list_url_i[1, "url"],
                destfile = paste0("data/06_sst/", list_url_i[1, "filename"]),
                mode = "wb",
                timeout = max(300, getOption("timeout"))) # 300 seconds to download the file, else error message
  
  # 2. Load the raster
  
  ncdf <- terra::rast(paste0("data/06_sst/", list_url_i[1, "filename"]))$analysed_sst
  
  crs(ncdf) <- "epsg:4326"
  
  # 3. Extract SST
  
  sst_i <- terra::extract(x = ncdf, y = data_reef, fun = mean, na.rm = TRUE) %>% 
    as_tibble() %>% 
    dplyr::select("ID", "analysed_sst") %>% 
    dplyr::mutate(date = unique(time(ncdf)))
  
  # 4. Delete raw file
  
  file.remove(paste0("data/06_sst/", list_url_i[1, "filename"]))
  
  # 5. Return the results
  
  return(sst_i)
  
}

# 5. Map over the function ----

data_sst <- future_map_dfr(1:nrow(list_url), ~extract_sst(row_nb = ., data_reef = data_reef)) %>% 
  rename(sst = analysed_sst) %>% 
  left_join(., data_reef %>% 
              st_drop_geometry() %>% 
              mutate(ID = row_number())) %>% 
  select(-ID)

# 6. Export the data ----

save(data_sst, file = "data/09_misc/data-sst.RData")
