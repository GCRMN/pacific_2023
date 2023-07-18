# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(purrr)
library(lubridate)
library(RcppRoll)
library(sf)
sf_use_s2(FALSE)

# 2. Load EEZ data ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3 List path of shapefiles ----

list_shp <- list.files(path = "data/01_background-shp/01_ne/ne_10m_bathymetry_all/", pattern = ".shp", full.names = TRUE)

# 4 Combine shapefiles ----

data_bathy <- map_dfr(list_shp, ~st_read(., quiet = TRUE)) %>% 
  mutate(fill_color = case_when(depth == 0 ~ "#e1f5fe",
                                depth == 200 ~ "#b3e5fc",
                                depth == 1000 ~ "#81d4fa",
                                depth == 2000 ~ "#4fc3f7",
                                depth == 3000 ~ "#29b6f6",
                                depth == 4000 ~ "#03a9f4",
                                depth == 5000 ~ "#039be5",
                                depth == 6000 ~ "#0288d1",
                                depth == 7000 ~ "#0288d1",
                                depth == 8000 ~ "#0277bd",
                                depth == 9000 ~ "#01579b",
                                depth == 10000 ~ "black")) %>% 
  mutate(color = fct_reorder(fill_color, depth)) %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 5. Select bathymetry only for selected EEZ ----

data_bathy <- st_intersection(data_bathy, data_eez) %>% 
  select(TERRITORY1, depth, fill_color)

# 6. Export the data ----

save(data_bathy, file = "data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData") # RData
