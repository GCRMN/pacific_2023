# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

# 3.1 Princeton land --

list_shp <- list.files(path = "data/01_background-shp/02_princeton",
                       pattern = ".shp$", full.names = TRUE, recursive = TRUE)

data_land <- map_dfr(list_shp, ~st_read(.)) %>% 
  st_transform(crs = 4326)

rm(list_shp)

# 3.2 EEZ --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 4. Add PNG (land part doesn't work properly with st_intersection)

data_png <- data_land %>% 
  filter(ISO == "PNG") %>% 
  mutate(TERRITORY1 = "Papua New Guinea",
         SOVEREIGN1 = "Papua New Guinea") %>% 
  select(TERRITORY1, SOVEREIGN1)

# 5. Make the intersection ----

data_land <- data_land %>% 
  filter(ISO != "PNG") %>% 
  st_intersection(data_eez, .) %>% 
  select(TERRITORY1, SOVEREIGN1) %>% 
  bind_rows(., data_png)

# 6. Export the data ----

save(data_land, file = "data/01_background-shp/02_princeton/data_land.RData") # RData
