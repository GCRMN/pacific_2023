# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/export_geoinf.R")

# 3. Maritime area ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_maritime_area <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2) %>% 
  group_by(TERRITORY1) %>% 
  summarise(AREA_KM2 = sum(AREA_KM2)) %>% 
  ungroup() %>% 
  mutate(AREA_KM2 = format(AREA_KM2, big.mark = ","))

# 4. Reef area ----

data_reefs <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

ggplot() +
  geom_sf(data = data_eez) +
  geom_sf(data = data_reefs, color = "red")

data_reef_area <- st_intersection(data_eez, data_reefs) %>%  
  group_by(TERRITORY1) %>% 
  summarise(area = sum(st_area(geometry))) %>% 
  st_drop_geometry() %>% 
  mutate(area = area*1e-6,
         area = as.numeric(area),
         area = round(area, 0),
         area = format(area, big.mark = ","))

# 5. Human population ----

data_population <- read.csv("data/02_geo-inf/01_human-pop.csv") %>% 
  rename(population = sum) %>% 
  group_by(TERRITORY1) %>% 
  summarise(population = sum(population)) %>% 
  ungroup() %>% 
  mutate(population = round(population, 0),
         population = format(population, big.mark = ",", scientific = FALSE))

# 6. Export ---- 

map(unique(data_eez$TERRITORY1), ~export_geoinf(territory_i = .))














library(tidyverse)
A <- read_csv("data/reefsurface_by_site.csv") %>% 
  select(GEONAME, sum)






