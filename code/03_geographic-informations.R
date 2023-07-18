# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Maritime area ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_maritime_area <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2) %>% 
  group_by(TERRITORY1) %>% 
  summarise(AREA_KM2 = sum(AREA_KM2)) %>% 
  ungroup() %>% 
  mutate(AREA_KM2 = format(AREA_KM2, big.mark = ","))

# 3. Reef area ----

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

# 4. Human population ----

data_population <- read.csv("data/02_geo-inf/01_human-pop.csv") %>% 
  rename(population = sum) %>% 
  group_by(TERRITORY1) %>% 
  summarise(population = sum(population)) %>% 
  ungroup() %>% 
  mutate(population = round(population, 0),
         population = format(population, big.mark = ",", scientific = FALSE))

# 5. Create a function to produce geographic information output ----

export_geoinf <- function(territory_i){
  
  data_maritime_area_i <- data_maritime_area %>% 
    filter(TERRITORY1 == territory_i) %>% 
    select(AREA_KM2) %>% 
    pull()
  
  data_reef_area_i <- data_reef_area %>% 
    filter(TERRITORY1 == territory_i) %>% 
    select(area) %>% 
    pull()
  
  data_population_i <- data_population %>% 
    filter(TERRITORY1 == territory_i) %>% 
    select(population) %>% 
    pull()
  
  writeLines(c("\\begin{tabular}{>{\\bfseries}>{\\color{color1}}rl}",
               paste0("Maritime area & ", data_maritime_area_i, " km\\textsuperscript{2} \\\\"),
               "Land area & 12,000 km\\textsuperscript{2} \\\\",
               paste0("Reef area & ", data_reef_area_i, " km\\textsuperscript{2} \\\\"),
               paste0("Population (2020) & ", data_population_i, " \\\\"),
               "\\end{tabular}"),
             paste0("figs/03_geo-inf/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}

# 6. Map over the function ----

map(unique(data_eez$TERRITORY1), ~export_geoinf(territory_i = .))
