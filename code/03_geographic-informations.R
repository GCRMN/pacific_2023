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
  rename(territory = TERRITORY1, maritime_area = AREA_KM2)

# 3. Mean land elevation ----

data_elevation <- read.csv("data/02_geo-inf/02_elevation.csv") %>% 
  rename(territory = TERRITORY1, mean_elevation = mean)

# 4. Human population ----

data_population <- read.csv("data/02_geo-inf/01_human-pop.csv") %>% 
  rename(population = sum, territory = TERRITORY1, year = date) %>% 
  mutate(year = year(year),
         population = population*1e-06) %>% 
  pivot_wider(names_from = year, values_from = population, names_prefix = "pop_") %>% 
  mutate(diff_pop_abs = pop_2020 - pop_2000,
       diff_pop_rel = 100*(pop_2020 - pop_2000)/pop_2000) %>% 
  select(territory, pop_2020, diff_pop_rel) %>% 
  mutate(diff_pop_rel = replace_na(diff_pop_rel, 0))

# 5. Reef area ----

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
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6) %>% 
  rename(territory = TERRITORY1) %>% 
  mutate(reef_area_rel_world = (100*reef_area_abs)/(as.numeric(st_area(data_reefs))*1e-6),
         reef_area_rel_pacific = (100*reef_area_abs)/sum(reef_area_abs))

# 6. Land area ----

data_land <- read.csv("data/02_geo-inf/02_land-area.csv") %>% 
  rename(territory = TERRITORY1, land_area = sum)

# 7. Group data together ----

data_geoinf <- left_join(data_maritime_area, data_elevation) %>% 
  left_join(., data_population) %>% 
  left_join(., data_reef_area) %>% 
  left_join(., data_land)

# 8. Calculate the totals ----

data_geoinf <- bind_rows(data_geoinf, 
                         data_geoinf %>% summarise(maritime_area = sum(maritime_area, na.rm = TRUE),
                                                   pop_2020 = sum(pop_2020, na.rm = TRUE),
                                                   reef_area_abs = sum(reef_area_abs, na.rm = TRUE),
                                                   reef_area_rel_world = sum(reef_area_rel_world, na.rm = TRUE),
                                                   reef_area_rel_pacific = sum(reef_area_rel_pacific, na.rm = TRUE),
                                                   land_area = sum(land_area, na.rm = TRUE)) %>%
                           mutate(territory = "TOTAL"))

# 9. Round and convert formats ----

data_geoinf <- data_geoinf %>% 
  mutate(maritime_area = format(maritime_area, big.mark = ",", scientific = FALSE),
         mean_elevation = round(mean_elevation, 0),
         pop_2020 = format(round(pop_2020, 0), big.mark = ",", scientific = FALSE),
         diff_pop_rel = paste0(round(diff_pop_rel, 0), " %"),
         reef_area_abs = format(round(reef_area_abs, 0), big.mark = ",", scientific = FALSE),
         reef_area_rel_world = paste0(format(round(reef_area_rel_world, 3), nsmall = 3), " %"),
         reef_area_rel_pacific = paste0(format(round(reef_area_rel_pacific, 2), nsmall = 2), " %"),
         land_area = format(round(land_area, 0), big.mark = ",", scientific = FALSE)) %>% 
  mutate(across(c(diff_pop_rel, reef_area_abs, reef_area_rel_world, reef_area_rel_pacific),
                ~ replace(., str_detect(., "NA"), NA)))

# 10. Export the data ----

write_csv2(data_geoinf, file = "figs/01_geographic-information.csv")

# 11. Create a function to produce geographic information output (for LaTeX report) ----

export_geoinf <- function(territory_i){
  
  data_i <- data_geoinf %>% 
    filter(territory == territory_i)
  
  writeLines(c("\\begin{tabular}{>{\\bfseries}>{\\color{color1}}rl}",
               paste0("Maritime area & ", data_i[1, "maritime_area"], " km\\textsuperscript{2} \\\\"),
               paste0("Land area & ", data_i[1, "land_area"], " km\\textsuperscript{2} \\\\"),
               paste0("Reef area & ", data_i[1, "reef_area_abs"], " km\\textsuperscript{2} \\\\"),
               paste0("Mean elevation & ", data_i[1, "mean_elevation"], " m \\\\"),
               paste0("Population (2020) & ", data_i[1, "pop_2020"], " \\\\"),
               paste0("Population change & ", data_i[1, "diff_pop_rel"], " \\\\"),
               "\\end{tabular}"),
             paste0("figs/03_geo-inf/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}

# 7. Map over the function ----

map(unique(data_geoinf$territory), ~export_geoinf(territory_i = .))
