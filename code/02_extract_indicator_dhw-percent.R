# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(tidyterra)
library(future)
library(furrr)

plan(multisession, workers = 2) # Set parallelization with 2 cores

# 2. Get NCDF files ----

ncdf_files <- list.files("data/09_dhw/", full.names = TRUE)

# 3. Load EEZ data ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  select(TERRITORY1) %>% 
  st_transform(crs = 4326)

# Visual Check
#plot(rast(ncdf_files[1])$degree_heating_week)
#plot(data_reef, add = TRUE)

# 4. Create a function to extract values of cells ----

extract_dhw_percent <- function(ncdf_i, data_reef){
  
  ncdf <- rast(ncdf_i)$degree_heating_week
  
  crs(ncdf) <- "epsg:4326"
  
  data_reef <- terra::vect(data_reef)
  
  results <- terra::freq(x = ncdf, zones = data_reef) %>% 
    mutate(date = lubridate::date(unique(time(ncdf))))
  
  return(results)
  
}

# 5. Map over the function ----

data_dhw_percent <- future_map_dfr(ncdf_files, ~extract_dhw_percent(ncdf_i = ., data_reef = data_reef)) %>% 
  rename(dhw = value) %>% 
  left_join(., tibble(territory = data_reef$TERRITORY1) %>%
              mutate(zone = row_number())) %>% 
  select(-layer, -zone) %>% 
  tidyr::complete(date, dhw, territory, fill = list(count = 0))

# 6. DHW percent per territory ----

data_dhw_percent_territory <- data_dhw_percent %>%  
  group_by(date, territory) %>% 
  mutate(freq = count*100/sum(count)) %>% 
  ungroup() %>% 
  mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                              dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                              dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                              dhw >= 8 ~ "DHW >= 8")) %>% 
  group_by(date, territory, dhw_type) %>%
  summarise(freq = sum(freq)) %>% 
  ungroup()

# 7. DHW percent for the Pacific ----

data_dhw_percent_pacific <- data_dhw_percent %>%  
  group_by(date) %>% 
  mutate(freq = count*100/sum(count)) %>% 
  ungroup() %>% 
  mutate(dhw_type = case_when(dhw == 0 ~ "DHW = 0",
                              dhw > 0 & dhw < 4 ~ "0 < DHW < 4",
                              dhw >= 4 & dhw < 8 ~ "4 <= DHW < 8",
                              dhw >= 8 ~ "DHW >= 8")) %>% 
  group_by(date, dhw_type) %>%
  summarise(freq = sum(freq)) %>% 
  ungroup() %>% 
  mutate(territory = "Pacific")

# 8. Combine data ----

data_dhw_percent <- bind_rows(data_dhw_percent_territory, data_dhw_percent_pacific)

# 9. Export the data ----

save(data_dhw_percent, file = "data/10_data-dhw-percent.RData")
