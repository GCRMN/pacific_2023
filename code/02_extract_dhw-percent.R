# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
library(tidyterra)

# 2. Get NCDF files ----

ncdf_files <- list.files("data/09_dhw/", full.names = TRUE)

# 3. Load EEZ data ----

data_eez <- read_sf("data/01_background-shp/03_eez/data_eez.shp") %>% 
  st_transform(crs = 4326)

data_eez_spatvector <- vect(data_eez)

data_eez_names <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1) %>% 
  mutate(zone = row_number())

# 4. Create a function to extract values of cells ----

extract_dhw_percent <- function(ncdf_i){
  
  ncdf <- rast(ncdf_i)
  
  results <- terra::freq(ncdf, zones = data_eez_spatvector, usenames = TRUE) %>% 
    filter(layer == "degree_heating_week") %>% 
    mutate(date = lubridate::date(unique(time(ncdf))))
  
  return(results)
  
}

# 5. Apply function to each NCDF files ----

data_dhw_percent <- map_dfr(ncdf_files[1:100], ~extract_dhw_percent(.))

data_dhw_percent <- data_dhw_percent %>% 
  rename(dhw = value) %>% 
  left_join(., data_eez_names) %>% 
  select(-layer, -zone) %>% 
  tidyr::complete(date, dhw, TERRITORY1, fill = list(count = 0)) %>%
  group_by(TERRITORY1, date) %>% 
  mutate(freq = count*100/sum(count)) %>% 
  filter(dhw >= 1) %>% 
  summarise(freq = sum(freq))

# 6. Export the data ----

save(data_dhw_percent, file = "data/10_data-dhw-percent.RData")

# 7. Make the plot ----

load("data/10_data-dhw-percent.RData")

ggplot(data = data_dhw_percent, aes(x = date, y = freq)) +
  geom_path() +
  facet_wrap(~TERRITORY1)
