# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(tidyterra)

# 2. Load data ----

# 2.1 EEZ --

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 2.2 Load 100 km coral reef buffer --

data_reef_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid() %>% 
  st_intersection(., data_eez) %>% 
  select(TERRITORY1) %>% 
  rename(territory = TERRITORY1)

# 2.3 GPW --

data_pop_all <- rast("data/13_population/gpw_v4_population_count_rev11_2pt5_min.nc")

# 3. Create list of raster layers to extract values from ----

list_names <- tibble(number = 1:5,
                     name = names(data_pop_all)[1:5],
                     year = c(2000, 2005, 2010, 2015, 2020))

# 4. Create a function ----

extract_population <- function(i, data_vector, vector_type){
  
  data_pop_i <- terra::subset(data_pop_all, i)
  
  result <- terra::extract(x = data_pop_i, y = data_vector, fun = sum, na.rm = TRUE) %>% 
    rename(population = 2) %>% 
    mutate(year = as.numeric(list_names[i, "year"]),
           territory = data_reef_buffer$territory,
           type = vector_type) %>% 
    select(-ID)
  
  return(result)
  
}

# 5. Map over the function ----

# 5.1 Extract population by reef buffer --

data_results_reef <- map_dfr(unique(list_names$number), 
                             ~extract_population(i = ., data_vector = data_reef_buffer,
                                                 vector_type = "100 km reef buffer"))

# 5.2 Extract population by EEZ --

data_results_eez <- map_dfr(unique(list_names$number), 
                            ~extract_population(i = ., data_vector = data_eez,
                                                vector_type = "EEZ"))

# 6. Transform the data ----

data_pop_2020_eez <- data_results_eez %>% 
  filter(year == 2020) %>% 
  select(-type, -year) %>% 
  rename(pop_eez_2020 = population)

data_pop_2020_reefs <- data_results_reef %>% 
  filter(year %in% c(2000, 2020)) %>% 
  select(-type) %>% 
  pivot_wider(names_from = year, values_from = population, names_prefix = "pop_reefs_")

# 7. Export the data ----

left_join(data_pop_2020_reefs, data_pop_2020_eez) %>% 
  write.csv2("data/02_geo-inf/01_human-pop.csv", row.names = FALSE)

write.csv(data_results_reef %>% select(-type), "data/14_human-pop_reef.csv", row.names = FALSE)
