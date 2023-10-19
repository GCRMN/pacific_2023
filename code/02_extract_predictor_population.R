# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(terra)
library(sf)
sf_use_s2(FALSE)
library(tidyterra)

# 2. Benthic cover sites ----

# 2.1 Import site coordinates --

data_benthic_sites <- st_read("data/15_benthic-site-coords/benthic-site-coords.shp") %>% 
  select(-FID) %>% 
  mutate(site_id = row_number())

# 2.2 Convert to sf and create buffer --

data_benthic_buffer <- data_benthic_sites %>% 
  # Create 50 km buffer around each site
  st_transform(crs = 7801) %>% 
  st_buffer(., dist = 50000) %>% 
  st_transform(crs = 4326) %>% 
  st_make_valid() %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

# 3. GPW --

data_pop_all <- rast("data/13_population/gpw_v4_population_count_rev11_2pt5_min.nc")

# 4. Visual check ----

plot(data_pop_all$`Population Count, v4.11 (2000, 2005, 2010, 2015, 2020): 2.5 arc-minutes_raster=1`)
plot(data_benthic_buffer, add = TRUE)

# 5. Extract total human population living within 50 km from each site ----

# 5.1 Create list of raster layers to extract values from --

list_names <- tibble(number = 1:5,
                     name = names(data_pop_all)[1:5],
                     year = c(2000, 2005, 2010, 2015, 2020))

# 5.2 Create a function --

extract_population <- function(i, data_vector){
  
  data_pop_i <- terra::subset(data_pop_all, i)
  
  result <- terra::extract(x = data_pop_i, y = data_vector, fun = sum, na.rm = TRUE) %>% 
    rename(population = 2, site_id = ID) %>% 
    mutate(year = as.numeric(list_names[i, "year"]))
  
  return(result)
  
}

# 5.3 Map over the function --

data_pred_pop <- map_dfr(unique(list_names$number), 
                        ~extract_population(i = ., data_vector = data_benthic_buffer)) %>% 
  left_join(data_benthic_sites, .) %>% 
  select(-site_id) %>% 
  mutate(decimalLongitude = st_coordinates(.)[,"X"],
         decimalLatitude = st_coordinates(.)[,"Y"]) %>% 
  st_drop_geometry()

# 5.4 Export the data --

save(data_pred_pop, file = "data/14_predictors/population.RData")
