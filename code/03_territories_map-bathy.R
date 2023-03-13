# 1. Load packages ----

library(tidyverse)
library(magrittr) # For special pipe %<>%
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function

# 2. Load functions ----

source("code/function/graphical_par.R")
source("code/function/map_eez_bathy.R")

# 3. Load data ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_parameters <- read.csv2("data/path_individual_maps.csv")

load("data/04_data-benthic.RData")

data_benthic <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, territory) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

# 5. Bathymetry ----

# 5.1 List path of shapefiles --

list_shp <- list.files(path = "data/01_background-shp/01_ne/ne_10m_bathymetry_all/", pattern = ".shp", full.names = TRUE)

# 5.2 Combine shapefiles --

data_bathy <- map_dfr(list_shp, ~st_read(., quiet = TRUE)) %>% 
  mutate(color = case_when(depth == 0 ~ "#e1f5fe",
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
  mutate(color = fct_reorder(color, depth)) %>% 
  st_transform(crs = 4326)

# 4. Create the EEZ individual maps ----

map(unique(data_eez$TERRITORY1), ~map_eez_bathy(territory_i = ., data_parameters = data_parameters))














territory_i <- "Hawaii"


data_eez_i <- data_eez %>% 
  filter(TERRITORY1 == territory_i) %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline()

# 2. Select bathy data ----

data_bathy_i <- st_intersection(data_bathy, data_eez_i)





ggplot() +
  # Bathymetry
  geom_sf(data = data_bathy_i, aes(fill = color), color = NA) +
  scale_fill_identity() +
  geom_sf(data = data_eez_i, color = "black", fill = NA, alpha = 0.75)






# 3. Load boundary data ----

data_land <- st_read(list.files(path = paste0("data/01_background-shp/02_princeton/", 
                                              str_replace_all(str_to_lower(territory_i), 
                                                              " ", "-"), "/"), 
                                pattern = ".shp$", 
                                full.names = TRUE))

# 4. Misc parameters ----

scale_bar_pos <- data_parameters %>% 
  filter(TERRITORY1 == territory_i) %>% 
  select(scale_pos) %>% 
  pull()

plot_width <- data_parameters %>% 
  filter(TERRITORY1 == territory_i) %>% 
  select(width) %>% 
  pull()

plot_height <- data_parameters %>% 
  filter(TERRITORY1 == territory_i) %>% 
  select(height) %>% 
  pull()

# 5. Make the plot ----

ggplot() +
  # Bathymetry
  geom_sf(data = data_bathy_i, aes(fill = color), color = NA) +
  scale_fill_identity() +
  geom_sf(data = data_eez_i, color = "black", fill = NA, alpha = 0.75) +
  geom_sf(data = data_land, fill = "#363737", col = "grey") +
  annotation_scale(location = scale_bar_pos, width_hint = 0.3, text_family = font_choose_graph, 
                   text_cex = 0.75, style = "bar", line_width = 0.5,  height = unit(0.05, "cm"),
                   pad_x = unit(0.75, "cm"), pad_y = unit(0.75, "cm"), bar_cols = c("black", "black")) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())




























map_eez_bathy(territory_i = "Palau", data_parameters = data_parameters)


data_eez_i <- data_eez %>% 
  filter(TERRITORY1 == "Hawaii") %>% 
  st_transform(crs = 4326) %>% 
  st_transform(., crs = 3460) %>% 
  st_union(.)


data_eez_i <- data_eez %>% filter(TERRITORY1 == "Hawaii") %>% st_transform(crs = 3460)

data_bathy_i <- st_intersection(data_bathy, data_eez_i)

ggplot() +
  geom_sf(data = data_bathy_i, aes(fill = color), color = NA) +
  scale_fill_identity()
  


ggplot() +
  #geom_sf(data = data_bathy, aes(fill = color), color = NA) +
  geom_sf(data = data_eez_i) +
  scale_fill_identity()
