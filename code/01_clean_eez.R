# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE) # Switch from S2 to GEOS
library(magrittr) # To use the pipe %<>%

# 2. Define changed CRS ----

# 2.1 Define the CRS --

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# 2.2 Define the offset --

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

# 2.3 Define a long and slim polygon that overlaps the meridian line --

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# 3. List of EEZ to plot ----

list_eez <- c("American Samoa", "Cook Islands", "Fiji", "French Polynesia", "Gilbert Islands", "Guam", "Hawaii",
              "Howland and Baker islands", "Jarvis Island", "Johnston Atoll", "Line Group", "Marshall Islands",
              "Micronesia", "Nauru", "New Caledonia", "Niue", "Northern Mariana Islands",
              "Palau", "Palmyra Atoll", "Papua New Guinea", "Phoenix Group", "Pitcairn", "Samoa", "Solomon Islands",
              "Tokelau", "Tonga", "Tuvalu", "Vanuatu", "Wake Island", "Wallis and Futuna")

# 4. Filter and change CRS ----

data_eez <- read_sf("data/01_background-shp/03_eez/World_EEZ_v11_20191118/eez_v11.shp") %>% 
  filter(TERRITORY1 %in% list_eez) %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

# 5. Remove vertical 180° line ----

data_eez %<>% # Special pipe from magrittr
  st_buffer(10) # To join polygon (remove vertical line)

# 6. Remove holes within polygons ----

data_eez <- nngeo::st_remove_holes(data_eez)

# 7. Include land within Papua New Guinea EEZ (to extract population, elevation, etc) ----

data_land <- st_read("data/01_background-shp/02_princeton/papua-new-guinea/PNG_adm0.shp") %>% 
  st_transform(crs_selected) %>% 
  select(geometry)

data_eez <- data_eez %>% 
  # Union the two polygons of PNG
  filter(TERRITORY1 == "Papua New Guinea") %>% 
  group_by(TERRITORY1, SOVEREIGN1) %>%
  summarise(do_union = TRUE) %>% 
  ungroup() %>% 
  # Union with PNG land polygon
  st_union(., data_land) %>% 
  nngeo::st_remove_holes(.) %>%
  # Add AREA_KM2 (sum of the two EEZ polygons)
  mutate(AREA_KM2 = read_sf("data/01_background-shp/03_eez/World_EEZ_v11_20191118/eez_v11.shp") %>%
           filter(TERRITORY1 == "Papua New Guinea") %>%
           st_drop_geometry() %>%
           select(AREA_KM2) %>%
           summarise(AREA_KM2 = sum(AREA_KM2)) %>%
           pull()) %>% 
  # Add to main EEZ data
  bind_rows(data_eez %>% 
              filter(TERRITORY1 != "Papua New Guinea"),
            .)

# 8. Attribute EEZ number ----

data_eez <- data_eez %>% 
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, "Micronesia", "Federated States of Micronesia")) %>% 
  mutate(number = case_when(TERRITORY1 == "Palau" ~ 1,
                            TERRITORY1 == "Federated States of Micronesia" ~ 2,
                            TERRITORY1 == "Guam" ~ 3,
                            TERRITORY1 == "Northern Mariana Islands" ~ 4,
                            TERRITORY1 == "Marshall Islands" ~ 5,
                            TERRITORY1 == "Wake Island" ~ 6,
                            TERRITORY1 == "Hawaii" ~ 7,
                            TERRITORY1 == "Johnston Atoll" ~ 8,
                            TERRITORY1 == "Papua New Guinea" ~ 9,
                            TERRITORY1 == "Nauru" ~ 10,
                            TERRITORY1 == "Solomon Islands" ~ 11,
                            TERRITORY1 == "Vanuatu" ~ 12,
                            TERRITORY1 == "New Caledonia" ~ 13,
                            TERRITORY1 == "Fiji" ~ 14,
                            TERRITORY1 == "Wallis and Futuna" ~ 15,
                            TERRITORY1 == "Tuvalu" ~ 16,
                            TERRITORY1 == "Gilbert Islands" ~ 17,
                            TERRITORY1 == "Howland and Baker islands" ~ 18,
                            TERRITORY1 == "Phoenix Group" ~ 19,
                            TERRITORY1 == "Tokelau" ~ 20,
                            TERRITORY1 == "American Samoa" ~ 21,
                            TERRITORY1 == "Samoa" ~ 22,
                            TERRITORY1 == "Tonga" ~ 23,
                            TERRITORY1 == "Niue" ~ 24,
                            TERRITORY1 == "Cook Islands" ~ 25,
                            TERRITORY1 == "French Polynesia" ~ 26,
                            TERRITORY1 == "Palmyra Atoll" ~ 27,
                            TERRITORY1 == "Jarvis Island" ~ 28,
                            TERRITORY1 == "Line Group" ~ 29,
                            TERRITORY1 == "Pitcairn" ~ 30))

# 9. Create coordinates for label placement ----

data_eez <- tibble(number = 1:30,
                   lat = c(4, 6, 13, 20, 15, 19.5, 27, 16, -2, -1,
                           -10, -18, -21, -21, -13.5, -8, -1, 1, -5,
                           -9, -15, -13, -23, -19, -20, -22, 7, -1, -9.5,
                           -25),
                   long = c(132, 143, 144, 144.5, 170, 167, -175, -169, 146, 167,
                            165, 170, 160, 177, -178, 178, 171, -176, -175,
                            -172, -168, -172, -176, -169, -160, -150, -163, -160.5, -150,
                            -130)) %>% 
  left_join(data_eez, .)

# 10. Save data ----

save(data_eez, file = "data/01_background-shp/03_eez/data_eez.RData") # RData

st_write(data_eez, "data/01_background-shp/03_eez/data_eez.shp", append = FALSE) # Shapefile
