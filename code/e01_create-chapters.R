# 1. Load packages ----

library(tidyverse)
library(rmarkdown)
library(sf)
library(googledrive)

# 2. Load functions ----

source("code/function/render_qmd.R")

# 3. Load data ----

load("data/05_cyclones/02_cyclones_extracted.RData")

# 4. Create docx files for each area ----

## 4.1 List of countries and territories ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  select(TERRITORY1) %>% 
  # Kiribati
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Phoenix Group" = "Kiribati",
                                                    "Gilbert Islands" = "Kiribati",
                                                    "Line Group" = "Kiribati"))) %>% 
  # Pacific Remote Island Area
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Jarvis Island" = "pria",
                                                    "Wake Island" = "pria",
                                                    "Howland and Baker Islands" = "pria",
                                                    "Johnston Atoll" = "pria",
                                                    "Palmyra Atoll" = "pria"))) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  arrange(TERRITORY1, .locale = "en") %>% 
  mutate(nb = row_number())

## 4.2 Map over the function ----

map(data_area, ~render_qmd(area_i = ., upload_drive = TRUE))

render_qmd(territory_i = "New Caledonia", upload_drive = FALSE)
