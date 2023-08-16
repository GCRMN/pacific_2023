# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(scico)
library(extrafont)
library(treemapify)

# 2. Source functions ----

source("code/function/graphical_par.R")

# 3. Load and transform data ----

data_reefs <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES"))

data_reef_area <- st_intersection(data_eez, data_reefs) %>%  
  group_by(TERRITORY1) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6) %>% 
  rename(territory = TERRITORY1) %>% 
  arrange(-reef_area_abs) %>% 
  mutate(color = scico(30, begin = 0.85, end = 0, palette = "lajolla"))

# 4. Make the plot ----

ggplot(data = data_reef_area, aes(area = reef_area_abs, fill = color, label = territory)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(color = "white", place = "centre", reflow = TRUE, family = font_choose_graph) +
  scale_fill_identity()

# 5. Save the plot ----

ggsave(filename = "figs/reef_area_pacific.png", height = 5, width = 5, dpi = 600)
