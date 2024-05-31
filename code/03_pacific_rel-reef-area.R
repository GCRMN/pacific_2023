# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(treemapify)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load and transform data ----

data_reefs <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

## 3.1 Reef area by Pacific EEZ ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

data_reef_area_pacific <- st_intersection(data_eez, data_reefs) %>%  
  group_by(TERRITORY1) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6) %>% 
  rename(territory = TERRITORY1) %>% 
  arrange(-reef_area_abs)

plot_b <- ggplot(data = data_reef_area_pacific, aes(area = reef_area_abs, fill = reef_area_abs, label = territory)) +
  geom_treemap(show.legend = FALSE, color = "white", size = 2) +
  geom_treemap_text(color = "white", place = "centre", reflow = TRUE, family = font_choose_graph) +
  scale_fill_gradientn(colours = palette_first[2:5]) +
  theme_graph()

ggsave(filename = "figs/01_part-1/fig-2b.png", plot = plot_b, height = 5, width = 5, dpi = fig_resolution)

## 3.2 Reef area by GCRMN regions ----

load("data/01_background-shp/gcrmn_regions.RData")

data_reef_area_gcrmn <- st_intersection(data_gcrmn_regions, data_reefs) %>%  
  group_by(gcrmn_region) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6,
         gcrmn_region = str_replace_all(gcrmn_region, "EAS", "East Asian Seas"),
         color = case_when(gcrmn_region == "Pacific" ~ palette_first[3],
                           TRUE ~ palette_first[4]),
         color_text = case_when(gcrmn_region == "Pacific" ~ "white",
                                TRUE ~ "white"))

plot_a <- ggplot(data = data_reef_area_gcrmn, aes(area = reef_area_abs, fill = color, label = gcrmn_region)) +
  geom_treemap(show.legend = FALSE, color = "white", size = 2, start = "bottomright") +
  geom_treemap_text(aes(color = color_text), place = "centre", reflow = TRUE,
                    family = font_choose_graph, start = "bottomright") +
  scale_fill_identity() +
  scale_color_identity() +
  theme_graph()

ggsave(filename = "figs/01_part-1/fig-2a.png", plot = plot_a, height = 5, width = 5, dpi = fig_resolution)

## 3.3 Combine the two figures ----

(plot_a + labs(title = "A") + theme(plot.margin = unit(c(0.25, 0.75, 0.25, 0), "cm"),
                                    plot.title = element_text(size = 18))) +
   (plot_b + labs(title = "B") + theme(plot.margin = unit(c(0.25, 0, 0.25, 0.75), "cm"),
                                       plot.title = element_text(size = 18)))

ggsave(filename = "figs/01_part-1/fig-2.png", height = 5, width = 10, dpi = fig_resolution)
