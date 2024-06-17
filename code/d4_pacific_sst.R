# 1. Load packages ----

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)
library(RcppRoll)
library(terra)
library(sf)
library(tidyterra)
library(RColorBrewer)
library(extrafont)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Comparison of warming rates ----

## 3.1 Load and transform data ----

data_warming <- read.csv("data/09_misc/data_warming.csv") %>% 
  filter(!(TERRITORY1 %in% c("Entire Pacific region", "Pacific Remote Island Area", "Kiribati"))) %>% 
  select(TERRITORY1, warming_rate, sst_increase) %>% 
  # The number 0.88°C is coming from Technical summary IPCC, 2021 (TS.2.4, The Ocean, page 74)
  add_row(TERRITORY1 = "Global Ocean", warming_rate = (0.88/(2020-1900))*(2022-1980), sst_increase = 0.88) %>% 
  mutate(warming_rate = round(warming_rate, 3),
         color = case_when(TERRITORY1 == "Global Ocean" ~ palette_first[5],
                           sst_increase > 0 & TERRITORY1 != "Global Ocean" ~ palette_first[3],
                           sst_increase <= 0 & TERRITORY1 != "Global Ocean" ~ palette_first[2]),
         TERRITORY1 = if_else(TERRITORY1 == "Global Ocean", "**Global Ocean**", TERRITORY1)) %>% 
  arrange(desc(sst_increase)) %>% 
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, c("Islands" = "Isl.",
                                                    "Federated States of Micronesia" = "Fed. Sts. Micronesia",
                                                    "Northern" = "North.",
                                                    "Howland" = "How.")))

## 3.2 Make the plot ----

ggplot(data = data_warming, aes(x = sst_increase, y = fct_reorder(TERRITORY1, sst_increase))) +
  geom_bar(stat = "identity", aes(fill = color), width = 0.6, color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_vline(xintercept = 0) +
  labs(x = "Change in SST (°C)\nbetween 1985 and 2023", y = NULL) +
  theme_graph() +
  theme(axis.text.y = element_markdown()) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 1.5))

## 3.3 Save the plot ----

ggsave("figs/01_part-1/fig-4.png", height = 10, width = 5, dpi = fig_resolution)

# 4. Southern Oscillation Index ----

## 4.1 Load and transform data ----

data_enso <- read_table("data/09_misc/enso-soi.txt", skip = 87) %>% 
  filter(YEAR %in% c(1980:2023)) %>% 
  select(-X14) %>% 
  mutate_all(., ~as.numeric(.)) %>% 
  pivot_longer(2:ncol(.), values_to = "soi", names_to = "month") %>% 
  rename(year = YEAR) %>% 
  mutate(month = str_replace_all(month, c("JAN" = "1",
                                          "FEB" = "2",
                                          "MAR" = "3",
                                          "APR"= "4",
                                          "MAY"= "5",
                                          "JUN" = "6",
                                          "JUL"= "7",
                                          "AUG" = "8",
                                          "SEP" = "9",
                                          "OCT" = "10",
                                          "NOV" = "11",
                                          "DEC"= "12")),
         date = ym(paste(year, month, sep = "-")),
         # 6 months moving average
         soi_roll = roll_mean(x = soi, n = 6, align = "center", fill = NA))

## 4.2 Make the plot ----

ggplot() +
  geom_bar(data = data_enso, aes(x = date, y = soi), stat = "identity", width = 30, fill = "lightgrey") +
  geom_ribbon(data = data_enso %>% mutate(soi_roll = if_else(soi_roll < 0, 0, soi_roll)),
              aes(x = date, ymin = 0, ymax = soi_roll), fill = palette_first[3], alpha = 0.9) +
  geom_ribbon(data = data_enso %>% mutate(soi_roll = if_else(soi_roll > 0, 0, soi_roll)),
              aes(x = date, ymin = 0, ymax = soi_roll), fill = palette_second[3], alpha = 0.9) +
  geom_line(data = data_enso, aes(x = date, y = soi_roll), linewidth = 0.3) +
  labs(x = "Year", y = "Southern Oscillation Index") +
  # Annotation
  annotate(geom = "rect", xmin = ym("1986-06"), xmax = ym("1993-08"),
           ymin = 2.3, ymax = 2.7, fill = palette_first[3], color = NA) +
  annotate(geom = "text", x = ym("1990-01"), y = 2.5, color = "white",
           label = "La Niña", family = font_choose_graph, size = 5) +
  annotate(geom = "rect", xmin = ym("2011-07"), xmax = ym("2018-06"),
           ymin = -3.2, ymax = -2.775, fill = palette_second[3], color = NA) +  
  annotate(geom = "text", x = ym("2015-01"), y = -3, color = "white",
           label = "El Niño", family = font_choose_graph, size = 5) +
  scale_y_continuous(limits = c(-3.5, 3.5), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  coord_cartesian(clip = "off")

## 4.3 Save the plot ----

ggsave("figs/01_part-1/fig-5.png", height = 4, width = 5, dpi = fig_resolution)

# 5. Comparison of SST distribution ----

## 5.1 Transform data ----

load("data/09_misc/data-sst.RData")

data_sst <- data_sst %>% 
  filter(!(TERRITORY1 %in% c("Entire Pacific region", "Pacific Remote Island Area", "Kiribati"))) %>% 
  group_by(TERRITORY1) %>% 
  summarise(mean = mean(sst)) %>% 
  ungroup() %>% 
  left_join(., data_sst)

## 5.2 Make the plot ----

ggplot(data = data_sst, aes(x = sst, y = fct_reorder(TERRITORY1, mean))) +
  geom_violin(draw_quantiles = c(0.5), fill = "#446CB3", alpha = 0.5) +
  labs(x = "SST (°C)", y = NULL) +
  theme_graph() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(20, 32.5))

## 5.3 Save the plot ----

ggsave("figs/04_supp/01_data-explo/02_sst-distribution.png", height = 8, width = 6, dpi = fig_resolution)

# 6. Map of maximum DHW per year ----

## 6.1 Load data ----

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")

data_eez <- read_sf("data/01_background-shp/03_eez/data_eez.shp")

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

## 6.2 List of files ----

data_files <- tibble(path = list.files("data/08_dhw-year/", full.names = TRUE)) %>% 
  mutate(year = as.numeric(str_sub(path, -8, -4)),
         group = rep(1:50, each = 8, length.out = nrow(.))) # 8 is the number of subplots (i.e. years) per plot

## 6.3 Create the function to make the plot for each year ----

map_dhw_year <- function(year_i, data_files_i){
  
  # 1. Load data ----
  
  raster <- rast(data_files_i %>% filter(year == year_i) %>% select(path) %>% pull)
  
  # 2. Make the plot ----
  
  ggplot() +
    geom_spatraster(data = raster) +
    geom_sf(data = data_eez, fill = NA) +
    geom_sf(data = data_map, fill = "#363737", col = "grey") +
    scale_fill_gradientn(colours = c("white", "#e4f1fe", "#89c4f4", "#59abe3", "#fffc7f", 
                                     "#f1d693", "#f9b42d", "#e67e22", "#ec644b",
                                     "#d64541", "#96281b", "#663399", "#5a228b"),
                         limits = c(0, 50),
                         name = "Maximum DHW (°C-weeks)",
                         guide = guide_colourbar(direction = "horizontal", 
                                                 title.position = "top", 
                                                 title.hjust = 0.5, 
                                                 ticks.colour = "black",
                                                 frame.colour = "black")) +
    coord_sf(crs = crs_selected,
             ylim = c(-4000000, 4000000), 
             xlim = c(-3500000, 11000000), expand = FALSE) +
    labs(title = year_i) +
    theme(text = element_text(family = "Open Sans"),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          legend.text = element_text(family = "Open Sans"),
          plot.title = element_text(family = "Open Sans", hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA),
          legend.key.width = unit(1.5, "cm"),
          legend.key.height = unit(0.2, "cm"),
          legend.position = "bottom")
  
}

## 6.4 Create the function to make the plot for each group ----

map_dhw_plot <- function(group_i){
  
  # 1. Filter the data_files ----
  
  data_files_i <- data_files %>% 
    filter(group == group_i)
  
  # 2. Create all the plots ----
  
  plots <- map(c(data_files_i$year), ~map_dhw_year(data_files_i = data_files_i, year_i = .))
  
  # 3. Combine plots ----
  
  combined_plots <- wrap_plots(plots) + 
    plot_layout(guides = "collect", ncol = 2) & 
    theme(legend.position = "bottom")
  
  # 4. Save the plot ----
  
  ggsave(filename = paste0("figs/04_supp/03_indicators/01_dhw-map_", min(data_files_i$year), "-", max(data_files_i$year), ".png"),
         height = 10, combined_plots, dpi = 600)
  
}

## 6.5 Map over the function ----

map(unique(data_files$group), ~map_dhw_plot(group_i = .))
