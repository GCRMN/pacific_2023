# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
colors <- c(palette_second[3], "#013C5E")

# 3. Load and transform data ----

data_ouano <- read_xlsx("data/18_boxes/BACI Poissons Ouano 04_20.xlsx", sheet = 1) %>% 
  rename(year = "Année") %>% 
  group_by(year, Protection) %>% 
  summarise(biomass_mean = mean(B_Com),
            biomass_se = sd(B_Com)/sqrt(n()),
            richness_mean = mean(Sr_com),
            richness_se = sd(Sr_com)/sqrt(n()))

# 4. Make the plots ----

## 4.1 Biomass of commercial species ----

plot_a <- ggplot() +
  geom_vline(xintercept = 2006.5) +
  geom_curve(aes(x = 2006.75, y = 88, xend = 2008.2, yend = 88),
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type = "open"), curvature = 0,
             col = "black", lineend = "round") + 
  annotate(geom = "text", x = 2008.5, y = 88, label = "Implementation of\nthe MPA",
           hjust = 0, family = font_choose_graph, color = colors[2]) +
  geom_errorbar(data = data_ouano, aes(x = year, 
                                       ymin = biomass_mean - biomass_se, 
                                       ymax = biomass_mean + biomass_se, color = Protection),
                width = 0.3, show.legend = FALSE) +
  geom_line(data = data_ouano, aes(x = year, y = biomass_mean, color = Protection),
            show.legend = FALSE) +
  geom_point(data = data_ouano, aes(x = year, y = biomass_mean, fill = Protection),
             size = 3, shape = 21, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  annotate(geom = "text", x = 2016.5, y = 65, label = "Within MPA",
           family = font_choose_graph, color = colors[2]) +
  annotate(geom = "text", x = 2017.5, y = 17, label = "Outside MPA", 
           family = font_choose_graph, color = colors[1]) +
  labs(x = "Year", y = expression(paste("Biomass (", g.m^{-1}, ")")), title = "A") +
  lims(y = c(0, 100)) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(fill = NA))

## 4.2 Species richness of commercial species ----

plot_b <- ggplot() +
  geom_vline(xintercept = 2006.5) +
  geom_curve(aes(x = 2006.75, y = 2.5, xend = 2008.2, yend = 2.5),
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type = "open"), curvature = 0,
             col = "black", lineend = "round") + 
  annotate(geom = "text", x = 2008.5, y = 2.5, label = "Implementation of\nthe MPA",
           hjust = 0, family = font_choose_graph, color = colors[2]) +
  geom_errorbar(data = data_ouano, aes(x = year, 
                                       ymin = richness_mean - richness_se, 
                                       ymax = richness_mean + richness_se, color = Protection),
                width = 0.3, show.legend = FALSE) +
  geom_line(data = data_ouano, aes(x = year, y = richness_mean, color = Protection),
            show.legend = FALSE) +
  geom_point(data = data_ouano, aes(x = year, y = richness_mean, fill = Protection),
             size = 3, shape = 21, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  annotate(geom = "text", x = 2017.5, y = 15, label = "Within MPA",
           family = font_choose_graph, color = colors[2]) +
  annotate(geom = "text", x = 2017.5, y = 8, label = "Outside MPA", 
           family = font_choose_graph, color = colors[1]) +
  labs(x = "Year", y = "Species richness", title = "B") +
  lims(y = c(0, 20)) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = NA),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        plot.title = element_text(face = "bold"),
        panel.border = element_rect(fill = NA))

## 4.3 Combine plots ----

plot_a + plot_b

ggsave("figs/02_part-2/boxes/01_box_fish-new-caledonia_1.png", bg = "transparent", width = 11, height = 5)

## 4.4 Map of Ouano ----

load("data/01_background-shp/02_princeton/data_land.RData")

data_land <- data_land %>% 
  filter(TERRITORY1 == "New Caledonia")

data_ouano <- tibble(lon = 165.779879, lat = -21.872524, site = "OUANO") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = data_land) +
  geom_sf(data = data_ouano, fill = colors[2], color = "white", size = 10, shape = 21) +
  geom_sf_label(data = data_ouano, aes(label = site), fill = colors[2], size = 9, label.padding = unit(7, "pt"),
                color = "white", family = font_choose_graph, nudge_x = 0.5, nudge_y = 0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  coord_sf(x = c(163, 168.6), y = c(-23, -19))

ggsave("figs/02_part-2/boxes/01_box_fish-new-caledonia_2.png", bg = "transparent")
