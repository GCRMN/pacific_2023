# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
colors <- c(palette_second[3], "#013C5E")

load("data/01_background-shp/02_princeton/data_land.RData")

# 3. Case study for New Caledonia (Laurent Wantiez) ----

## 3.1 Load and transform data ----

data_ouano <- read_xlsx("data/13_boxes/BACI Poissons Ouano 04_20.xlsx", sheet = 1) %>% 
  rename(year = "Année") %>% 
  group_by(year, Protection) %>% 
  summarise(biomass_mean = mean(B_Com),
            biomass_se = sd(B_Com)/sqrt(n()),
            richness_mean = mean(Sr_com),
            richness_se = sd(Sr_com)/sqrt(n()))

## 3.2 Biomass of commercial species ----

plot_a <- ggplot() +
  geom_segment(aes(x = 2006.5, y = 0, xend = 2006.5, yend = 78), linetype = "dashed", color = "darkgrey") +
  geom_curve(aes(x = 2006.5, y = 80, xend = 2008.2, yend = 88),
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type = "open"), curvature = -0.35,
             col = "black", lineend = "round") + 
  annotate(geom = "text", x = 2008.5, y = 88, label = "Implementation\nof the MPA",
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
  labs(x = "Year", y = expression(paste("Biomass (", g.m^{-1}, ") per station")), title = "A") +
  lims(y = c(0, 100)) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        axis.line.y = element_line(linewidth = 0.4, color = "black"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0.05, 0)))

## 3.3 Species richness of commercial species ----

plot_b <- ggplot() +
  geom_segment(aes(x = 2006.5, y = 4.5, xend = 2006.5, yend = 18), linetype = "dashed", color = "darkgrey") +
  geom_curve(aes(x = 2006.5, y = 4, xend = 2009, yend = 2.5),
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type = "open"), curvature = 0.35,
             col = "black", lineend = "round") + 
  annotate(geom = "text", x = 2009.5, y = 2.5, label = "Implementation\nof the MPA",
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
  labs(x = "Year", y = "Species richness per station", title = "B") +
  lims(y = c(0, 20)) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        axis.line.y = element_line(linewidth = 0.4, color = "black"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0.05, 0)))

## 3.4 Combine plots ----

plot_a + plot_b &
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA_character_),
                                panel.border = element_rect(fill = NA, color = NA)))

ggsave("figs/02_part-2/case-studies/01_new-caledonia_2.png",
       bg = "transparent", width = 10, height = 4.5, dpi = 300)

## 3.5 Map ----

data_land_ouano <- data_land %>% 
  filter(TERRITORY1 == "New Caledonia")

data_ouano <- tibble(lon = 165.779879, lat = -21.872524, site = "OUANO") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = data_land_ouano) +
  geom_sf(data = data_ouano, fill = colors[2], color = "white", size = 10, shape = 21) +
  geom_sf_label(data = data_ouano, aes(label = site), fill = colors[2], size = 7, label.padding = unit(7, "pt"),
                color = "white", family = font_choose_graph, nudge_x = 0.5, nudge_y = 0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  coord_sf(x = c(163, 168.6), y = c(-23, -19))

ggsave("figs/02_part-2/case-studies/01_new-caledonia_1.png", bg = "transparent")

## 3.6 Remove useless objects ----

rm(data_land_ouano, data_ouano, plot_a, plot_b)

# 4. Case study for Guam (Mary Allen) ----

## 4.1 Map ----

data_land_guam <- data_land %>% 
  filter(TERRITORY1 == "Guam")

ggplot() +
  geom_sf(data = data_land_guam) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/02_part-2/case-studies/02_guam_1.png", bg = "transparent")

## 4.2 Remove useless objects ----

rm(data_land_guam)

# 5. Case study for Yap (Mary Allen) ----

## 5.1 Map ----

data_land_yap <- data_land %>% 
  filter(TERRITORY1 == "Federated States of Micronesia")

ggplot() +
  geom_sf(data = data_land_yap) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  coord_sf(x = c(138, 138.25), y = c(9.4, 9.67))

ggsave("figs/02_part-2/case-studies/03_yap_1.png", bg = "transparent")

## 5.2 Remove useless objects ----

rm(data_land_yap)
