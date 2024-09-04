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
source("code/function/extract_coeff.R")

theme_set(theme_graph())

# 3. Comparison of warming rates ----

## 3.1 Load and transform data ----

data_warming <- read.csv("data/09_misc/data-warming.csv") %>% 
  filter(!(TERRITORY1 %in% c("Pacific Remote Island Area", "Kiribati"))) %>% 
  select(TERRITORY1, warming_rate, sst_increase) %>% 
  # The number 0.97°C is coming from Forster et al (2024) - Table 5, page 2638
  add_row(TERRITORY1 = "Global Ocean", warming_rate = NA, sst_increase = 0.97) %>% 
  mutate(warming_rate = round(warming_rate, 3),
         color = case_when(TERRITORY1 == "Global Ocean" ~ "black",
                           TERRITORY1 == "Entire Pacific region" ~ palette_second[4],
                           sst_increase > 0 & TERRITORY1 != "Global Ocean" ~ palette_second[3],
                           sst_increase <= 0 & TERRITORY1 != "Global Ocean" ~ palette_first[2]),
         TERRITORY1 = if_else(TERRITORY1 == "Global Ocean", "**Global Ocean**", TERRITORY1),
         TERRITORY1 = if_else(TERRITORY1 == "Entire Pacific region", "**Entire Pacific region**", TERRITORY1)) %>% 
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

ggsave("figs/01_part-1/fig-4.png", height = 10.5, width = 5, dpi = fig_resolution)

# 4. Southern Oscillation Index ----

## 4.1 Load and transform data ----

data_enso <- read_table("data/09_misc/nino34.long.anom.data.txt", skip = 1, col_names = FALSE, n_max = 154) %>% 
  rename(year = 1) %>% 
  pivot_longer(2:ncol(.), values_to = "nino", names_to = "month") %>% 
  mutate(month = str_remove_all(month, "X"),
         month = as.numeric(month)-1,
         date = ym(paste(year, month, sep = "-")),
         nino_roll = roll_mean(x = nino, n = 6, align = "center", fill = NA)) %>% 
  filter(year >= 1980)

## 4.2 Make the plot ----

plot_enso <- ggplot() +
  annotate(geom = "segment", x = ym("1980-01"), xend = ym("2023-12"), y = 1.5, linetype = "dashed") +
  annotate(geom = "segment", x = ym("1980-01"), xend = ym("2023-12"), y = -1.5, linetype = "dashed") +
  geom_ribbon(data = data_enso %>% mutate(nino_roll = if_else(nino_roll < 0, 0, nino_roll)),
              aes(x = date, ymin = 0, ymax = nino_roll), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_enso %>% mutate(nino_roll = if_else(nino_roll > 0, 0, nino_roll)),
              aes(x = date, ymin = 0, ymax = nino_roll), fill = palette_first[3], alpha = 0.9) +
  labs(x = "Year", y = "Niño 3.4 SST Index") +
  # Annotation
  annotate(geom = "rect", xmin = ym("1985-09"), xmax = ym("1994-05"),
           ymin = 2.2, ymax = 2.8, fill = palette_second[3], color = NA) +
  annotate(geom = "text", x = ym("1990-01"), y = 2.5, color = "white",
           label = "El Niño", family = font_choose_graph, size = 4) +
  annotate(geom = "rect", xmin = ym("2010-10"), xmax = ym("2019-03"),
           ymin = -2.2, ymax = -2.8, fill = palette_first[3], color = NA) +  
  annotate(geom = "text", x = ym("2015-01"), y = -2.5, color = "white",
           label = "La Niña", family = font_choose_graph, size = 4) +
  scale_y_continuous(limits = c(-3, 3), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  coord_cartesian(clip = "off")

## 4.3 Save the plot ----

ggsave("figs/01_part-1/fig-5.png", plot = plot_enso, height = 4, width = 5, dpi = fig_resolution)

# 5. SST anomaly ----

## 5.1 Load and transform data ----

load("data/09_misc/data-sst_processed.RData")

data_sst_pacific <- data_sst %>% 
  filter(TERRITORY1 == "Entire Pacific region") %>% 
  drop_na(sst_anom_mean) %>% 
  mutate(date = as_date(date))

## 5.2 Make the plot ----

plot_anom <- ggplot(data = data_sst_pacific) +
  geom_ribbon(data = data_sst_pacific %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_pacific %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

## 5.3 Save the plot ----

ggsave("figs/01_part-1/fig-6a.png", plot = plot_anom, height = 4, width = 5, dpi = fig_resolution)

# 6. SST anomaly with trend ----

## 6.1 Load and transform data ----

load("data/09_misc/data-sst_processed.RData")

data_sst_pacific <- data_sst %>% 
  filter(TERRITORY1 == "Entire Pacific region") %>% 
  drop_na(sst_anom_mean)

data_sst_pacific <- data_sst_pacific %>% 
  mutate(date = as.numeric(as_date(date))) %>% 
  group_by(TERRITORY1) %>% 
  # Extract linear model coefficients
  group_modify(~extract_coeff(data = .x, var_y = "sst_anom_mean", var_x = "date")) %>% 
  ungroup() %>% 
  left_join(data_sst_pacific, .) %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_anom_mean_linear = slope*date_num+intercept)

## 6.2 Make the plot ----

plot_anom_trend <- ggplot(data = data_sst_pacific) +
  geom_ribbon(data = data_sst_pacific %>% mutate(sst_anom_mean = if_else(sst_anom_mean < sst_anom_mean_linear,
                                                                         sst_anom_mean_linear,
                                                                         sst_anom_mean)),
              aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_pacific %>% mutate(sst_anom_mean = if_else(sst_anom_mean > sst_anom_mean_linear,
                                                                         sst_anom_mean_linear,
                                                                         sst_anom_mean)),
              aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = sst_anom_mean_linear)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

## 6.3 Save the plot ----

ggsave("figs/01_part-1/fig-6b.png", plot = plot_anom_trend, height = 4, width = 5, dpi = fig_resolution)

# 7. Combine the two SST anomaly plots ----

(plot_anom + labs(title = "A", x = NULL) + theme(plot.title = element_text(face = "bold"))) +
  (plot_anom_trend + labs(title = "B") + theme(plot.title = element_text(face = "bold"))) + plot_layout(ncol = 1)

ggsave("figs/01_part-1/fig-6.png", height = 8, width = 5, dpi = fig_resolution)

# 8. Comparison of SST distribution ----

## 8.1 Transform data ----

data_sst <- data_sst %>% 
  filter(!(TERRITORY1 %in% c("Entire Pacific region", "Pacific Remote Island Area", "Kiribati"))) %>% 
  group_by(TERRITORY1) %>% 
  summarise(mean = mean(sst)) %>% 
  ungroup() %>% 
  left_join(., data_sst)

## 8.2 Make the plot ----

ggplot(data = data_sst, aes(x = sst, y = fct_reorder(TERRITORY1, mean))) +
  geom_violin(draw_quantiles = c(0.5), fill = "#446CB3", alpha = 0.5) +
  labs(x = "SST (°C)", y = NULL) +
  theme_graph() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(20, 32.5))

## 8.3 Save the plot ----

ggsave("figs/04_supp/01_data-explo/02_sst-distribution.png", height = 8, width = 6, dpi = fig_resolution)

# 9. Map of SST anomaly per year ----

## 9.1 Load data ----

data_map <- read_sf("data/01_background-shp/01_ne/ne_10m_land/ne_10m_land.shp")

data_eez <- read_sf("data/01_background-shp/03_eez/data_eez.shp")

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

## 9.2 List of files ----

data_files <- tibble(path = list.files("data/08_sst-anom-year/", full.names = TRUE)) %>% 
  mutate(year = as.numeric(str_sub(path, -8, -4)),
         group = rep(1:50, each = 8, length.out = nrow(.))) # 8 is the number of subplots (i.e. years) per plot

## 9.3 Create the function to make the plot for each year ----

map_ssta_year <- function(year_i, data_files_i){
  
  # 1. Load data
  
  raster <- rast(data_files_i %>% filter(year == year_i) %>% select(path) %>% pull)
  
  # 2. Make the plot
  
  ggplot() +
    geom_spatraster(data = raster) +
    geom_sf(data = data_eez, fill = NA) +
    geom_sf(data = data_map, fill = "#363737", col = "grey") +
    scale_fill_gradientn(colours = c(rev(palette_first), "white", palette_second),
                         limits = c(-5, 5),
                         name = "Yearly average SST anomaly (°C)",
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
          legend.key.width = unit(2.5, "cm"),
          legend.key.height = unit(0.4, "cm"),
          legend.position = "bottom")
  
}

## 9.4 Create the function to make the plot for each group ----

map_ssta_plot <- function(group_i){
  
  # 1. Filter the data_files
  
  data_files_i <- data_files %>% 
    filter(group == group_i)
  
  # 2. Create all the plots
  
  plots <- map(c(data_files_i$year), ~map_ssta_year(data_files_i = data_files_i, year_i = .))
  
  # 3. Combine plots
  
  combined_plots <- wrap_plots(plots) + 
    plot_layout(guides = "collect", ncol = 2) & 
    theme(legend.position = "bottom")
  
  # 4. Save the plot
  
  ggsave(filename = paste0("figs/04_supp/03_indicators/map_sst-anom_", min(data_files_i$year), "-", max(data_files_i$year), ".png"),
         height = 13, width = 9, combined_plots, dpi = 600)
  
}

## 9.5 Map over the function ----

map(unique(data_files$group), ~map_ssta_plot(group_i = .))
