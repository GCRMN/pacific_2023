# 1. Load packages ----

library(tidyverse)
library(glue)
library(ggtext)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Comparison of warming rates ----

## 3.1 Load and transform data ----

data_warming <- read.csv2("figs/01_part-1/table-3.csv") %>% 
  select(TERRITORY1, warming_rate, sst_increase) %>% 
  # The number 0.88°C is coming from Technical summary IPCC, 2021 (TS.2.4, The Ocean, page 74)
  add_row(TERRITORY1 = "Global Ocean", warming_rate = (0.88/(2020-1900))*(2022-1980), sst_increase = 0.88) %>% 
  mutate(warming_rate = round(warming_rate, 3),
         TERRITORY1 = if_else(TERRITORY1 == "Global Ocean", "**Global Ocean**", TERRITORY1),
         color = if_else(sst_increase > 0, "#d24d57", "#5c97bf")) %>% 
  arrange(desc(sst_increase)) 

## 3.2 Make the plot ----

ggplot(data = data_warming, aes(x = sst_increase, y = fct_reorder(TERRITORY1, sst_increase))) +
  geom_bar(stat = "identity", aes(fill = color), width = 0.6, color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_vline(xintercept = 0) +
  labs(x = "Change in SST (°C) between 1980 and 2023", y = NULL) +
  theme_graph() +
  theme(axis.text.y = element_markdown()) +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(-0.5, 1.5))

## 3.3 Save the plot ----

ggsave("figs/01_part-1/fig-4.png", height = 8, width = 6, dpi = 600)

# 4. DHW ----

## 4.1 Max DHW ----

load("data/09_data-dhw.RData")

data_dhw <- data_dhw %>% 
  group_by(date) %>% 
  summarise(max_dhw = max(dhw, na.rm = TRUE))

plot_a <- ggplot(data = data_dhw, aes(x = date, y = max_dhw)) +
  geom_line(color = "black", linewidth = 0.25) +
  labs(x = "Year", y = "Max. DHW (°C-weeks)", title = "A") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

## 4.2 DHW percent ----

load("data/10_data-dhw-percent.RData")

data_dhw_percent <- data_dhw_percent %>% 
  filter(territory == "Pacific" & dhw_type != "DHW = 0")

plot_b <- ggplot(data = data_dhw_percent, aes(x = date, y = freq, fill = dhw_type)) +
  geom_area(stat = "identity", position = "identity") +
  scale_y_continuous(limits = c(0, 110), breaks = c(0, 25, 50, 75, 100)) +
  scale_fill_manual(breaks = c("0 < DHW < 4", "4 <= DHW < 8", "DHW >= 8"), 
                    values = c("#2c82c9", "#fabe58", "#d64541"), name = NULL) +
  labs(x = "Year", y = "Percent of coral reefs", title = "B") +
  theme(legend.direction = "horizontal",
        legend.position = c(0.5, 0.925),
        legend.background = element_blank())

## 4.3 Combine the plots ----

plot_a + plot_b + plot_layout(ncol = 2)

## 4.4 Save the plot ----

ggsave(filename = "figs/dhw-pacific.png", width = 10, height = 4, dpi = 600)

# 5. Southern Oscillation Index ----

## 5.1 Load and transform data ----

data_enso <- read_table("data/enso_soi.txt", skip = 87) %>% 
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
         color = if_else(soi < 0, "#d64541", "#446CB3"))

## 5.2 Make the plot ----

ggplot(data = data_enso, aes(x = date, y = soi, fill = color)) +
  geom_bar(stat = "identity", width = 30) +
  scale_fill_identity() +
  labs(x = "Year", y = "Southern Oscillation Index") +
  # Annotation
  annotate(geom = "rect", xmin = ym("1987-06"), xmax = ym("1992-08"),
           ymin = 2.3, ymax = 2.65, fill = "#446CB3", color = NA) +
  annotate(geom = "text", x = ym("1990-01"), y = 2.5, color = "white",
           label = "La Niña", family = font_choose_graph, size = 4) +
  annotate(geom = "rect", xmin = ym("2012-07"), xmax = ym("2017-06"),
           ymin = -3.2, ymax = -2.825, fill = "#d64541", color = NA) +  
  annotate(geom = "text", x = ym("2015-01"), y = -3, color = "white",
           label = "El Niño", family = font_choose_graph, size = 4) +
  scale_y_continuous(limits = c(-3.5, 3.5), breaks = c(-3, -2, -1, 0, 1, 2, 3)) +
  scale_x_continuous(limits = c(ym("1980-01"), ym("2030-01")),
                     breaks = c(ym("1980-01"), ym("1990-01"), ym("2000-01"),
                                ym("2010-01"), ym("2020-01"), ym("2030-01")),
                     labels = c("1980", "1990", "2000", "2010", "2020", "2030"),
                     expand = c(0, 0)) +
  theme_graph() +
  coord_cartesian(clip = "off")

## 5.3 Save the plot ----

ggsave("figs/01_part-1/fig-5.png", height = 5, width = 7, dpi = 600)

# 6. Comparison of SST distribution ----

## 6.1 Transform data ----

load("data/07_data_sst.RData")

data_sst <- data_sst %>% 
  group_by(TERRITORY1) %>% 
  summarise(mean = mean(sst)) %>% 
  ungroup() %>% 
  left_join(., data_sst)

## 6.2 Make the plot ----

ggplot(data = data_sst, aes(x = sst, y = fct_reorder(TERRITORY1, mean))) +
  geom_violin(draw_quantiles = c(0.5), fill = "#446CB3", alpha = 0.5) +
  labs(x = "SST (°C)", y = NULL) +
  theme_graph() +
  coord_cartesian(clip = "off") +
  scale_x_continuous(expand = c(0, 0), limits = c(20, 32.5))

## 6.3 Save the plot ----

ggsave("figs/05_additional/02_sst-distribution.png", height = 8, width = 6, dpi = 600)
