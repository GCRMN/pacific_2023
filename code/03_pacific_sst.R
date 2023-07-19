# 1. Load packages ----

library(tidyverse)
library(glue)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Load and transform data ----

data_warming <- read.csv2("figs/sst_indicators.csv") %>% 
  mutate(warming_rate = round(warming_rate, 3),
         color = if_else(sst_increase > 0, "#d64541", "#446CB3")) %>% 
  arrange(desc(sst_increase))

# 4. Make the plot ----

ggplot(data = data_warming, aes(x = sst_increase, y = fct_reorder(TERRITORY1, sst_increase))) +
  # Global mean SST change
  # The number 0.88°C is coming from Technical summary IPCC, 2021 (TS.2.4, The Ocean, page 74)
  geom_vline(xintercept = (0.88/(2020-1900))*(2022-1980), color = "#34495e", linetype = "dashed") + 
  geom_segment(aes(xend = 0, yend = fct_reorder(TERRITORY1, sst_increase))) +
  geom_point(aes(fill = color), shape = 21, size = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_vline(xintercept = 0) +
  annotate("curve", x = (0.88/(2020-1900))*(2022-1980), xend = 0.5, y = 3, yend = 2, curvature = 0.25,
           arrow = arrow(length = unit(0.015, "npc"), ends = "first"), color = "#34495e") +
  annotate("text", x = 0.52, y = 2.1, label = "Global mean SST ocean change",
           family = font_choose_graph, hjust = 0, size = 3, color = "#34495e") +
  labs(x = "Change in SST (°C) between 1980 and 2022", y = NULL) +
  lims(x = c(-0.45, 1.25))

# 5. Save the plot ----

ggsave("figs/warming-rate-territories.png", height = 8, width = 8)
