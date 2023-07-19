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
  geom_segment(aes(xend = 0, yend = fct_reorder(TERRITORY1, sst_increase))) +
  geom_point(aes(fill = color), shape = 21, size = 3) +
  scale_fill_identity() +
  scale_color_identity() +
  geom_vline(xintercept = 0) +
  labs(x = "Change in SST (°C) between 1980 and 2022", y = NULL) +
  lims(x = c(-0.45, 1.25))

# 5. Save the plot ----

ggsave("figs/warming-rate-territories.png", height = 8, width = 8)
