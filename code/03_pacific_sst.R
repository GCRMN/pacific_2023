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
         color = if_else(sst_increase > 0, "#d64541", "#446CB3"),
         warming_rate_label = glue("<b><span style='color:{color}'>{warming_rate}</span></b> °C.y<sup>-1</sup>"),
         label_x = if_else(sst_increase > 0, sst_increase + 0.18, sst_increase - 0.18)) %>% 
  arrange(desc(sst_increase))

# 4. Make the plot ----

ggplot(data = data_warming) +
  geom_segment(aes(x = 0, 
                   xend = sst_increase, 
                   y = fct_reorder(TERRITORY1, sst_increase), 
                   yend = fct_reorder(TERRITORY1, sst_increase),
                   color = color)) +
  geom_point(aes(x = sst_increase, y = fct_reorder(TERRITORY1, sst_increase), fill = color), 
             shape = 21, size = 4, color = "white") +
  scale_fill_identity() +
  scale_color_identity() +
  geom_richtext(aes(x = label_x, y = fct_reorder(TERRITORY1, sst_increase), label = warming_rate_label),
                size = 3, label.padding = grid::unit(rep(0, 4), "pt"), fill = NA, label.color = NA, family = font_choose_graph) +
  geom_vline(xintercept = 0) +
  labs(x = "Change in SST (°C) between 1980 and 2022", y = NULL) +
  lims(x = c(-0.45, 1.4))

# 5. Save the plot ----

ggsave("figs/warming-rate-territories.png", height = 8, width = 8)
