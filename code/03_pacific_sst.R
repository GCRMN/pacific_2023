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

# 3.1 Load and transform data --

data_warming <- read.csv2("figs/sst_indicators.csv") %>% 
  mutate(warming_rate = round(warming_rate, 3),
         color = if_else(sst_increase > 0, "#d64541", "#446CB3")) %>% 
  arrange(desc(sst_increase))

# 3.2 Make the plot --

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

# 3.3 Save the plot --

ggsave("figs/warming-rate-territories.png", height = 8, width = 8)

# 4. DHW ----

# 4.1 Max DHW --

load("data/09_data-dhw.RData")

data_dhw <- data_dhw %>% 
  group_by(date) %>% 
  summarise(max_dhw = max(dhw, na.rm = TRUE))

plot_a <- ggplot(data = data_dhw, aes(x = date, y = max_dhw)) +
  geom_line(color = "black", linewidth = 0.25) +
  labs(x = "Year", y = "Max. DHW (°C-weeks)", title = "A") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

# 4.2 DHW percent --

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

# 4.3 Combine the plots --

plot_a + plot_b + plot_layout(ncol = 2)

# 4.4 Save the plot --

ggsave(filename = "figs/dhw-pacific.png", width = 10, height = 4, dpi = 600)
