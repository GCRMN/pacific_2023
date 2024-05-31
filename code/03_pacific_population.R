# 1. Load packages ----

library(tidyverse)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_population <- read.csv("data/02_indicators/ind_human-pop_5km.csv") %>% 
  group_by(TERRITORY1, date) %>% 
  summarise(population = sum(sum)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4))) %>% 
  rename(territory = TERRITORY1) %>% 
  select(-date) %>% 
  mutate(population = population*1e-06, # Convert to million
         territory_type = if_else(territory %in% c("Papua New Guinea", "Fiji", "Hawaii"),
                                  territory, 
                                  "Other territories")) %>% 
  group_by(territory_type, year) %>% 
  summarise(population = sum(population, na.rm = TRUE)) %>% 
  ungroup()

# 4. Make the plot ----

ggplot(data = data_population, aes(x = year, y = population, fill = territory_type)) +
  geom_area(show.legend = FALSE) +
  scale_fill_manual(values = rev(palette_first)) +
  labs(x = "Year", y = "Inhabitants (millions)") +
  annotate(geom = "text", label = "Papua New Guinea", x = 2003, y = 0.45, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Other territories", x = 2003, y = 1.75, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Hawaii", x = 2003, y = 3.1, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Fiji", x = 2003, y = 3.8, 
           family = font_choose_graph, color = "white", hjust = 0) +
  theme_graph() + 
  lims(y = c(0, 6)) +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.025)), limits = c(2000, 2020))

# 5. Export the plot ----

ggsave("figs/01_part-1/fig-3.png", height = 4, width = 5, dpi = fig_resolution)
