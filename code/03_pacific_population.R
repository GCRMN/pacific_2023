# 1. Load packages ----

library(tidyverse)
library(sf)
library(scico)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_population <- read.csv("data/02_geo-inf/01_human-pop.csv") %>% 
  rename(population = sum, territory = TERRITORY1, year = date) %>% 
  mutate(year = year(year)) %>% 
  mutate(population = population*1e-06, # Convert to million
         territory_type = if_else(territory %in% c("Papua New Guinea", "Fiji", "Hawaii"),
                                  territory, 
                                  "Other territories")) %>% 
  group_by(territory_type, year) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()

# 4. Make the plot ----

ggplot(data = data_population, aes(x = year, y = population, fill = territory_type)) +
  geom_area(show.legend = FALSE) +
  scale_fill_manual(values = scico(5, begin = 0.3, end = 1, palette = "lajolla")) +
  labs(x = "Year", y = "Number of inhabitants (millions)") +
  annotate(geom = "text", label = "Papua New Guinea", x = 2003, y = 3, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Other territories", x = 2003, y = 7, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Hawaii", x = 2003, y = 8.5, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Fiji", x = 2003, y = 9.4, 
           family = font_choose_graph, color = "white", hjust = 0) +
  theme_graph() + 
  lims(y = c(0, 15)) +
  guides(fill = guide_legend(reverse=TRUE))

# 5. Export the plot ----

ggsave("figs/pacific_human-pop.png", height = 5, width = 6, dpi = 600)
