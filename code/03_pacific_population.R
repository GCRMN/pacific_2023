# 1. Load packages ----

library(tidyverse)
library(scico)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_population <- read.csv("data/14_human-pop_reef.csv") %>% 
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
  scale_fill_manual(values = rev(scico(5, begin = 0, end = 0.8, palette = "lajolla"))) +
  labs(x = "Year", y = "Number of inhabitants (millions)") +
  annotate(geom = "text", label = "Papua New Guinea", x = 2003, y = 1.4, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Other territories", x = 2003, y = 3.75, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Hawaii", x = 2003, y = 5.3, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Fiji", x = 2003, y = 6.4, 
           family = font_choose_graph, color = "white", hjust = 0) +
  theme_graph() + 
  lims(y = c(0, 10))

# 5. Export the plot ----

ggsave("figs/pacific_human-pop.png", height = 5, width = 6, dpi = 600)
