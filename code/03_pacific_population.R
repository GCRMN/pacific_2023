# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Load data ----

data_population <- read.csv("data/02_geo-inf/01_human-pop.csv") %>% 
  rename(population = sum, territory = TERRITORY1, year = date) %>% 
  mutate(year = year(year)) %>% 
  group_by(year) %>% 
  summarise(population = sum(population)) %>% 
  ungroup() %>% 
  mutate(population = population*1e-06) # Convert to million

# 4. Make the plot ----

ggplot(data = data_population, aes(x = year, y = population)) +
  geom_line() +
  geom_point(size = 4, shape = 21, fill = "#446cb3", color = "white") +
  labs(x = "Year", y = "Number of inhabitants (millions)") +
  theme_graph()

# 5. Export the plot ----

ggsave("figs/pacific_human-pop.png", height = 5, width = 6, dpi = 600)
