# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 2. Load data ----

load("data/07_data_sst.RData")

data_sst <- data_sst %>% 
  group_by(GEONAME) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup()

# 3. Compare SST regime per territory ----

ggplot(data = data_sst, aes(x = sst, y = fct_reorder(GEONAME, mean_sst))) +
  geom_violin(draw_quantiles = c(0.5), fill = "#52b3d9") +
  labs(x = "Sea Surface Temperature (°C)", y = NULL)

ggsave(filename = "figs/02_sst-regime-comparison.png", height = 10, width = 8)

# 4. Plot time-series per territory ----

data_sst <- data_sst %>% 
  filter(GEONAME == "Tokelau Exclusive Economic Zone") %>% 
  group_by(GEONAME) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup()

ggplot(data = data_sst, aes(x = date, y = sst)) +
  geom_line(color = "black", linewidth = 0.25) +
  geom_hline(yintercept = unique(data_sst$mean_sst), linetype = "dashed", color = "#d64541", linewidth = 1) +
  labs(x = "Year", y = "Sea Surface Temperature (°C)")

ggsave(filename = "figs/02_sst-time-series-1.png", height = 4, width = 10)












###### TEST ######



data_sst_values <- data_sst %>% 
  group_by(GEONAME) %>% 
  summarise(mean = mean(sst, na.rm = TRUE),
            sd = sd(sst, na.rm = TRUE),
            min = min(sst, na.rm = TRUE),
            max = max(sst, na.rm = TRUE))


data_sst_values <- data_sst %>% 
  nest(GEONAME) %>% 
  mutate(model = map(data, ~lm(sst ~ date, data = .)),
         model = map(model, broom::tidy)) %>% 
  unnest(model)


U <- data_sst %>% 
  group_by(GEONAME) %>% 
  nest() %>% 
  mutate(model = map(data, ~lm(sst ~ date, data = .)))




# 3. Make the plot ----

ggplot(data = data_sst, aes(x = date, y = sst)) +
  geom_line() +
  facet_wrap(~GEONAME)
