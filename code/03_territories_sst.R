# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(RcppRoll)
library(sf)
library(patchwork)

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/map_sst.R")

theme_set(theme_graph())

# 2. Load data ----

load("data/07_data_sst.RData")

load("data/01_background-shp/03_eez/data_eez.RData")

data_sst <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, GEONAME) %>% 
  left_join(data_sst, .) %>% 
  group_by(TERRITORY1) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE),
         sst_anom = sst - mean_sst,
         sst_anom_mean = roll_mean(x = sst_anom, n = 365, align = "center", fill = NA)) %>% 
  ungroup()

# 3. 

map(unique(data_eez$TERRITORY1), ~map_sst(territory_i = .))





data_sst2 <- data_sst %>% 
  filter(TERRITORY1 == "French Polynesia") 
  
















data_sst2 <- data_sst %>% 
  filter(TERRITORY1 == "French Polynesia") 

  
ggplot(data = data_sst2, aes(x = date, y = sst)) +
  geom_line(color = "black", linewidth = 0.25) +
  geom_hline(yintercept = unique(data_sst2$mean_sst), linetype = "dashed", color = "#d64541", linewidth = 1) +
  labs(x = "Year", y = "SST (°C)", title = "A")

ggplot(data = data_sst2, aes(x = date, y = sst_anom_mean)) +
  geom_line(color = "black", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  labs(x = "Year", y = "SST anomaly (°C)", title = "B")


























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
  labs(x = "Year", y = "SST (°C)")

# SST anomaly

ggplot(data = data_sst, aes(x = date, y = sst_anom_mean)) +
  geom_line(color = "black", linewidth = 0.25) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
  labs(x = "Year", y = "SST anomaly (°C)")


ggsave(filename = "figs/02_sst-time-series-1.png", height = 3, width = 8)












###### TEST ######


results <- data_sst %>% 
  filter(TERRITORY1 %in% c("American Samoa", "Cook Islands")) %>% 
  nest(data = -TERRITORY1) %>% 
  mutate(model = map(data, ~lm(sst ~ date, data = .)),
         model = map(model, broom::tidy)) %>% 
  unnest(model)



test <- data_sst %>% 
  filter(TERRITORY1 %in% c("Cook Islands"))

ggplot(data = test, aes(x = date, y = sst)) +
  geom_line(color = "black", linewidth = 0.25) +
  geom_abline(intercept = 2.698746e+01, slope = 4.990322e-10) +
  labs(x = "Year", y = "SST anomaly (°C)")










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
