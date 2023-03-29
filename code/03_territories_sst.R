# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(RcppRoll)
library(sf)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Load data ----

load("data/07_data_sst.RData")
load("data/01_background-shp/03_eez/data_eez.RData")

# 4. Transform data ----

data_sst <- data_eez %>% 
  # Join to add TERRITORY1
  st_drop_geometry() %>% 
  select(TERRITORY1, GEONAME) %>% 
  left_join(data_sst, .) %>% 
  # Calculate SST anomaly
  group_by(TERRITORY1) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE),
         sst_anom = sst - mean_sst,
         sst_anom_mean = roll_mean(x = sst_anom, n = 365, align = "center", fill = NA)) %>% 
  ungroup() %>% 
  drop_na(TERRITORY1)

# 5. Extract indicators (sst increase, warming rate, and mean sst) ----

# 5.1 Create the function --

extract_coeff <- function(data){
  
  model <- lm(sst ~ date, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

# 5.2 Map over the function --

data_warming <- data_sst %>% 
  # Convert date as numeric
  mutate(date = as.numeric(as_date(date))) %>% 
  # Extract linear model coefficients
  group_by(TERRITORY1) %>% 
  group_modify(~extract_coeff(data = .x)) %>% 
  ungroup() %>% 
  # Calculate increase in SST over the period
  mutate(min_date = as.numeric(as_date(min(data_sst$date))),
         max_date = as.numeric(as_date(max(data_sst$date)))) %>% 
  mutate(sst_increase = ((max_date)*slope+intercept) - ((min_date)*slope+intercept)) %>% 
  select(-min_date, -max_date) %>% 
  # Calculate the warming rate (°C per year)
  mutate(warming_rate = sst_increase/(year(max(data_sst$date))-year(min(data_sst$date)))) %>% 
  # Add mean_sst for each territory
  left_join(., data_sst %>% 
              select(TERRITORY1, mean_sst) %>% 
              distinct())

# 5.3 Export results --

write.csv2(data_warming, file = "figs/sst_indicators.csv", row.names = FALSE)

# 6. Make the plots for each territory ----

# 6.1 Create the function --

map_sst <- function(territory_i){
  
  # 1. Filter SST data ----
  
  data_sst_i <- data_sst %>% 
    filter(TERRITORY1 == territory_i) %>% 
    mutate(daymonth = str_sub(date, 6, 10),
           year = year(date))
  
  # 2. Make the plot ----
  
  # 2.1 SST --
  
  plot_a <- ggplot(data = data_sst_i, aes(x = date, y = sst)) +
    geom_line(color = "black", linewidth = 0.25) +
    geom_smooth(method = "lm", se = FALSE, color = "#446CB3") +
    geom_hline(yintercept = unique(data_sst_i$mean_sst), linetype = "dashed", color = "#d64541", linewidth = 1) +
    labs(x = "Year", y = "SST (°C)", title = "A")
  
  # 2.2 SST anomaly --
  
  plot_b <- ggplot(data = data_sst_i, aes(x = date, y = sst_anom_mean)) +
    geom_line(color = "black", linewidth = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    labs(x = "Year", y = "SST anomaly (°C)", title = "B")
  
  # 2.3 SST by year --
  
  data_sst_i_mean <- data_sst_i %>% 
    group_by(daymonth) %>% 
    summarise(sst = mean(sst, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(year = "all")
  
  plot_c <- ggplot() +
    geom_line(data = data_sst_i, aes(x = daymonth, y = sst, group = year), color = "grey", alpha = 0.75, linewidth = 0.5) +
    geom_line(data = data_sst_i_mean, aes(x = daymonth, y = sst, group = year), color = "black", linewidth = 1) +
    scale_color_identity() +
    scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                                "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                     labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", 
                                "Sep.", "Oct.", "Nov.", "Dec.")) +
    labs(x = "Month", y = "SST (°C)", title = "C") + 
    theme_graph() +
    theme(axis.text.x = element_text(size = 8))
  
  # 2.4 Combine the plot --
  
  plot_a + plot_b + plot_c + plot_layout(ncol = 1)
  
  # 3. Export the plot ----
  
  ggsave(filename = paste0("figs/territories_fig-2/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 5, height = 10, dpi = 600)
  
}

# 6.2 Map over the function --

map(unique(data_sst$TERRITORY1), ~map_sst(territory_i = .))
