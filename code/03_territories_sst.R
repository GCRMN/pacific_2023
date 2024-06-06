# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(RcppRoll)
library(sf)
library(scales)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Load data ----

load("data/09_misc/data-sst.RData")

# 4. Calculate SST anomaly ----

data_sst <- data_sst %>% 
  group_by(TERRITORY1) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE),
         sst_anom = sst - mean_sst,
         sst_anom_mean = roll_mean(x = sst_anom, n = 365, align = "center", fill = NA)) %>% 
  ungroup()

# 5. Extract SST indicators ----

## 5.1 Calculate long-term average SST ----

data_sst <- data_sst %>% 
  group_by(TERRITORY1) %>% 
  mutate(mean_sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup()

## 5.2 Create the function ----

extract_coeff <- function(data){
  
  model <- lm(sst ~ date, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

## 5.3 Map over the function ----

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

## 5.4 Export the data ----

write.csv2(data_warming, file = "figs/01_part-1/table-3.csv", row.names = FALSE)

## 5.5 Transform data ----

data_sst <- left_join(data_sst, data_warming) %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_linear = slope*date_num+intercept)

# 6. SST (month) for each territory ----

## 6.1 Transform data ----

data_sst_month <- data_sst %>% 
  mutate(daymonth = str_sub(date, 6, 10),
         year = year(date),
         decade = case_when(year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            year >= 2020 & year < 2030 ~ "2020s")) %>% 
  arrange(decade)

data_sst_month_mean <- data_sst_month %>% 
  group_by(daymonth, TERRITORY1) %>% 
  summarise(sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = "all")

## 6.2 Create the function for the base plot ----

base_plot <- function(territory_i){
  
  plot_i <- ggplot() +
    geom_line(data = data_sst_month %>% filter(TERRITORY1 %in% territory_i),
              aes(x = daymonth, y = sst, group = year, color = decade),
              alpha = 0.75, linewidth = 0.5) +
    geom_line(data = data_sst_month_mean %>% filter(TERRITORY1 %in% territory_i),
              aes(x = daymonth, y = sst, group = year),
              color = "black", linewidth = 1) +
    scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                                "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                     labels = c("", "Feb.", "", "Apr.", "", "Jun.", "", "Aug.", 
                                "", "Oct.", "", "Dec.")) +
    labs(x = "Month", y = "SST (°C)") + 
    theme(legend.title.position = "top",
          legend.title = element_text(hjust = 0.5),
          strip.background = element_blank(),
          strip.text = element_text(size = 14)) +
    scale_color_manual(name = "Decade", values = palette_second) +
    guides(color = guide_legend(override.aes = list(linewidth = 1))) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))
  
  return(plot_i)
  
}

## 6.3 Create the function to produce the plots ----

map_sst_month <- function(group_territory_i){
  
  if(group_territory_i == "PRIA"){
    
    base_plot(territory_i = c("Palmyra Atoll", "Howland and Baker Islands",
                              "Johnston Atoll", "Jarvis Island", "Wake Island")) +
      facet_wrap(~TERRITORY1, ncol = 3, scales = "free_x")
    
    ggsave(filename = "figs/02_part-2/fig-2/pria.png", width = 15, height = 9, dpi = fig_resolution)
    
  }else if(group_territory_i == "Kiribati"){
    
    base_plot(territory_i = c("Gilbert Islands", "Line Group", "Phoenix Group")) +
      facet_wrap(~TERRITORY1)
    
    ggsave(filename = "figs/02_part-2/fig-2/kiribati.png", width = 15, height = 6, dpi = fig_resolution)
    
  }else{
    
    base_plot(territory_i = group_territory_i)
    
    ggsave(filename = paste0("figs/02_part-2/fig-2/",
                             str_replace_all(str_to_lower(group_territory_i), " ", "-"), ".png"),
           width = 5, height = 4.5, dpi = fig_resolution)
    
  }
  
}

## 6.4 Map over the function ----

map(data_sst_month %>% 
      select(TERRITORY1) %>% 
      distinct() %>% 
      filter(!(TERRITORY1 %in% c("Gilbert Islands", "Line Group", "Phoenix Group",
                                 "Palmyra Atoll", "Howland and Baker Islands", "Johnston Atoll",
                                 "Jarvis Island", "Wake Island"))) %>% 
      bind_rows(., tibble(TERRITORY1 = c("PRIA", "Kiribati"))) %>% 
      pull(),
    ~map_sst_month(.))

## 6.5 Remove useless functions ----

rm(base_plot, map_sst_month, extract_coeff)

# 7. SST anomaly for each territory ----

## 7.1 Create the function for the base plot ----

base_plot <- function(territory_i){
  
  data_i <- data_sst %>% 
    filter(TERRITORY1 == territory_i) %>% 
    drop_na(sst_anom_mean)
  
  plot_i <- ggplot(data = data_i, aes(x = date, y = sst_anom_mean)) +
    geom_ribbon(data = data_i %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0, 0, sst_anom_mean)),
                aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_first[3], alpha = 0.9) +
    geom_ribbon(data = data_i %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0, 0, sst_anom_mean)),
                aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_first[4], alpha = 0.9) +
    geom_line(color = "black", linewidth = 0.3) +
    geom_hline(yintercept = 0, color = "black") +
    scale_fill_identity() +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 14)) +
    labs(x = "Year", y = "SST anomaly (°C)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))
  
  return(plot_i)
  
}

## 7.2 Create the function to produce the plots ----

map_sst_anom <- function(group_territory_i){
  
  if(group_territory_i == "PRIA"){
    
    base_plot(territory_i = c("Palmyra Atoll", "Howland and Baker Islands",
                              "Johnston Atoll", "Jarvis Island", "Wake Island")) +
      facet_wrap(~TERRITORY1, ncol = 3, scales = "free_x")
    
    ggsave(filename = "figs/02_part-2/fig-3/pria.png", width = 15, height = 9, dpi = fig_resolution)
    
  }else if(group_territory_i == "Kiribati"){
    
    base_plot(territory_i = c("Gilbert Islands", "Line Group", "Phoenix Group")) +
      facet_wrap(~TERRITORY1)
    
    ggsave(filename = "figs/02_part-2/fig-3/kiribati.png", width = 15, height = 6, dpi = fig_resolution)
    
  }else{
    
    base_plot(territory_i = group_territory_i)
    
    ggsave(filename = paste0("figs/02_part-2/fig-3/",
                             str_replace_all(str_to_lower(group_territory_i), " ", "-"), ".png"),
           width = 6, height = 4, dpi = fig_resolution)
    
  }
  
}

## 7.3 Map over the function ----

map(data_sst_month %>% 
      select(TERRITORY1) %>% 
      distinct() %>% 
      filter(!(TERRITORY1 %in% c("Gilbert Islands", "Line Group", "Phoenix Group",
                                 "Palmyra Atoll", "Howland and Baker Islands", "Johnston Atoll",
                                 "Jarvis Island", "Wake Island"))) %>% 
      bind_rows(., tibble(TERRITORY1 = c("PRIA", "Kiribati"))) %>% 
      pull(),
    ~map_sst_anom(.))

## 7.4 Remove useless functions ----

rm(base_plot, map_sst_anom)

# 8. SST (year) for each territory ----

data_sst <- data_sst %>% 
  filter(TERRITORY1 %in% unique(data_sst$TERRITORY1)) %>%  
  mutate(daymonth = str_sub(date, 6, 10),
         year = year(date))

ggplot(data = data_sst %>% 
           filter(TERRITORY1 %in% sort(unique(data_sst$TERRITORY1))[1:15]),
         aes(x = date, y = sst)) +
    geom_line(color = "#2c3e50", linewidth = 0.25) +
    geom_line(aes(x = date, y = sst_linear), color = palette_second[2], linewidth = 0.8) +
    geom_hline(aes(yintercept = mean_sst), color = palette_second[4], linewidth = 0.8) +
    labs(x = "Year", y = "Sea Surface Temperature (°C)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) + 
    facet_wrap(~TERRITORY1, ncol = 3, scales = "free") +
    theme_graph() +
    theme(strip.text = element_text(hjust = 0.5),
          strip.background = element_blank())

ggsave(filename = "figs/04_supp/03_indicators/02_sst_a.png", width = 10, height = 12, dpi = 300)

ggplot(data = data_sst %>% 
         filter(TERRITORY1 %in% sort(unique(data_sst$TERRITORY1))[16:30]),
       aes(x = date, y = sst)) +
  geom_line(color = "#2c3e50", linewidth = 0.25) +
  geom_line(aes(x = date, y = sst_linear), color = palette_second[2], linewidth = 0.8) +
  geom_hline(aes(yintercept = mean_sst), color = palette_second[4], linewidth = 0.8) +
  labs(x = "Year", y = "Sea Surface Temperature (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) + 
  facet_wrap(~TERRITORY1, ncol = 3, scales = "free") +
  theme_graph() +
  theme(strip.text = element_text(hjust = 0.5),
        strip.background = element_blank())

ggsave(filename = "figs/04_supp/03_indicators/02_sst_b.png", width = 10, height = 12, dpi = 300)
