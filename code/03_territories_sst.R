# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(RcppRoll)
library(sf)
library(patchwork)
library(scales)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Load data ----

load("data/07_data_sst.RData")
load("data/09_data_dhw.RData")
load("data/10_data-dhw-percent.RData")
load("data/01_background-shp/03_eez/data_eez.RData")

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

# 6. Make the plots of SST (year) for each territory ----

## 6.1 Transform data ----

data_sst <- left_join(data_sst, data_warming) %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_linear = slope*date_num+intercept)

## 6.2 Create the function ----

map_sst_year <- function(territory_i){
  
  data_sst_i <- data_sst %>% 
    filter(TERRITORY1 == territory_i) %>% 
    mutate(daymonth = str_sub(date, 6, 10),
           year = year(date))

  ggplot(data = data_sst_i, aes(x = date, y = sst)) +
    geom_line(color = "#2c3e50", linewidth = 0.25) +
    geom_line(aes(x = date, y = sst_linear), color = palette_5cols[2], linewidth = 0.8) +
    geom_hline(yintercept = unique(data_sst_i$mean_sst), linetype = "dashed",
               color = palette_5cols[4], linewidth = 0.8) +
    labs(x = "Year", y = "SST (°C)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

  ggsave(filename = paste0("figs/02_part-2/fig-3/",
                           str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 4, dpi = 600)
  
}

## 6.3 Map over the function ----

map(unique(data_sst$TERRITORY1), ~map_sst_year(territory_i = .))

# 7. Make the plots of SST (month) for each territory ----

## 7.1 Transform data ----

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

## 7.2 Create the function ----

map_sst_month <- function(territory_i, color_decade){
  
  if(color_decade == TRUE){
    
    ggplot() +
      geom_line(data = data_sst_month %>% filter(TERRITORY1 == territory_i),
                aes(x = daymonth, y = sst, group = year, color = decade),
                alpha = 0.75, linewidth = 0.5) +
      geom_line(data = data_sst_month_mean %>% filter(TERRITORY1 == territory_i),
                aes(x = daymonth, y = sst, group = year),
                color = "black", linewidth = 1) +
      scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                                  "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                       labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", 
                                  "Sep.", "Oct.", "Nov.", "Dec.")) +
      labs(x = "Month", y = "SST (°C)") + 
      theme(axis.text.x = element_text(size = 8)) +
      scale_color_manual(name = "Decade", values = palette_5cols) +
      guides(color = guide_legend(override.aes = list(linewidth = 1))) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))
    
    ggsave(filename = paste0("figs/02_part-2/fig-4/",
                             str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           width = 5.5, height = 4.5, dpi = 600)
    
  }else{
    
    ggplot() +
      geom_line(data = data_sst_month %>% filter(TERRITORY1 == territory_i),
                aes(x = daymonth, y = sst, group = year),
                color = "grey", alpha = 0.75, linewidth = 0.5) +
      geom_line(data = data_sst_month_mean %>% filter(TERRITORY1 == territory_i),
                aes(x = daymonth, y = sst, group = year),
                color = "black", linewidth = 1) +
      scale_x_discrete(breaks = c("01-01", "02-01", "03-01", "04-01", "05-01", "06-01", 
                                  "07-01", "08-01", "09-01", "10-01", "11-01", "12-01"), 
                       labels = c("Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.", "Jul.", "Aug.", 
                                  "Sep.", "Oct.", "Nov.", "Dec.")) +
      labs(x = "Month", y = "SST (°C)") + 
      theme(axis.text.x = element_text(size = 8)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))
    
    ggsave(filename = paste0("figs/02_part-2/fig-4/",
                             str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           width = 6, height = 4, dpi = 600)
    
  }
  
}

## 7.3 Map over the function ----

map(unique(data_sst$TERRITORY1), ~map_sst_month(territory_i = ., color_decade = TRUE))

# 8. SST anomaly ----

data_sst %>% 
  filter(TERRITORY1 %in% unique(data_sst$TERRITORY1)[1:15]) %>% 
  ggplot(data = ., aes(x = date, y = sst_anom_mean)) +
    geom_line(color = "black", linewidth = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
    labs(x = "Year", y = "SST anomaly (°C)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
    facet_wrap(~TERRITORY1, scales = "free", ncol = 3) +
    theme(panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
          strip.background = element_rect(color = NA, fill = NA),
          strip.text = element_text(face = "bold", color = palette_5cols[4]))

ggsave(filename = "figs/04_supp/fig-1_a.png", width = 8.5, height = 11.5, dpi = 600)

data_sst %>% 
  filter(TERRITORY1 %in% unique(data_sst$TERRITORY1)[16:30]) %>% 
    ggplot(data = ., aes(x = date, y = sst_anom_mean)) +
    geom_line(color = "black", linewidth = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.75) +
    labs(x = "Year", y = "SST anomaly (°C)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
    facet_wrap(~TERRITORY1, scales = "free", ncol = 3) +
    theme(panel.border = element_rect(color = "black", linewidth = 1, fill = NA),
          strip.background = element_rect(color = NA, fill = NA),
          strip.text = element_text(face = "bold", color = palette_5cols[4]))

ggsave(filename = "figs/04_supp/fig-1_b.png", width = 8.5, height = 11.5, dpi = 600)

# 9. Make the plots of DHW for each territory ----

## 9.1 Create the function ----

map_dhw <- function(territory_i){
  
  data_dhw_percent %>% 
    filter(territory == territory_i) %>% 
    filter(dhw_type != "DHW = 0") %>% 
    mutate(year = year(date)) %>% 
    group_by(year, dhw_type) %>% 
    filter(freq == max(freq)) %>% 
    ungroup() %>% 
    select(-date) %>% 
    distinct() %>% 
    ggplot(data = ., aes(x = year, y = freq, fill = dhw_type)) +
    geom_bar(stat = "identity") +
    lims(y = c(0, 100)) +
    scale_fill_manual(breaks = c("0 < DHW < 4", "4 <= DHW < 8", "DHW >= 8"), 
                      labels = c("Possible bleaching", "Bleaching likely", "Mortality likely"),
                      values = c("#5c97bf", "#fbc093", "#c44d56"), name = NULL) +
    labs(x = "Year", y = "Percent of coral reefs") +
    theme(legend.position = "top")

  ggsave(filename = paste0("figs/02_part-2/fig-5/", 
                           str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 4, dpi = 600)

}
  
# 9.2 Map over the function ----

map(unique(data_dhw$territory), ~map_dhw(territory_i = .))

# 10 Maximum DHW per date and EEZ ----

data_dhw %>% 
  filter(territory %in% unique(data_dhw$territory)[1:15]) %>% 
  ggplot(data = ., aes(x = date, y = dhw)) +
    geom_line(color = "black", linewidth = 0.25) +
    labs(x = "Year", y = "Maximum DHW (°C-weeks)") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
    facet_wrap(~territory, scales = "free_y", ncol = 3)

ggsave(filename = "figs/04_supp/fig-2_a.png", width = 8, height = 10, dpi = 600)

data_dhw %>% 
  filter(territory %in% unique(data_dhw$territory)[16:30]) %>% 
  ggplot(data = ., aes(x = date, y = dhw)) +
  geom_line(color = "black", linewidth = 0.25) +
  labs(x = "Year", y = "Maximum DHW (°C-weeks)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
  facet_wrap(~territory, scales = "free_y", ncol = 3)

ggsave(filename = "figs/04_supp/fig-2_b.png", width = 8, height = 10, dpi = 600)
