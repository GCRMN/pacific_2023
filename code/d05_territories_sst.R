# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(lubridate)
library(RcppRoll)
library(sf)
library(scales)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/extract_coeff.R")

theme_set(theme_graph())

# 3. Load data ----

data_warming <- read.csv("data/09_misc/data-warming.csv") 

# 4. Export tables ----

## 4.1 Transform the table ----

data_table_3 <- data_warming %>% 
  rename(territory = TERRITORY1) %>% 
  mutate(subterritory = territory,
         territory = case_when(subterritory %in% c("Line Group", "Phoenix Group", "Gilbert Islands") ~ "Kiribati",
                               subterritory %in% c("Jarvis Island", "Johnston Atoll", 
                                                   "Wake Island", "Howland and Baker Islands",
                                                   "Palmyra Atoll") ~ "Pacific Remote Island Area",
                               TRUE ~ subterritory),
         subterritory = if_else(subterritory == territory, NA, subterritory)) %>% 
  arrange(territory, !is.na(subterritory)) %>% 
  relocate(subterritory, .after = territory) %>% 
  filter(territory != "Entire Pacific region") %>% 
  bind_rows(., data_warming %>% 
              filter(TERRITORY1 == "Entire Pacific region") %>% 
              rename(territory = TERRITORY1)) %>% 
  select(territory, subterritory, mean_sst, sst_increase, warming_rate) %>% 
  mutate(mean_sst = format(round(mean_sst, 2), nsmall = 2),
         sst_increase = format(round(sst_increase, 2), nsmall = 2),
         warming_rate = format(round(warming_rate, 3), nsmall = 2))

## 4.2 Export as .xlsx ----

openxlsx::write.xlsx(data_table_3, file = "figs/01_part-1/table-3.xlsx")

## 4.3 Export as LaTeX ----

latex_table_line <- function(i, subterritory){
  
  color <- ifelse(i %% 2 == 0, "white", "secondcolor")
  
  if(subterritory == FALSE){
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{2}{|l|}{", data_table_3[i, "territory"], "} &", data_table_3[i, "mean_sst"], "&",
                     data_table_3[i, "sst_increase"], "&", data_table_3[i, "warming_rate"]," \\\\ \\hline"))
    
  }else{
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{1}{|l}{} & ", data_table_3[i, "subterritory"], " &", data_table_3[i, "mean_sst"], "&",
                     data_table_3[i, "sst_increase"], "&", data_table_3[i, "warming_rate"]," \\\\ \\hline"))
    
  }
  
  return(line)
  
}

writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|R{2.85cm}|R{2.85cm}|R{2.85cm}|}",
             "\\hline",
             "\\rowcolor{firstcolor}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{Countries and territories}} & \\textcolor{white}{Mean SST ($^\\circ$C)} & \\textcolor{white}{SST change 1985-2023 ($^\\circ$C)}  & \\textcolor{white}{Warming rate ($^\\circ$C.year\\textsuperscript{-1})} \\\\ \\hline",
             map(1:8, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(9:11, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(12:17, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(18:22, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(23:32, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             paste0("\\rowcolor{secondcolor}"),
             paste0("\\multicolumn{2}{|l|}{\\textbf{", data_table_3[33, "territory"], "}} &", data_table_3[33, "mean_sst"], "&",
                    data_table_3[33, "sst_increase"], "&", data_table_3[33, "warming_rate"]," \\\\ \\hline"),
             "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/01_part-1/table-3.tex"))

# 5. SST (year) for each territory ----

load("data/09_misc/data-sst_processed.RData")

data_sst <- data_sst %>% 
  filter(!(TERRITORY1 %in% c("Entire Pacific region", "Pacific Remote Island Area", "Kiribati")))

data_sst <- data_sst %>% 
  left_join(., data_warming %>% select(-mean_sst), by = "TERRITORY1") %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_linear = slope*date_num+intercept) %>% 
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

ggsave(filename = "figs/04_supp/03_indicators/01_sst_a.png", width = 10, height = 12, dpi = fig_resolution)

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

ggsave(filename = "figs/04_supp/03_indicators/01_sst_b.png", width = 10, height = 12, dpi = fig_resolution)

# 6. SST (month) for each territory ----

data_sst_month <- data_sst %>% 
  mutate(daymonth = str_sub(date, 6, 10),
         year = year(date),
         decade = case_when(year >= 1980 & year < 1990 ~ "1980s",
                            year >= 1990 & year < 2000 ~ "1990s",
                            year >= 2000 & year < 2010 ~ "2000s",
                            year >= 2010 & year < 2020 ~ "2010s",
                            year >= 2020 & year < 2030 ~ "2020s")) %>% 
  arrange(decade) %>% 
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, "Federated States of Micronesia", "Fed. States of Micronesia"))

data_sst_month_mean <- data_sst_month %>% 
  group_by(daymonth, TERRITORY1) %>% 
  summarise(sst = mean(sst, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(year = "all")

ggplot() +
  geom_line(data = data_sst_month %>% filter(TERRITORY1 %in% sort(unique(data_sst_month$TERRITORY1))[1:15]),
            aes(x = daymonth, y = sst, group = year, color = decade),
            alpha = 0.75, linewidth = 0.5) +
  geom_line(data = data_sst_month_mean %>% filter(TERRITORY1 %in% sort(unique(data_sst_month$TERRITORY1))[1:15]),
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
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
  facet_wrap(~TERRITORY1, ncol = 3, scales = "free_y")

ggsave(filename = "figs/04_supp/03_indicators/02_sst-month_a.png", width = 10, height = 13, dpi = fig_resolution)

ggplot() +
  geom_line(data = data_sst_month %>% filter(TERRITORY1 %in% sort(unique(data_sst_month$TERRITORY1))[16:30]),
            aes(x = daymonth, y = sst, group = year, color = decade),
            alpha = 0.75, linewidth = 0.5) +
  geom_line(data = data_sst_month_mean %>% filter(TERRITORY1 %in% sort(unique(data_sst_month$TERRITORY1))[16:30]),
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
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = ".")) +
  facet_wrap(~TERRITORY1, ncol = 3, scales = "free_y")

ggsave(filename = "figs/04_supp/03_indicators/02_sst-month_b.png", width = 10, height = 13, dpi = fig_resolution)

# 7. SST anomaly (trend) for each territory ----

load("data/09_misc/data-sst_processed.RData")

data_sst <- data_sst %>% 
  filter(!(TERRITORY1 %in% c("Entire Pacific region", "Pacific Remote Island Area", "Kiribati"))) %>% 
  drop_na(sst_anom_mean)

data_sst <- data_sst %>% 
  mutate(date = as.numeric(as_date(date))) %>% 
  group_by(TERRITORY1) %>% 
  # Extract linear model coefficients
  group_modify(~extract_coeff(data = .x, var_y = "sst_anom_mean", var_x = "date")) %>% 
  ungroup() %>% 
  left_join(data_sst, .) %>% 
  mutate(date_num = as.numeric(as_date(date)),
         sst_anom_mean_linear = slope*date_num+intercept)

## 7.1 Create the function for the base plot ----

base_plot <- function(territory_i){
  
  data_i <- data_sst %>% 
    filter(TERRITORY1 %in% territory_i)
  
  plot_i <- ggplot(data = data_i) +
    geom_ribbon(data = data_i %>% mutate(sst_anom_mean = if_else(sst_anom_mean < sst_anom_mean_linear,
                                                                 sst_anom_mean_linear,
                                                                 sst_anom_mean)),
                aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
    geom_ribbon(data = data_i %>% mutate(sst_anom_mean = if_else(sst_anom_mean > sst_anom_mean_linear,
                                                                 sst_anom_mean_linear,
                                                                 sst_anom_mean)),
                aes(x = date, ymin = sst_anom_mean_linear, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
    geom_line(aes(x = date, y = sst_anom_mean_linear)) +
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
      facet_wrap(~TERRITORY1, ncol = 3, scales = "free")
    
    ggsave(filename = "figs/02_part-2/fig-2/pria.png", width = 15, height = 9, dpi = fig_resolution)
    
  }else if(group_territory_i == "Kiribati"){
    
    base_plot(territory_i = c("Gilbert Islands", "Line Group", "Phoenix Group")) +
      facet_wrap(~TERRITORY1, scales = "free_x")
    
    ggsave(filename = "figs/02_part-2/fig-2/kiribati.png", width = 15, height = 6, dpi = fig_resolution)
    
  }else{
    
    base_plot(territory_i = group_territory_i)
    
    ggsave(filename = paste0("figs/02_part-2/fig-2/",
                             str_replace_all(str_to_lower(group_territory_i), " ", "-"), ".png"),
           width = 6, height = 4, dpi = fig_resolution)
    
  }
  
}

## 7.3 Map over the function ----

map(data_sst %>% 
      select(TERRITORY1) %>% 
      distinct() %>% 
      filter(!(TERRITORY1 %in% c("Gilbert Islands", "Line Group", "Phoenix Group",
                                 "Palmyra Atoll", "Howland and Baker Islands", "Johnston Atoll",
                                 "Jarvis Island", "Wake Island"))) %>% 
      bind_rows(., tibble(TERRITORY1 = c("PRIA", "Kiribati"))) %>% 
      pull(),
    ~map_sst_anom(.))

# 8. SST anomaly for each territory ----

load("data/09_misc/data-sst_processed.RData")

data_sst <- data_sst %>% 
  filter(!(TERRITORY1 %in% c("Entire Pacific region", "Pacific Remote Island Area", "Kiribati"))) %>% 
  mutate(TERRITORY1 = str_replace_all(TERRITORY1, "Federated States of Micronesia", "Fed. States of Micronesia"))

data_sst_anom <- data_sst %>% 
  drop_na(sst_anom_mean) %>%
  filter(TERRITORY1 %in% sort(unique(data_sst$TERRITORY1))[1:15])

ggplot(data = data_sst_anom) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                               0,
                                                               sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  facet_wrap(~TERRITORY1, ncol = 3, scales = "free_y") +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

ggsave(filename = "figs/04_supp/03_indicators/03_sst-anom_a.png", width = 10, height = 12, dpi = fig_resolution)

data_sst_anom <- data_sst %>% 
  drop_na(sst_anom_mean) %>%
  filter(TERRITORY1 %in% sort(unique(data_sst$TERRITORY1))[16:30])

ggplot(data = data_sst_anom) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean < 0,
                                                                      0,
                                                                      sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill = palette_second[3], alpha = 0.9) +
  geom_ribbon(data = data_sst_anom %>% mutate(sst_anom_mean = if_else(sst_anom_mean > 0,
                                                                      0,
                                                                      sst_anom_mean)),
              aes(x = date, ymin = 0, ymax = sst_anom_mean), fill =  palette_first[3], alpha = 0.9) +
  geom_line(aes(x = date, y = 0)) +
  facet_wrap(~TERRITORY1, ncol = 3, scales = "free_y") +
  theme_graph() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 14)) +
  labs(x = "Year", y = "SST anomaly (°C)") +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, decimal.mark = "."))

ggsave(filename = "figs/04_supp/03_indicators/03_sst-anom_b.png", width = 10, height = 12, dpi = fig_resolution)

# 9. Max SST anom values (for writing) ----

load("data/09_misc/data-sst_processed.RData")

data_sst_anom_values <- data_sst %>% 
  mutate(year = year(date)) %>% 
  group_by(TERRITORY1, year) %>% 
  summarise(mean_sst_anom = max(sst_anom_mean, na.rm = TRUE)) %>% 
  filter(TERRITORY1 == "French Polynesia")
