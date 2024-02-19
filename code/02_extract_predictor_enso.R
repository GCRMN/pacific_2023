# 1. Load packages ----

library(tidyverse)
library(RcppRoll)

# 2. Load and transform SOI data ----

read_table("data/enso_soi.txt", skip = 87) %>% 
  filter(YEAR %in% c(1980:2023)) %>% 
  select(-X14) %>% 
  mutate_all(., ~as.numeric(.)) %>% 
  pivot_longer(2:ncol(.), values_to = "soi", names_to = "month") %>% 
  rename(year = YEAR) %>% 
  mutate(month = str_replace_all(month, c("JAN" = "1",
                                          "FEB" = "2",
                                          "MAR" = "3",
                                          "APR"= "4",
                                          "MAY"= "5",
                                          "JUN" = "6",
                                          "JUL"= "7",
                                          "AUG" = "8",
                                          "SEP" = "9",
                                          "OCT" = "10",
                                          "NOV" = "11",
                                          "DEC"= "12")),
         date = ym(paste(year, month, sep = "-"))) %>% 
  # Mean of SOI value over the last 24 months (i.e. 2 years)
  mutate(enso = roll_mean(x = soi, n = 24, align = "right", fill = NA)) %>% 
  filter(month == "12") %>% 
  select(year, enso) %>% 
  write.csv(., file = "data/14_predictors/pred_enso.csv", row.names = FALSE)
