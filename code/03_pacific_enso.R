# 1. Load packages ----

library(tidyverse)
library(glue)
library(ggtext)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

theme_set(theme_graph())

# 3. Load and transform data ----

data_enso <- read_table("data/enso_soi.txt", skip = 87) %>% 
  filter(YEAR %in% c(1980:2022)) %>% 
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
         date = ym(paste(year, month, sep = "-")),
         color = if_else(soi < 0, "#d64541", "#446CB3"))

# 4. Make the plot ----

ggplot(data = data_enso, aes(x = date, y = soi, fill = color)) +
  geom_bar(stat = "identity") +
  scale_fill_identity() +
  labs(x = "Year", y = "Southern Oscillation Index") +
  geom_hline(yintercept = 0) +
  annotate(geom = "text", x = ym("1990-01"), y = 3, color = "#446CB3",
           label = "La Niña", family = font_choose_graph, size = 5) +
  annotate(geom = "text", x = ym("2015-01"), y = -3, color = "#d64541",
           label = "El Niño", family = font_choose_graph, size = 5) +
  lims(y = c(-3.5, 3.5))

# 5. Save the plot ----

ggsave("figs/pacific_enso-soi-index.png", height = 4, width = 8)
