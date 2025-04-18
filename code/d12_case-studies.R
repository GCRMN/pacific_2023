# 1. Load packages ----

library(tidyverse)
library(readxl)
library(sf)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
colors <- c(palette_first[5], palette_first[3])

load("data/01_background-shp/02_princeton/data_land.RData")

# 3. Case study for New Caledonia ----

## 3.1 Load and transform data ----

data_ouano <- read_xlsx("data/13_case-studies/BACI Poissons Ouano 04_20.xlsx", sheet = 1) %>% 
  rename(year = "Année") %>% 
  group_by(year, Protection) %>% 
  summarise(biomass_mean = mean(B_Com),
            biomass_se = sd(B_Com)/sqrt(n()),
            richness_mean = mean(Sr_com),
            richness_se = sd(Sr_com)/sqrt(n()))

## 3.2 Biomass of commercial species ----

plot_a <- ggplot() +
  geom_segment(aes(x = 2006.5, y = 0, xend = 2006.5, yend = 78), linetype = "dashed", color = "darkgrey") +
  geom_curve(aes(x = 2006.5, y = 80, xend = 2008.2, yend = 88),
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type = "open"), curvature = -0.35,
             col = "black", lineend = "round") + 
  annotate(geom = "text", x = 2008.5, y = 88, label = "Implementation\nof the MPA",
           hjust = 0, family = font_choose_graph, color = colors[2]) +
  geom_errorbar(data = data_ouano, aes(x = year, 
                                       ymin = biomass_mean - biomass_se, 
                                       ymax = biomass_mean + biomass_se, color = Protection),
                width = 0.3, show.legend = FALSE) +
  geom_line(data = data_ouano, aes(x = year, y = biomass_mean, color = Protection),
            show.legend = FALSE) +
  geom_point(data = data_ouano, aes(x = year, y = biomass_mean, fill = Protection),
             size = 3, shape = 21, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  annotate(geom = "text", x = 2016.5, y = 65, label = "Within MPA",
           family = font_choose_graph, color = colors[2]) +
  annotate(geom = "text", x = 2017.5, y = 17, label = "Outside MPA", 
           family = font_choose_graph, color = colors[1]) +
  labs(x = "Year", y = expression(paste("Biomass (", g.m^{-1}, ") per station")), title = "A") +
  lims(y = c(0, 100)) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        axis.line.y = element_line(linewidth = 0.4, color = "black"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 100), expand = expansion(mult = c(0.05, 0)))

## 3.3 Species richness of commercial species ----

plot_b <- ggplot() +
  geom_segment(aes(x = 2006.5, y = 4.5, xend = 2006.5, yend = 18), linetype = "dashed", color = "darkgrey") +
  geom_curve(aes(x = 2006.5, y = 4, xend = 2009, yend = 2.5),
             arrow = arrow(length = unit(0.02, "npc"), ends = "first", type = "open"), curvature = 0.35,
             col = "black", lineend = "round") + 
  annotate(geom = "text", x = 2009.5, y = 2.5, label = "Implementation\nof the MPA",
           hjust = 0, family = font_choose_graph, color = colors[2]) +
  geom_errorbar(data = data_ouano, aes(x = year, 
                                       ymin = richness_mean - richness_se, 
                                       ymax = richness_mean + richness_se, color = Protection),
                width = 0.3, show.legend = FALSE) +
  geom_line(data = data_ouano, aes(x = year, y = richness_mean, color = Protection),
            show.legend = FALSE) +
  geom_point(data = data_ouano, aes(x = year, y = richness_mean, fill = Protection),
             size = 3, shape = 21, color = "white", show.legend = FALSE) +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  annotate(geom = "text", x = 2017.5, y = 15, label = "Within MPA",
           family = font_choose_graph, color = colors[2]) +
  annotate(geom = "text", x = 2017.5, y = 8, label = "Outside MPA", 
           family = font_choose_graph, color = colors[1]) +
  labs(x = "Year", y = "Species richness per station", title = "B") +
  lims(y = c(0, 20)) +
  theme_graph() +
  theme(plot.background = element_rect(fill = "transparent", color = "transparent"),
        axis.ticks.y = element_line(linewidth = 0.4, color = "black"),
        axis.line.y = element_line(linewidth = 0.4, color = "black"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        axis.text = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, 20), expand = expansion(mult = c(0.05, 0)))

## 3.4 Combine plots ----

plot_a + plot_b &
  plot_annotation(theme = theme(plot.background = element_rect(fill = "transparent", colour = NA_character_),
                                panel.border = element_rect(fill = NA, color = NA)))

ggsave("figs/02_part-2/case-studies/new-caledonia_2.png",
       bg = "transparent", width = 10, height = 4.5, dpi = 300)

## 3.5 Map ----

data_land_ouano <- data_land %>% 
  filter(TERRITORY1 == "New Caledonia")

data_ouano <- tibble(lon = 165.779879, lat = -21.872524, site = "OUANO") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = data_land_ouano) +
  geom_sf(data = data_ouano, fill = colors[2], color = "white", size = 10, shape = 21) +
  geom_sf_label(data = data_ouano, aes(label = site), fill = colors[2], size = 7, label.padding = unit(7, "pt"),
                color = "white", family = font_choose_graph, nudge_x = 0.5, nudge_y = 0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA)) +
  coord_sf(x = c(163, 168.6), y = c(-23, -19))

ggsave("figs/02_part-2/case-studies/new-caledonia_1.png", bg = "transparent")

## 3.6 Remove useless objects ----

rm(data_land_ouano, data_ouano, plot_a, plot_b)

# 4. Case study for Guam ----

## 4.1 Figure ----

data_guam <- read_xlsx("data/13_case-studies/socmon_guam.xlsx", sheet = 1, range = "H4:M8") %>% 
  rename(value = 1) %>% 
  pivot_longer(2:ncol(.), names_to = "reply", values_to = "perc") %>% 
  mutate(value = factor(value, c("Frequently", "Sometimes", "Rarely", "Never")),
         reply = case_when(reply == "To give to extended family members and/or friends" ~ "To give to extended family\nmembers and/or friends",
                           reply == "Feed myself and my family/household" ~ "Feed myself and my\nfamily/household",
                           reply == "For special occasions and cultural events" ~ "For special occasions\nand cultural events",
                           TRUE ~ reply),
         perc = perc*100,
         perc_label = round(perc, 0),
         perc_label = if_else(perc_label < 8, "", paste0(as.character(perc_label), "%")),
         text_color = ifelse(value %in% c("Rarely", "Never"), "white", "black"))

ggplot(data = data_guam, aes(x = reply, y = perc, fill = value, label = perc_label, color = text_color)) +
  geom_bar(stat = "identity", width = 0.75,
           position = position_stack(reverse = TRUE), color = "white", linewidth = 0.05) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 3) + 
  coord_flip() +
  scale_color_identity() +
  scale_fill_manual(breaks = c("Frequently", "Sometimes", "Rarely", "Never"),
                    values = c("#ce6693", "#f8a07e", "#7393C9", "#2C5D96")) +
  scale_x_discrete(limits = rev) +
  labs(x = NULL, y = "Percentage of respondents", fill = "Frequency") +
  theme_graph() +
  theme(legend.position = "right",
        legend.direction = "vertical")

ggsave("figs/02_part-2/case-studies/guam_2.png", width = 10, height = 4, bg = "transparent")

## 4.2 Map ----

data_land_guam <- data_land %>% 
  filter(TERRITORY1 == "Guam")

data_habitat_guam <- st_read("data/13_case-studies/guam_site_revised.shp") %>% 
  filter(row_number() != 3) %>% 
  st_union()

data_habitat_guam_sea <- st_read("data/13_case-studies/guam_site_revised.shp") %>% 
  filter(row_number() == 3) %>% 
  st_union()

data_habitat_guam_label <- tibble(lon = 144.35, lat = 12.85, site = "MANELL-GEUS") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

ggplot() +
  geom_sf(data = data_habitat_guam_sea, fill = colors[2], alpha = 0.25, color = NA) +
  geom_sf(data = data_land_guam) +
  geom_sf(data = data_habitat_guam, fill = colors[2]) +
  geom_sf_label(data = data_habitat_guam_label, aes(label = site), fill = colors[2],
                size = 3, label.padding = unit(7, "pt"),
                color = "white", family = font_choose_graph, nudge_x = 0.5, nudge_y = 0.4) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/02_part-2/case-studies/guam_1.png", bg = "transparent")

## 4.3 Remove useless objects ----

rm(data_land_guam, data_guam)

# 5. Case study for Yap ----

## 5.1 Figure ----

data_yap <- read.csv("data/13_case-studies/FSM_Weloy_CSV.csv") %>% 
  select("Questionnaire_ID", "condition_coral", "condition_fish",
         "condition_shell", "condition_invertebrate",
         "condition_crabs", "condition_salt_water") %>% 
  pivot_longer(2:ncol(.), names_to = "question", values_to = "value") %>% 
  mutate(question = str_remove_all(question, "condition_|_"),
         question = str_replace_all(question, c("coral" = "Hard corals",
                                                "fish" = "Fishes",
                                                "shell" = "Shells",
                                                "invertebrate" = "Invertebrates",
                                                "crabs" = "Crabs",
                                                "saltwater" = "Sea water")),
         value = as.character(value),
         # Codes taken from the file "FSM_Weloy_codebook.xlsx"
         value = str_replace_all(value, c("1" = "A lot worse",
                                          "2" = "Somewhat worse",
                                          "3" = "Stayed the same",
                                          "4" = "Somewhat better",
                                          "5" = "A lot better",
                                          "8" = "Don't know"))) %>% 
  group_by(question, value) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(question) %>% 
  mutate(perc = (n*100)/sum(n)) %>% 
  ungroup() %>% 
  mutate(value = factor(value, c("A lot worse", "Somewhat worse",
                                 "Stayed the same", "Somewhat better",
                                 "A lot better", "Don't know")),
         perc_label = round(perc, 0),
         perc_label = if_else(perc_label < 8, "", paste0(as.character(perc_label), "%")),
         text_color = ifelse(value %in% c("A lot worse", "Somewhat better",
                                          "A lot better"), "white", "black"))

ggplot(data = data_yap, aes(x = question, y = perc, fill = value, label = perc_label, color = text_color)) +
  geom_bar(stat = "identity", width = 0.75,
           position = position_stack(reverse = TRUE), color = "white", linewidth = 0.05) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 3) + 
  coord_flip() +
  scale_color_identity() +
  scale_fill_manual(breaks = c("A lot worse", "Somewhat worse",
                               "Stayed the same", "Somewhat better",
                               "A lot better", "Don't know"),
                    values = c("#ce6693", "#f8a07e", "#B2BBCC", "#7393C9", "#2C5D96", "#efeff0")) +
  scale_x_discrete(limits = rev) +
  labs(x = NULL, y = "Percentage of respondents", fill = "Perceived\ncondition") +
  theme_graph() +
  theme(legend.position = "right",
        legend.direction = "vertical")

ggsave("figs/02_part-2/case-studies/federated-states-of-micronesia_2.png",
       width = 10, height = 4, bg = "transparent")

## 5.2 Map ----

data_land_yap <- st_read("data/13_case-studies/yap_municipality_poly_approximate.shp") %>% 
  mutate(color = if_else(Name == "Weloy", colors[2], "lightgrey"))

ggplot() +
  geom_sf(data = data_land_yap, aes(fill = color)) +
  geom_sf_label(data = data_land_yap %>% filter(Name == "Weloy"),
                aes(label = "WELOY"), fill = colors[2], size = 7, label.padding = unit(7, "pt"),
                color = "white", family = font_choose_graph, nudge_x = -4000, nudge_y = 2000) +
  theme_minimal() +
  scale_fill_identity() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))

ggsave("figs/02_part-2/case-studies/federated-states-of-micronesia_1.png", bg = "transparent")

## 5.3 Remove useless objects ----

rm(data_land_yap)

# 6. Case study for American Samoa ----

## 6.1 Load and transform data ----

data_2014 <- read.csv("data/13_case-studies/NCRMP-Socio-AmericanSamoa-2014_Data.csv") %>% 
  select(condition_1, condition_2, condition_3, condition_4) %>% 
  pivot_longer(1:4, values_to = "reply", names_to = "question") %>% 
  mutate(year = 2014,
         question = str_replace_all(question, c("condition_1" = "Ocean water quality",
                                                "condition_2" = "Amount of live coral",
                                                "condition_3" = "Number of fish",
                                                "condition_4" = "Amount of marine resources for gleaning")),
         reply = as.character(reply),
         reply = str_replace_all(reply, c("1" = "Very bad",
                                          "2" = "Bad",
                                          "3" = "Neither good nor bad",
                                          "4" = "Good",
                                          "5" = "Very good",
                                          "8" = "Not sure")))

data_2021 <- read.csv("data/13_case-studies/NCRMP_Socio_AS_2021_Data.csv") %>% 
  select(current_1, current_2, current_3, current_J4) %>% 
  pivot_longer(1:4, values_to = "reply", names_to = "question") %>% 
  mutate(year = 2021,
         question = str_replace_all(question, c("current_1" = "Ocean water quality",
                                                "current_2" = "Amount of live coral",
                                                "current_3" = "Number of fish",
                                                "current_J4" = "Amount of marine resources for gleaning")),
         reply = as.character(reply),
         reply = str_replace_all(reply, c("1" = "Very bad",
                                          "2" = "Bad",
                                          "3" = "Neither good nor bad",
                                          "4" = "Good",
                                          "5" = "Very good",
                                          "88" = "Not sure",
                                          "777" = NA_character_)))

data_amsamoa <- bind_rows(data_2014, data_2021) %>% 
  drop_na(reply) %>% 
  filter(reply != ".") %>% 
  group_by(question, year, reply) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(question, year) %>% 
  mutate(freq = (n*100)/sum(n)) %>% 
  ungroup() %>% 
  mutate(year = factor(year, c("2021", "2014")),
         reply = factor(reply, c("Very bad", "Bad", "Neither good nor bad", "Good",
                                 "Very good", "Not sure")),
         freq_label = round(freq, 0),
         freq_label = if_else(freq_label < 8, "", paste0(as.character(freq_label), "%")),
         text_color = ifelse(reply %in% c("Very bad", "Good", "Very good"), "white", "black"))

## 6.2 Make the plot ----

ggplot(data = data_amsamoa, aes(x = year, y = freq, fill = reply, label = freq_label, color = text_color)) +
  geom_bar(stat = "identity", width = 0.75,
           position = position_stack(reverse = TRUE), color = "white", linewidth = 0.05) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 3) + 
  coord_flip() +
  scale_color_identity() +
  facet_wrap(~question, ncol = 1) +
  scale_fill_manual(breaks = c("Very bad", "Bad", "Neither good nor bad", "Good",
                               "Very good", "Not sure"),
                    values = c("#ce6693", "#f8a07e", "#B2BBCC", "#7393C9", "#2C5D96", "#efeff0")) +
  labs(x = NULL, y = "Percentage of respondents", fill = "Perceived\ncondition") +
  theme_graph() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.text = element_text(hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = NA))

ggsave("figs/02_part-2/case-studies/american-samoa_2.png", bg = "transparent", height = 9, width = 8)

## 6.3 Remove useless objects ----

rm(data_amsamoa, data_2014, data_2021)

# 7. Case study for Hawaii ----

## 7.1 Load and transform data ----

data_2015 <- read.csv("data/13_case-studies/NCRMP-Socio-Hawaii-2015_Data.csv") %>% 
  select(condition_1, condition_2, condition_3, condition_7) %>% 
  pivot_longer(1:4, values_to = "reply", names_to = "question") %>% 
  mutate(year = 2015,
         question = str_replace_all(question, c("condition_1" = "Ocean water quality",
                                                "condition_2" = "Amount of live coral",
                                                "condition_3" = "Number of fish",
                                                "condition_7" = "Variety of fishes")),
         reply = as.character(reply),
         reply = str_replace_all(reply, c("1" = "Very bad",
                                          "2" = "Bad",
                                          "3" = "Neither good nor bad",
                                          "4" = "Good",
                                          "5" = "Very good",
                                          "8" = "Not sure")))

data_2020 <- read.csv("data/13_case-studies/NCRMP-Socio_HI-2020_Data.csv") %>% 
  select(current_1, current_2, current_3, current_4) %>% 
  pivot_longer(1:4, values_to = "reply", names_to = "question") %>% 
  mutate(year = 2020,
         question = str_replace_all(question, c("current_1" = "Ocean water quality",
                                                "current_2" = "Amount of live coral",
                                                "current_3" = "Number of fish",
                                                "current_4" = "Variety of fishes")),
         reply = as.character(reply),
         reply = str_replace_all(reply, c("1" = "Very bad",
                                          "2" = "Bad",
                                          "3" = "Neither good nor bad",
                                          "4" = "Good",
                                          "5" = "Very good",
                                          "88" = "Not sure",
                                          "777" = NA_character_)))

data_hawaii <- bind_rows(data_2015, data_2020) %>% 
  drop_na(reply) %>% 
  filter(reply != ".") %>% 
  group_by(question, year, reply) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(question, year) %>% 
  mutate(freq = (n*100)/sum(n)) %>% 
  ungroup() %>% 
  mutate(year = factor(year, c("2020", "2015")),
         reply = factor(reply, c("Very bad", "Bad", "Neither good nor bad", "Good",
                                 "Very good", "Not sure")),
         freq_label = round(freq, 0),
         freq_label = if_else(freq_label < 8, "", paste0(as.character(freq_label), "%")),
         text_color = ifelse(reply %in% c("Very bad", "Good", "Very good"), "white", "black"))

## 7.2 Make the plot ----

ggplot(data = data_hawaii, aes(x = year, y = freq, fill = reply, label = freq_label, color = text_color)) +
  geom_bar(stat = "identity", width = 0.75,
           position = position_stack(reverse = TRUE), color = "white", linewidth = 0.05) +
  geom_text(position = position_stack(vjust = 0.5, reverse = TRUE),
            family = font_choose_graph, size = 3) + 
  coord_flip() +
  scale_color_identity() +
  facet_wrap(~question, ncol = 1) +
  scale_fill_manual(breaks = c("Very bad", "Bad", "Neither good nor bad", "Good",
                               "Very good", "Not sure"),
                    values = c("#ce6693", "#f8a07e", "#B2BBCC", "#7393C9", "#2C5D96", "#efeff0")) +
  labs(x = NULL, y = "Percentage of respondents", fill = "Perceived\ncondition") +
  theme_graph() +
  theme(legend.position = "right",
        legend.direction = "vertical",
        strip.text = element_text(hjust = 0, face = "bold"),
        strip.background = element_rect(fill = NA, color = NA))

ggsave("figs/02_part-2/case-studies/hawaii_2.png", bg = "transparent", height = 9, width = 8)

## 7.3 Remove useless objects ----

rm(data_hawaii, data_2015, data_2020)

# 8. Case study for the 4 GBE ----

read_xlsx("data/13_case-studies/data_crw.xlsx") %>% 
  mutate(dhw_4_to_8 = dhw_4_to_8 - dhw_8_to_12 - dhw_12_more,
         dhw_8_to_12 = dhw_8_to_12 - dhw_12_more) %>% 
  pivot_longer(2:ncol(.), names_to = "dhw", values_to = "value") %>% 
  mutate(dhw = as.factor(dhw),
         dhw = fct_relevel(dhw, "dhw_4_to_8", "dhw_8_to_12", "dhw_12_more")) %>% 
  ggplot(data = ., aes(x = year, y = value, fill = dhw)) +
  geom_bar(stat = "identity") +
  theme_graph() +
  lims(y = c(0, 60)) +
  scale_fill_manual(breaks = c("dhw_4_to_8", "dhw_8_to_12", "dhw_12_more"),
                    labels = c("4 ≥ DHW < 8", "8 ≥ DHW < 12", "DHW ≥ 12"),
                    values = c("#7393C9", "#ce6693", "#f8a07e"),
                    name = "Maximum Degree<br>Heating Weeks (DHW)") +
  labs(x = "Year", y = "Proportion of reef pixels") +
  theme(legend.title = element_markdown(),
        legend.direction = "vertical",
        legend.position = c(0.31, 0.725),
        legend.background = element_rect("white"))

ggsave("figs/02_part-2/case-studies/crw_1.png", bg = "transparent", height = 5, width = 8)
