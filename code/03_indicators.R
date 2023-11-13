# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(pdftools)
library(docxtractr)

# 2. Table 1 - Geographic information ----

# 2.1 Maritime area ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_maritime_area <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2) %>% 
  group_by(TERRITORY1) %>% 
  summarise(AREA_KM2 = sum(AREA_KM2)) %>% 
  ungroup() %>% 
  rename(territory = TERRITORY1, maritime_area = AREA_KM2)

# 2.2 Land area ----

data_land <- read.csv("data/02_indicators/ind_land.csv") %>% 
  rename(territory = TERRITORY1, land_area = sum)

# 2.3 Mean land elevation ----

data_elevation <- read.csv("data/02_indicators/ind_elevation.csv") %>% 
  rename(territory = TERRITORY1, mean_elevation = mean)

# 2.4 Reef area ----

data_reefs <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) 

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

ggplot() +
  geom_sf(data = data_eez) +
  geom_sf(data = data_reefs, color = "red")

data_reef_area <- st_intersection(data_eez, data_reefs) %>%  
  group_by(TERRITORY1) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6) %>% 
  rename(territory = TERRITORY1) %>% 
  mutate(reef_area_rel_pacific = (100*reef_area_abs)/sum(reef_area_abs),
         reef_area_rel_world = (100*reef_area_abs)/(as.numeric(st_area(data_reefs))*1e-6))

# 2.5 Group data together ----

data_table_1 <- left_join(data_reef_area, data_maritime_area) %>% 
  left_join(., data_land) %>% 
  left_join(., data_elevation) %>% 
  # Add subterritory
  mutate(subterritory = territory,
       territory = case_when(subterritory %in% c("Line Group", "Phoenix Group", "Gilbert Islands") ~ "Kiribati",
                             subterritory %in% c("Jarvis Island", "Johnston Atoll", 
                                                 "Wake Island", "Howland and Baker Islands",
                                                 "Palmyra Atoll") ~ "Pacific Remote Island Area",
                             TRUE ~ subterritory),
       subterritory = if_else(subterritory == territory, NA, subterritory)) %>% 
  arrange(territory, subterritory) %>% 
  relocate(subterritory, .after = territory)

# 2.6 Add the sum for all territories ----

data_table_1 <- bind_rows(data_table_1, data_table_1 %>% 
              summarise(across(c("reef_area_abs", "reef_area_rel_world", "reef_area_rel_pacific",
                                 "maritime_area", "land_area"), ~sum(.x)))) %>% 
  mutate(territory = if_else(is.na(territory), "Total", territory))

# 2.7 Add the sum for Kiribati and Pacific Remote Islands Area ----

data_table_1 <- bind_rows(data_table_1, data_table_1 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("reef_area_abs", "reef_area_rel_world", "reef_area_rel_pacific",
                                               "maritime_area", "land_area"), ~sum(.x))) %>% 
                            ungroup() %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total")

# 2.8 Reformat the table ----

data_table_1 <- data_table_1 %>% 
  mutate(reef_area_abs = format(round(reef_area_abs, 0), big.mark = ",", scientific = FALSE),
         reef_area_rel_world = format(round(reef_area_rel_world, 3), nsmall = 3),
         reef_area_rel_pacific = format(round(reef_area_rel_pacific, 2), nsmall = 2),
         land_area = format(round(land_area, 0), big.mark = ",", scientific = FALSE),
         maritime_area = format(round(maritime_area, 0), big.mark = ",", scientific = FALSE),
         mean_elevation = round(mean_elevation, 0))

# 2.9 Export the table ----

write_csv2(data_table_1, file = "figs/01_table-1_geo-inf.csv")

# 2.10 Remove useless objects ----

rm(data_reefs, data_reef_area, data_maritime_area, data_land, data_elevation)

# 2.11 Create a function to produce geographic information output (for LaTeX report) ----

export_geoinf <- function(territory_i){
  
  data_i <- data_table_1 %>% 
    filter(territory == territory_i)
  
  writeLines(c("\\begin{tabular}{>{\\bfseries}>{\\color{color1}}rl}",
               paste0("Maritime area & ", data_i[1, "maritime_area"], " km\\textsuperscript{2} \\\\"),
               paste0("Land area & ", data_i[1, "land_area"], " km\\textsuperscript{2} \\\\"),
               paste0("Reef area & ", data_i[1, "reef_area_abs"], " km\\textsuperscript{2} \\\\"),
               paste0("Mean elevation & ", data_i[1, "mean_elevation"], " m \\\\")),
             paste0("figs/03_geo-inf/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}

# 2.12 Map over the function ----

map(unique(data_table_1$territory), ~export_geoinf(territory_i = .))

# 3. Table 2 - Human population ----

# 3.1 Load and transform data --

data_population_eez <- read.csv("data/02_indicators/ind_human-pop_eez.csv") %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4))) %>% 
  rename(territory = TERRITORY1) %>% 
  select(-date) %>% 
  filter(year %in% c(2000, 2020)) %>% 
  pivot_wider(names_from = year, values_from = sum, names_prefix = "pop_eez_") 
  
data_population_5km <- read.csv("data/02_indicators/ind_human-pop_5km.csv") %>% 
  group_by(TERRITORY1, date) %>% 
  summarise(sum = sum(sum)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4))) %>% 
  rename(territory = TERRITORY1) %>% 
  select(-date) %>% 
  filter(year %in% c(2000, 2020)) %>% 
  pivot_wider(names_from = year, values_from = sum, names_prefix = "pop_5km_")

# 3.2 Join and add subterritory --

data_table_2 <- left_join(data_population_5km, data_population_eez) %>% 
  mutate(subterritory = territory,
         territory = case_when(subterritory %in% c("Line Group", "Phoenix Group", "Gilbert Islands") ~ "Kiribati",
                               subterritory %in% c("Jarvis Island", "Johnston Atoll", 
                                                   "Wake Island", "Howland and Baker Islands",
                                                   "Palmyra Atoll") ~ "Pacific Remote Island Area",
                               TRUE ~ subterritory),
         subterritory = if_else(subterritory == territory, NA, subterritory)) %>% 
  arrange(territory, subterritory) %>% 
  relocate(subterritory, .after = territory)

# 3.3 Add total --

data_table_2 <- bind_rows(data_table_2, data_table_2 %>% 
                            summarise(across(c("pop_5km_2000", "pop_5km_2020", "pop_eez_2000", "pop_eez_2020"),
                                             ~sum(.x))) %>% 
                            mutate(territory = "Total", subterritory = NA))

# 3.4 Add total for two territories --

data_table_2 <- bind_rows(data_table_2, data_table_2 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("pop_5km_2000", "pop_5km_2020", "pop_eez_2000", "pop_eez_2020"),
                                             ~sum(.x))) %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total")

# 3.5 Calculate population change and export results ----

data_table_2 %>% 
  mutate(pop_5km_change = ((pop_5km_2020-pop_5km_2000)/pop_5km_2000)*100,
         pop_percent = (pop_5km_2020*100)/pop_eez_2020) %>% 
  mutate(across(c("pop_5km_change", "pop_percent"), ~if_else(is.na(.x), 0, .x))) %>% 
  select(-pop_5km_2000, -pop_eez_2000, -pop_eez_2020) %>% 
  # Reformat the data
  mutate(pop_5km_2020 = format(round(pop_5km_2020, 0), big.mark = ",", scientific = FALSE),
         pop_percent = format(round(pop_percent, 2), nsmall = 2),
         pop_5km_change = format(round(pop_5km_change, 2), nsmall = 2)) %>%
  write_csv2(., file = "figs/01_table-2_human-pop.csv")

rm(data_population_5km, data_population_eez, data_table_2)

# 4. Table 3 - Socio-economy ----

# 4.1 Tourism (extract table from supp. mat. of Spalding et al, 2017) ----

# 4.1.1 Extract first part of the table (page 1) --

spalding_a <- pdf_text("data/spalding-et-al-2017_supp-material.pdf") %>% 
  magrittr::extract(1) %>% 
  str_split(., "\n", simplify = FALSE) %>% 
  magrittr::extract2(1) %>% 
  magrittr::extract(78:87) %>% 
  as_tibble()

# 4.1.2 Extract second part of the table (page 2) --

spalding_b <- pdf_text("data/spalding-et-al-2017_supp-material.pdf") %>% 
  magrittr::extract(2) %>% 
  str_split(., "\n", simplify = FALSE) %>% 
  magrittr::extract2(1) %>% 
  magrittr::extract(48:87) %>% 
  as_tibble()

# 4.1.3 Extract third part of the table (page 3) --

spalding_c <- pdf_text("data/spalding-et-al-2017_supp-material.pdf") %>% 
  magrittr::extract(3) %>% 
  str_split(., "\n", simplify = FALSE) %>% 
  magrittr::extract2(1) %>% 
  magrittr::extract(48:78) %>% 
  as_tibble()

# 4.1.4 Combine tables and data cleaning --

data_tourism <- bind_rows(spalding_a, spalding_b, spalding_c) %>% 
  separate(data = ., col = 1, sep = "\\s{3,}+",
           into = c("territory", "tourists", "reef_tourists",
                    "spending", "reef_spending", "spending_prop", "gdp_prop", "reef_value")) %>% 
  # Correct issue for St. Vincent and the Grenadines
  filter(territory != "St. Vincent and the") %>% 
  mutate(territory = str_replace(territory, "Grenadines", "St. Vincent and the Grenadines")) %>% 
  # Data cleaning
  mutate(across(everything(.), ~na_if(.x, "n/a")),
         across(everything(.), ~str_squish(.)),
         across("tourists":"reef_value", ~str_replace_all(., "\\$|\\%|\\,", "")),
         across("tourists":"reef_value", ~as.numeric(.))) %>% 
  # Filter territories of the Pacific
  filter(territory %in% c("Cook Islands", "Fiji", "French Polynesia", "Guam", 
                          "Micronesia, Fed. Sts.", "New Caledonia", "Northern Mariana Islands",
                          "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga",
                          "United States (Hawaii)", "Vanuatu")) %>% 
  mutate(territory = str_replace_all(territory, c("Micronesia, Fed. Sts." = "Federated States of Micronesia",
                                                  "United States \\(Hawaii\\)" = "Hawaii"))) %>% 
  select(territory, reef_tourists, spending_prop, gdp_prop)

remove(spalding_a, spalding_b, spalding_c)

# 4.2 Coastal protection (extract data from supp; mat. of Spalding et al, 2022 ----

data_protection <- docx_extract_tbl(docx = read_docx("data/spalding-et-al-2022_supp-material.docx"), tbl_number = 2) %>% 
  filter(Country.territory %in% c("Cook Islands", "Fiji", "French Polynesia", "Gilbert Islands",
                                  "Marshall Islands", "Micronesia", "New Caledonia", "Palau",
                                  "Papua New Guinea", "Samoa", "Solomon Islands", "Tuvalu",
                                  "United States (Hawaii)", "Vanuatu", "Wallis and Futuna")) %>% 
  mutate(Country.territory = str_replace_all(Country.territory, "United States \\(Hawaii\\)", "Hawaii")) %>% 
  select("Country.territory", "Thousands.of.people.avoiding.flooding", "Millions.of.dollars.protected.from.flooding") %>% 
  rename(territory = "Country.territory",
         flooding_people = "Thousands.of.people.avoiding.flooding",
         flooding_dollars = "Millions.of.dollars.protected.from.flooding")

# 4.3 Combine data ----

data_table_3 <- data_eez %>% 
  mutate(territory = TERRITORY1) %>% 
  select(territory) %>% 
  st_drop_geometry() %>% 
  left_join(., data_tourism) %>% 
  left_join(., data_protection) %>% 
  # Add subterritory
  mutate(subterritory = territory,
         territory = case_when(subterritory %in% c("Line Group", "Phoenix Group", "Gilbert Islands") ~ "Kiribati",
                               subterritory %in% c("Jarvis Island", "Johnston Atoll", 
                                                   "Wake Island", "Howland and Baker Islands",
                                                   "Palmyra Atoll") ~ "Pacific Remote Island Area",
                               TRUE ~ subterritory),
         subterritory = if_else(subterritory == territory, NA, subterritory)) %>% 
  arrange(territory, subterritory) %>% 
  relocate(subterritory, .after = territory)

# 4.4 Add the sum for Kiribati and Pacific Remote Islands Area ----

data_table_3 <- bind_rows(data_table_3, data_table_3 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("reef_tourists", "spending_prop"), ~sum(., na.rm = FALSE))) %>% 
                            ungroup() %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total") %>% 
  mutate(reef_tourists = format(reef_tourists, big.mark = ",", scientific = FALSE))
