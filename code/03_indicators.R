# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# 2. Table 1 - Geographic information ----

## 2.1 Maritime area ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_maritime_area <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2) %>% 
  group_by(TERRITORY1) %>% 
  summarise(AREA_KM2 = sum(AREA_KM2)) %>% 
  ungroup() %>% 
  rename(territory = TERRITORY1, maritime_area = AREA_KM2)

## 2.2 Land area ----

data_land <- read.csv("data/02_indicators/ind_land.csv") %>% 
  rename(territory = TERRITORY1, land_area = sum)

## 2.3 Mean land elevation ----

data_elevation <- read.csv("data/02_indicators/ind_elevation.csv") %>% 
  rename(territory = TERRITORY1, mean_elevation = mean)

## 2.4 Reef area ----

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

## 2.5 Group data together ----

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

## 2.6 Add the sum for all territories ----

data_table_1 <- bind_rows(data_table_1, data_table_1 %>% 
              summarise(across(c("reef_area_abs", "reef_area_rel_world", "reef_area_rel_pacific",
                                 "maritime_area", "land_area"), ~sum(.x)))) %>% 
  mutate(territory = if_else(is.na(territory), "Entire Pacific region", territory))

## 2.7 Add the sum for Kiribati and Pacific Remote Islands Area ----

data_table_1 <- bind_rows(data_table_1, data_table_1 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("reef_area_abs", "reef_area_rel_world", "reef_area_rel_pacific",
                                               "maritime_area", "land_area"), ~sum(.x))) %>% 
                            ungroup() %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total")

## 2.8 Reformat the table ----

data_table_1 <- data_table_1 %>% 
  mutate(reef_area_abs = format(round(reef_area_abs, 0), big.mark = ",", scientific = FALSE),
         reef_area_rel_world = format(round(reef_area_rel_world, 3), nsmall = 3),
         reef_area_rel_pacific = format(round(reef_area_rel_pacific, 2), nsmall = 2),
         land_area = format(round(land_area, 0), big.mark = ",", scientific = FALSE),
         maritime_area = format(round(maritime_area, 0), big.mark = ",", scientific = FALSE),
         mean_elevation = round(mean_elevation, 0))

## 2.9 Export the table ----

data_table_1b <- data_table_1 %>% 
  filter(territory != "Entire Pacific region") %>% 
  bind_rows(., data_table_1 %>% 
              filter(territory == "Entire Pacific region")) %>% 
  openxlsx::write.xlsx(., file = "figs/01_part-1/table-1.xlsx")

## 2.10 Remove useless objects ----

rm(data_reefs, data_reef_area, data_maritime_area, data_land, data_elevation)

## 2.11 Create a function to produce geographic information output (for LaTeX report) ----

map_tex <- function(territory_i){
  
  data_i <- data_table_1 %>% 
    filter(territory == territory_i)
  
  writeLines(c("\\begin{center}",
               "\\begin{tabular}{|>{\\raggedleft\\arraybackslash}m{3.75cm}|m{2.75cm}|}",
               "\\hline",
               "\\rowcolor{colortable1}",
               paste0("Maritime area & ", data_i[1, "maritime_area"], " km\\textsuperscript{2} \\\\ \\hline"),
               "\\rowcolor{colortable2}",
               paste0("Land area & ", data_i[1, "land_area"], " km\\textsuperscript{2} \\\\ \\hline"),
               "\\rowcolor{colortable1}",
               paste0("Reef area & ", data_i[1, "reef_area_abs"], " km\\textsuperscript{2} \\\\ \\hline"),
               "\\rowcolor{colortable2}",
               paste0("Mean elevation & ", data_i[1, "mean_elevation"], " m \\\\ \\hline"),
               "\\end{tabular}",
               "\\end{center}"),
             paste0("figs/02_part-2/tbl-1/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}

## 2.12 Map over the function ----

map(setdiff(unique(data_table_1$territory), "Entire Pacific region"),
    ~map_tex(territory_i = .))

# 3. Table 2 - Human population ----

## 3.1 Load and transform data --

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

## 3.2 Join and add subterritory --

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
                            mutate(territory = "Entire Pacific region", subterritory = NA))

## 3.4 Add total for two territories --

data_table_2 <- bind_rows(data_table_2, data_table_2 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("pop_5km_2000", "pop_5km_2020", "pop_eez_2000", "pop_eez_2020"),
                                             ~sum(.x))) %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total")

## 3.5 Calculate population change ----

data_table_2 <- data_table_2 %>% 
  mutate(pop_5km_change = ((pop_5km_2020-pop_5km_2000)/pop_5km_2000)*100,
         pop_percent = (pop_5km_2020*100)/pop_eez_2020) %>% 
  mutate(across(c("pop_5km_change", "pop_percent"), ~if_else(is.na(.x), 0, .x))) %>% 
  select(-pop_5km_2000, -pop_eez_2000, -pop_eez_2020) %>% 
  # Reformat the data
  mutate(pop_5km_2020 = format(round(pop_5km_2020, 0), big.mark = ",", scientific = FALSE),
         pop_percent = format(round(pop_percent, 2), nsmall = 2),
         pop_5km_change = format(round(pop_5km_change, 2), nsmall = 2))

data_table_2 <- data_table_2 %>% 
  filter(territory != "Entire Pacific region") %>% 
  bind_rows(., data_table_2 %>% 
              filter(territory == "Entire Pacific region"))

## 3.6 Export the table ----

### 3.6.1 In .xlsx format ----

openxlsx::write.xlsx(data_table_2, file = "figs/01_part-1/table-2.xlsx")

### 3.6.2 In .tex format ----

writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|r|l|l|}",
             "\\hline",
             "\\rowcolor{colortable3}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{Country/Territory}} & \\textcolor{white}{Pop. 2020} & \\textcolor{white}{Rel. change}  & \\textcolor{white}{Percent.} \\\\ \\hline",
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[1, "territory"], "} &", data_table_2[1, "pop_5km_2020"], "&",
                    data_table_2[1, "pop_5km_change"], "&", data_table_2[1, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[2, "territory"], "} &", data_table_2[2, "pop_5km_2020"], "&",
                    data_table_2[2, "pop_5km_change"], "&", data_table_2[2, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[3, "territory"], "} &", data_table_2[3, "pop_5km_2020"], "&",
                    data_table_2[3, "pop_5km_change"], "&", data_table_2[3, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[4, "territory"], "} &", data_table_2[4, "pop_5km_2020"], "&",
                    data_table_2[4, "pop_5km_change"], "&", data_table_2[4, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[5, "territory"], "} &", data_table_2[5, "pop_5km_2020"], "&",
                    data_table_2[5, "pop_5km_change"], "&", data_table_2[5, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[6, "territory"], "} &", data_table_2[6, "pop_5km_2020"], "&",
                    data_table_2[6, "pop_5km_change"], "&", data_table_2[6, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[7, "territory"], "} &", data_table_2[7, "pop_5km_2020"], "&",
                    data_table_2[7, "pop_5km_change"], "&", data_table_2[7, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[8, "territory"], "} &", data_table_2[8, "pop_5km_2020"], "&",
                    data_table_2[8, "pop_5km_change"], "&", data_table_2[8, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{1}{|l}{} & ", data_table_2[9, "subterritory"], " &", data_table_2[9, "pop_5km_2020"], "&",
                    data_table_2[9, "pop_5km_change"], "&", data_table_2[9, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{1}{|l}{} & ", data_table_2[10, "subterritory"], " &", data_table_2[10, "pop_5km_2020"], "&",
                    data_table_2[10, "pop_5km_change"], "&", data_table_2[10, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{1}{|l}{} & ", data_table_2[11, "subterritory"], " &", data_table_2[11, "pop_5km_2020"], "&",
                    data_table_2[11, "pop_5km_change"], "&", data_table_2[11, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[12, "territory"], "} &", data_table_2[12, "pop_5km_2020"], "&",
                    data_table_2[12, "pop_5km_change"], "&", data_table_2[12, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[13, "territory"], "} &", data_table_2[13, "pop_5km_2020"], "&",
                    data_table_2[13, "pop_5km_change"], "&", data_table_2[13, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable2}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[14, "territory"], "} &", data_table_2[14, "pop_5km_2020"], "&",
                    data_table_2[14, "pop_5km_change"], "&", data_table_2[14, "pop_percent"]," \\\\ \\hline"),
             "\\rowcolor{colortable1}",
             paste0("\\multicolumn{2}{|l|}{", data_table_2[15, "territory"], "} &", data_table_2[15, "pop_5km_2020"], "&",
                    data_table_2[15, "pop_5km_change"], "&", data_table_2[15, "pop_percent"]," \\\\ \\hline"),
             "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/01_part-1/table-2.tex"))

## 3.7 Remove useless objects ----

rm(data_population_5km, data_population_eez, data_table_2)
