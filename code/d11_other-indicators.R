# 1. Load packages ----

library(tidyverse)
library(sf)
sf_use_s2(FALSE)
library(treemapify)
library(patchwork)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Table 1 - Geographic information ----

## 3.1 Maritime area ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_maritime_area <- data_eez %>% 
  st_drop_geometry() %>% 
  select(TERRITORY1, AREA_KM2) %>% 
  group_by(TERRITORY1) %>% 
  summarise(AREA_KM2 = sum(AREA_KM2)) %>% 
  ungroup() %>% 
  rename(territory = TERRITORY1, maritime_area = AREA_KM2)

## 3.2 Land area ----

data_land <- read.csv("data/02_indicators/ind_land.csv") %>% 
  rename(territory = TERRITORY1, land_area = sum)

## 3.3 Mean land elevation ----

data_elevation <- read.csv("data/02_indicators/ind_elevation.csv") %>% 
  rename(territory = TERRITORY1, mean_elevation = mean)

## 3.4 Reef area ----

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

## 3.5 Group data together ----

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

## 3.6 Add the sum for all territories ----

data_table_1 <- bind_rows(data_table_1, data_table_1 %>% 
              summarise(across(c("reef_area_abs", "reef_area_rel_world", "reef_area_rel_pacific",
                                 "maritime_area", "land_area"), ~sum(.x)))) %>% 
  mutate(territory = if_else(is.na(territory), "Entire Pacific region", territory))

## 3.7 Add the sum for Kiribati and Pacific Remote Islands Area ----

data_table_1 <- bind_rows(data_table_1, data_table_1 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("reef_area_abs", "reef_area_rel_world", "reef_area_rel_pacific",
                                               "maritime_area", "land_area"), ~sum(.x))) %>% 
                            ungroup() %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total")

## 3.8 Reformat the table ----

data_table_1 <- data_table_1 %>% 
  mutate(reef_area_abs = format(round(reef_area_abs, 0), big.mark = ",", scientific = FALSE),
         reef_area_rel_world = format(round(reef_area_rel_world, 3), nsmall = 3),
         reef_area_rel_pacific = format(round(reef_area_rel_pacific, 2), nsmall = 2),
         land_area = format(round(land_area, 0), big.mark = ",", scientific = FALSE),
         maritime_area = format(round(maritime_area, 0), big.mark = ",", scientific = FALSE),
         mean_elevation = round(mean_elevation, 0))

## 3.9 Export the table ----

### 3.9.1 In .xlsx format ---- 

data_table_1b <- data_table_1 %>% 
  filter(territory != "Entire Pacific region") %>% 
  bind_rows(., data_table_1 %>% 
              filter(territory == "Entire Pacific region"))

openxlsx::write.xlsx(data_table_1b, file = "figs/01_part-1/table-1.xlsx")

### 3.9.2 In .tex format ----

latex_table_line <- function(i, subterritory){
  
  color <- ifelse(i %% 2 == 0, "white", "secondcolor")
  
  if(subterritory == FALSE){
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{2}{|l|}{", data_table_1b[i, "territory"], "} &", data_table_1b[i, "reef_area_abs"], "&",
                     data_table_1b[i, "reef_area_rel_pacific"], "&", data_table_1b[i, "reef_area_rel_world"]," \\\\ \\hline"))
    
  }else{
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{1}{|l}{} & ", data_table_1b[i, "subterritory"], " &", data_table_1b[i, "reef_area_abs"], "&",
                     data_table_1b[i, "reef_area_rel_pacific"], "&", data_table_1b[i, "reef_area_rel_world"]," \\\\ \\hline"))
    
  }
  
  return(line)
  
}

writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|R{2.9cm}|R{2.9cm}|R{2.9cm}|}",
             "\\hline",
             "\\rowcolor{firstcolor}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{Countries and territories}} & \\textcolor{white}{Absolute extent (km\\textsuperscript{2})
} & \\textcolor{white}{Extent rel. to the Pacific (\\%)}  & \\textcolor{white}{Extent rel. to the world (\\%)} \\\\ \\hline",
             map(1:8, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(9:11, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(12:17, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(18:22, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(23:32, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             paste0("\\rowcolor{secondcolor}"),
             paste0("\\multicolumn{2}{|l|}{\\textbf{", data_table_1b[33, "territory"], "}} &", data_table_1b[33, "reef_area_abs"], "&",
                    data_table_1b[33, "reef_area_rel_pacific"], "&", data_table_1b[33, "reef_area_rel_world"]," \\\\ \\hline"),
             "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/01_part-1/table-1.tex"))

## 3.10 Remove useless objects ----

rm(data_reefs, data_reef_area, data_maritime_area, data_land, data_elevation)

## 3.11 Create a function to produce geographic information output (for LaTeX report) ----

map_tex <- function(territory_i){
  
  if(territory_i == "Kiribati"){
    
    data_i <- data_table_1 %>% 
      filter(territory == "Kiribati" & subterritory != "All") %>% 
      select(-territory, -reef_area_rel_pacific, -reef_area_rel_world)
    
    writeLines(c("\\begin{center}",
                 "\\begin{tabular}{|>{\\raggedleft\\arraybackslash}m{3.75cm}|C{3.5cm}|C{3.5cm}|C{3.5cm}|}",
                 "\\hline",
                 paste0(" & ", 
                        data_i[1, "subterritory"], " & ", 
                        data_i[2, "subterritory"], " & ", 
                        data_i[3, "subterritory"], "\\\\ \\hline"),
                 "\\rowcolor{secondcolor}",
                 paste0("Maritime area & ", 
                        data_i[1, "maritime_area"], " km\\textsuperscript{2} & ",
                        data_i[2, "maritime_area"], " km\\textsuperscript{2} & ",
                        data_i[3, "maritime_area"], " km\\textsuperscript{2} \\\\ \\hline"),
                 "\\rowcolor{white}",
                 paste0("Land area & ", 
                        data_i[1, "land_area"], " km\\textsuperscript{2} & ",
                        data_i[2, "land_area"], " km\\textsuperscript{2} & ",
                        data_i[3, "land_area"], " km\\textsuperscript{2} \\\\ \\hline"),
                 "\\rowcolor{secondcolor}",
                 paste0("Reef area & ", 
                        data_i[1, "reef_area_abs"], " km\\textsuperscript{2} & ",
                        data_i[2, "reef_area_abs"], " km\\textsuperscript{2} & ",
                        data_i[3, "reef_area_abs"], " km\\textsuperscript{2} \\\\ \\hline"),
                 "\\rowcolor{white}",
                 paste0("Mean elevation & ", 
                        data_i[1, "mean_elevation"], " m & ",
                        data_i[2, "mean_elevation"], " m & ",
                        data_i[3, "mean_elevation"], " m \\\\ \\hline"),
                 "\\end{tabular}",
                 "\\end{center}"),
               paste0("figs/02_part-2/tbl-1/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
    
  }else if (territory_i == "Pacific Remote Island Area"){
      
    data_i <- data_table_1 %>% 
      filter(territory == "Pacific Remote Island Area" & subterritory != "All") %>% 
      mutate(subterritory = str_replace_all(subterritory, "Howland and Baker Islands", "How. and Baker Isl."),
             land_area = str_replace_all(land_area, "0", ">1")) %>% 
      select(-territory, -reef_area_rel_pacific, -reef_area_rel_world)
    
    writeLines(c("\\begin{center}",
                 "\\begin{tabular}{|>{\\raggedleft\\arraybackslash}m{3.75cm}|C{1.9cm}|C{1.9cm}|C{1.9cm}|C{1.9cm}|C{1.9cm}|}",
                 "\\hline",
                 paste0(" & ", 
                        data_i[1, "subterritory"], " & ", 
                        data_i[2, "subterritory"], " & ", 
                        data_i[3, "subterritory"], " & ", 
                        data_i[4, "subterritory"], " & ", 
                        data_i[5, "subterritory"], "\\\\ \\hline"),
                 "\\rowcolor{secondcolor}",
                 paste0("Maritime area & ", 
                        data_i[1, "maritime_area"], " km\\textsuperscript{2} & ",
                        data_i[2, "maritime_area"], " km\\textsuperscript{2} & ",
                        data_i[3, "maritime_area"], " km\\textsuperscript{2} & ",
                        data_i[4, "maritime_area"], " km\\textsuperscript{2} & ",
                        data_i[5, "maritime_area"], " km\\textsuperscript{2} \\\\ \\hline"),
                 "\\rowcolor{white}",
                 paste0("Land area & ", 
                        data_i[1, "land_area"], " km\\textsuperscript{2} & ",
                        data_i[2, "land_area"], " km\\textsuperscript{2} & ",
                        data_i[3, "land_area"], " km\\textsuperscript{2} & ",
                        data_i[4, "land_area"], " km\\textsuperscript{2} & ",
                        data_i[5, "land_area"], " km\\textsuperscript{2} \\\\ \\hline"),
                 "\\rowcolor{secondcolor}",
                 paste0("Reef area & ", 
                        data_i[1, "reef_area_abs"], " km\\textsuperscript{2} & ",
                        data_i[2, "reef_area_abs"], " km\\textsuperscript{2} & ",
                        data_i[3, "reef_area_abs"], " km\\textsuperscript{2} & ",
                        data_i[4, "reef_area_abs"], " km\\textsuperscript{2} & ",
                        data_i[5, "reef_area_abs"], " km\\textsuperscript{2} \\\\ \\hline"),
                 "\\rowcolor{white}",
                 paste0("Mean elevation & ", 
                        data_i[1, "mean_elevation"], " m & ",
                        data_i[2, "mean_elevation"], " m & ",
                        data_i[3, "mean_elevation"], " m & ",
                        data_i[4, "mean_elevation"], " m & ",
                        data_i[5, "mean_elevation"], " m \\\\ \\hline"),
                 "\\end{tabular}",
                 "\\end{center}"),
               paste0("figs/02_part-2/tbl-1/pria.tex"))
    
  }else{
    
    data_i <- data_table_1 %>% 
      filter(territory == territory_i)
    
    writeLines(c("\\begin{center}",
               "\\begin{tabular}{|>{\\raggedleft\\arraybackslash}m{3.75cm}|m{2.75cm}|}",
               "\\hline",
               "\\rowcolor{secondcolor}",
               paste0("Maritime area & ", data_i[1, "maritime_area"], " km\\textsuperscript{2} \\\\ \\hline"),
               "\\rowcolor{white}",
               paste0("Land area & ", data_i[1, "land_area"], " km\\textsuperscript{2} \\\\ \\hline"),
               "\\rowcolor{secondcolor}",
               paste0("Reef area & ", data_i[1, "reef_area_abs"], " km\\textsuperscript{2} \\\\ \\hline"),
               "\\rowcolor{white}",
               paste0("Mean elevation & ", data_i[1, "mean_elevation"], " m \\\\ \\hline"),
               "\\end{tabular}",
               "\\end{center}"),
             paste0("figs/02_part-2/tbl-1/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
    
    }
  
}

## 3.12 Map over the function ----

map(setdiff(unique(data_table_1$territory), c("Entire Pacific region")),
    ~map_tex(territory_i = .))

# 4. Table 2 - Human population ----

## 4.1 Load and transform data ----

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

## 4.2 Join and add subterritory ----

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

## 4.3 Add total ----

data_table_2 <- bind_rows(data_table_2, data_table_2 %>% 
                            summarise(across(c("pop_5km_2000", "pop_5km_2020", "pop_eez_2000", "pop_eez_2020"),
                                             ~sum(.x))) %>% 
                            mutate(territory = "Entire Pacific region", subterritory = NA))

## 4.4 Add total for two territories --

data_table_2 <- bind_rows(data_table_2, data_table_2 %>% 
                            filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
                            group_by(territory) %>% 
                            summarise(across(c("pop_5km_2000", "pop_5km_2020", "pop_eez_2000", "pop_eez_2020"),
                                             ~sum(.x))) %>% 
                            mutate(subterritory = "All")) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Total")

## 4.5 Calculate population change ----

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

## 4.6 Export the table ----

### 4.6.1 In .xlsx format ----

openxlsx::write.xlsx(data_table_2, file = "figs/01_part-1/table-2.xlsx")

### 4.6.2 In .tex format ----

latex_table_line <- function(i, subterritory){
  
  color <- ifelse(i %% 2 == 0, "white", "secondcolor")
  
  if(subterritory == FALSE){
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{2}{|l|}{", data_table_2[i, "territory"], "} &", data_table_2[i, "pop_5km_2020"], "&",
                     data_table_2[i, "pop_5km_change"], "&", data_table_2[i, "pop_percent"]," \\\\ \\hline"))
    
  }else{
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{1}{|l}{} & ", data_table_2[i, "subterritory"], " &", data_table_2[i, "pop_5km_2020"], "&",
                     data_table_2[i, "pop_5km_change"], "&", data_table_2[i, "pop_percent"]," \\\\ \\hline"))
    
  }
  
  return(line)
  
}

writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|R{2.85cm}|R{2.85cm}|R{2.85cm}|}",
             "\\hline",
             "\\rowcolor{firstcolor}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{Countries and territories}} & \\textcolor{white}{Human pop. in 2020} & \\textcolor{white}{Change in human pop. (\\%)}  & \\textcolor{white}{Percentage of total human pop. (\\%)} \\\\ \\hline",
             map(1:8, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(9:11, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(12:17, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(18:22, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(23:32, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             paste0("\\rowcolor{secondcolor}"),
             paste0("\\multicolumn{2}{|l|}{\\textbf{", data_table_2[33, "territory"], "}} &", data_table_2[33, "pop_5km_2020"], "&",
                    data_table_2[33, "pop_5km_change"], "&", data_table_2[33, "pop_percent"]," \\\\ \\hline"),
            "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/01_part-1/table-2.tex"))

# 5. Relative coral reef extent of the Pacific ----

## 5.1 Load and transform data ----

data_reefs <- st_read("data/03_reefs-area_wri/reef_500_poly.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

## 5.2 Reef area by Pacific EEZ ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline(options = c("WRAPDATELINE=YES")) %>% 
  st_make_valid()

data_reef_area_pacific <- st_intersection(data_eez, data_reefs) %>%  
  group_by(TERRITORY1) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6) %>% 
  rename(territory = TERRITORY1) %>% 
  arrange(-reef_area_abs)

plot_b <- ggplot(data = data_reef_area_pacific, aes(area = reef_area_abs, fill = reef_area_abs, label = territory)) +
  geom_treemap(show.legend = FALSE, color = "white", size = 2) +
  geom_treemap_text(color = "white", place = "centre", reflow = TRUE, family = font_choose_graph) +
  scale_fill_gradientn(colours = palette_first[2:5]) +
  theme_graph()

ggsave(filename = "figs/01_part-1/fig-2b.png", plot = plot_b, height = 5, width = 5, dpi = fig_resolution)

## 5.3 Reef area by GCRMN regions ----

load("data/01_background-shp/gcrmn_regions.RData")

data_reef_area_gcrmn <- st_intersection(data_gcrmn_regions, data_reefs) %>%  
  group_by(gcrmn_region) %>% 
  summarise(reef_area_abs = sum(st_area(geometry))) %>% 
  ungroup() %>% 
  st_drop_geometry() %>% 
  mutate(reef_area_abs = as.numeric(reef_area_abs)*1e-6,
         gcrmn_region = str_replace_all(gcrmn_region, "EAS", "East Asian Seas"),
         color = case_when(gcrmn_region == "Pacific" ~ palette_first[3],
                           TRUE ~ palette_first[4]),
         color_text = case_when(gcrmn_region == "Pacific" ~ "white",
                                TRUE ~ "white"))

plot_a <- ggplot(data = data_reef_area_gcrmn, aes(area = reef_area_abs, fill = color, label = gcrmn_region)) +
  geom_treemap(show.legend = FALSE, color = "white", size = 2, start = "bottomright") +
  geom_treemap_text(aes(color = color_text), place = "centre", reflow = TRUE,
                    family = font_choose_graph, start = "bottomright") +
  scale_fill_identity() +
  scale_color_identity() +
  theme_graph()

ggsave(filename = "figs/01_part-1/fig-2a.png", plot = plot_a, height = 5, width = 5, dpi = fig_resolution)

## 5.4 Combine the two figures ----

(plot_a + labs(title = "A") + theme(plot.margin = unit(c(0.25, 0.75, 0.25, 0), "cm"),
                                    plot.title = element_text(size = 18))) +
  (plot_b + labs(title = "B") + theme(plot.margin = unit(c(0.25, 0, 0.25, 0.75), "cm"),
                                      plot.title = element_text(size = 18)))

ggsave(filename = "figs/01_part-1/fig-2.png", height = 5, width = 10, dpi = fig_resolution)

# 6. Pacific population ----

## 6.1 Load data ----

data_population <- read.csv("data/02_indicators/ind_human-pop_5km.csv") %>% 
  group_by(TERRITORY1, date) %>% 
  summarise(population = sum(sum)) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(str_sub(date, 1, 4))) %>% 
  rename(territory = TERRITORY1) %>% 
  select(-date) %>% 
  mutate(population = population*1e-06, # Convert to million
         territory_type = if_else(territory %in% c("Papua New Guinea", "Fiji", "Hawaii"),
                                  territory, 
                                  "Other territories")) %>% 
  group_by(territory_type, year) %>% 
  summarise(population = sum(population, na.rm = TRUE)) %>% 
  ungroup()

## 6.2 Make the plot ----

ggplot(data = data_population, aes(x = year, y = population, fill = territory_type)) +
  geom_area(show.legend = FALSE) +
  scale_fill_manual(values = rev(palette_first)) +
  labs(x = "Year", y = "Inhabitants (millions)") +
  annotate(geom = "text", label = "Papua New Guinea", x = 2003, y = 0.45, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Other territories", x = 2003, y = 1.75, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Hawaii", x = 2003, y = 3.1, 
           family = font_choose_graph, color = "white", hjust = 0) +
  annotate(geom = "text", label = "Fiji", x = 2003, y = 3.8, 
           family = font_choose_graph, color = "white", hjust = 0) +
  theme_graph() + 
  lims(y = c(0, 6)) +
  scale_x_continuous(expand = expansion(mult = c(0.025, 0.025)), limits = c(2000, 2020))

## 6.3 Export the plot ----

ggsave("figs/01_part-1/fig-3.png", height = 4, width = 5, dpi = fig_resolution)
