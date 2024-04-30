# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(ggnewscale)
library(sf)
sf_use_s2(FALSE)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/data_descriptors.R")

# 3. Map of spatio-temporal distribution of monitoring sites ----

## 3.1 Change CRS ----

### 3.1.1 Define CRS ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

### 3.1.2 Define the offset ----

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

### 3.3 Define a long and slim polygon that overlaps the meridian line ----

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

## 3.2 Load data ----

### 3.2.1 EEZ ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = crs_selected)

data_eez_supp_a <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(TERRITORY1 == "Australia") %>% 
  st_transform(crs = 4326) %>% 
  nngeo::st_remove_holes(.) %>% 
  st_cast(., "POLYGON") %>% 
  mutate(row = 1:8) %>% 
  filter(row == 1)

data_eez_supp <- read_sf("data/01_background-shp/03_eez/World_EEZ_v12_20231025/eez_v12.shp") %>% 
  filter(TERRITORY1 %in% c("Indonesia", "Japan", "Philippines") | 
           GEONAME == "Overlapping claim Matthew and Hunter Islands: France / Vanuatu") %>% 
  st_transform(crs = 4326) %>% 
  nngeo::st_remove_holes(.) %>% 
  bind_rows(., data_eez_supp_a) %>% 
  st_transform(crs = crs_selected) 

### 3.2.2 Land ----

load("data/01_background-shp/02_princeton/data_land.RData")

data_land <- data_land %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

list_shp <- list.files(path = "data/01_background-shp/04_princeton_additional",
                       pattern = ".shp$", full.names = TRUE, recursive = TRUE)

data_land_supp <- map_dfr(list_shp, ~st_read(.)) %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

rm(list_shp, data_eez_supp_a)

### 3.2.3 Bathymetry ----

load("data/01_background-shp/01_ne/ne_10m_bathymetry_all.RData")

data_bathy <- data_bathy %>% 
  st_transform(crs = 4326) %>% 
  st_difference(correction_polygon) %>% 
  st_transform(crs_selected)

### 3.2.4 Benthic data ----

load("data/09_misc/data-benthic.RData")

data_benthic_sites <- data_benthic %>% 
  select(decimalLatitude, decimalLongitude, year, territory) %>% 
  distinct() %>% 
  group_by(decimalLatitude, decimalLongitude, territory) %>% 
  summarise(interval_years = max(year, na.rm = TRUE) - min(year, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(interval_class = cut(interval_years, 
                              breaks = c(-Inf, 1, 5, 10, 15, Inf),
                              labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years")),
         interval_class = as.factor(interval_class)) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) %>% 
  st_transform(crs = crs_selected)

## 3.3 Create the function ----

map_eez <- function(territory_i){
  
  # 1. Filter ----
  
  data_eez_i <- data_eez %>% 
    filter(TERRITORY1 == territory_i)
  
  data_benthic_sites_i <- data_benthic_sites %>% 
    filter(territory == territory_i) %>%
    arrange(interval_class)
  
  # 2. Create the bbox ----
  
  x_min <- st_bbox(data_eez_i)["xmin"]
  x_max <- st_bbox(data_eez_i)["xmax"]
  y_min <- st_bbox(data_eez_i)["ymin"]
  y_max <- st_bbox(data_eez_i)["ymax"]
  
  percent_margin_ltr <- 10 # Margin in percentage for left, top, and right of plot
  
  data_bbox <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                              x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                      lat = c(y_min - ((y_max - y_min)*percent_margin_ltr/100),
                              y_max + ((y_max - y_min)*percent_margin_ltr/100))) %>% 
    st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
    st_bbox() %>% 
    st_as_sfc()
  
  # 5. Layer to mask external zone of eez_i ----
  
  data_alpha <- st_difference(data_bbox, data_eez_i)
  
  # 6. Make the plot ----
  
  plot_i <- ggplot() +
    geom_sf(data = data_bathy %>% filter(depth == 0), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 200), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 1000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 2000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 3000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 4000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 5000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 6000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 7000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 8000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 9000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    geom_sf(data = data_bathy %>% filter(depth == 10000), aes(fill = fill_color), color = NA, alpha = 0.2) +
    scale_fill_identity() +
    geom_sf(data = data_eez, color = "black", fill = NA) +
    geom_sf(data = data_eez_supp, color = "black", fill = NA) +
    geom_sf(data = data_land, fill = "grey", col = "darkgrey") +
    geom_sf(data = data_land_supp, fill = "grey", col = "darkgrey") +
    geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
    new_scale_fill() +
    geom_sf(data = data_benthic_sites_i, aes(fill = interval_class), color = "black", shape = 21, size = 2.5,
            show.legend = c(shape = TRUE))  +
    scale_fill_manual(values = palette_second,
                      labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                      drop = FALSE, name = "Number of years with data") +
    coord_sf(xlim = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                      x_max + ((x_max - x_min)*percent_margin_ltr/100)),
             ylim = c(y_min - ((y_max - y_min)*percent_margin_ltr/100),
                      y_max + ((y_max - y_min)*percent_margin_ltr/100)),
             expand = FALSE) +
    theme_minimal() +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "null"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal") +
    guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4)))
  
  # 7. Export the plot ----
  
  ggsave(filename = paste0("figs/02_part-2/fig-7/",
                           str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         plot = plot_i, dpi = 600)
  
}

## 3.4 Map over the function (except PRIA) ----

map(setdiff(unique(data_eez$TERRITORY1),
            c("Palmyra Atoll", "Johnston Atoll",
              "Wake Island", "Jarvis Island",
              "Howland and Baker Islands")), # PRIA territories
    ~map_eez(territory_i = .))

## 3.5 Map for Pacific Remote Islands Area (PRIA) ----

### 3.5.1 Filter ----

data_eez_i <- data_eez %>% 
  filter(TERRITORY1 %in% c("Palmyra Atoll", "Johnston Atoll",
                           "Wake Island", "Jarvis Island",
                           "Howland and Baker Islands"))

data_benthic_sites_i <- data_benthic_sites %>% 
  filter(territory %in% c("Palmyra Atoll", "Johnston Atoll",
                          "Wake Island", "Jarvis Island",
                          "Howland and Baker Islands")) %>% 
  arrange(interval_class)

### 3.5.2 Create the bbox ----

x_min <- st_bbox(data_eez_i)["xmin"]
x_max <- st_bbox(data_eez_i)["xmax"]
y_min <- st_bbox(data_eez_i)["ymin"]
y_max <- st_bbox(data_eez_i)["ymax"]

percent_margin_ltr <- 10 # Margin in percentage for left, top, and right of plot

### 3.5.3 White polygon to put scale on ----

poly_scale <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                             x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                     lat = c(y_min - ((y_max - y_min)*percent_margin_ltr/100),
                             y_min - ((y_max - y_min)*(percent_margin_ltr/1.5)/100))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 3.5.4 Define plot limits with additional margins ----

data_bbox <- tibble(lon = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                            x_max + ((x_max - x_min)*percent_margin_ltr/100)),
                    lat = c(y_min - ((y_max - y_min)*percent_margin_ltr/100),
                            y_max + ((y_max - y_min)*percent_margin_ltr/100))) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = crs_selected) %>% 
  st_bbox() %>% 
  st_as_sfc()

### 3.5.5 Layer to mask external zone of eez_i ----

data_alpha <- data_eez_i %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_difference(data_bbox, .)

### 3.5.6 Make the plot ----

plot_i <- ggplot() +
  geom_sf(data = data_bathy %>% filter(depth == 0), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 200), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 1000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 2000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 3000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 4000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 5000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 6000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 7000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 8000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 9000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  geom_sf(data = data_bathy %>% filter(depth == 10000), aes(fill = fill_color), color = NA, alpha = 0.2) +
  scale_fill_identity() +
  geom_sf(data = data_eez, color = "black", fill = NA) +
  geom_sf(data = data_land, fill = "grey", col = "darkgrey") +
  geom_sf(data = data_bbox, fill = "white", alpha = 0.5) +
  geom_sf(data = data_eez_i, color = "black", fill = NA) +
  geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
  new_scale_fill() +
  geom_sf(data = data_benthic_sites_i, aes(fill = interval_class), color = "black", shape = 21, size = 2.5)  +
  scale_fill_manual(values = palette_second,
                    labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                    drop = FALSE, name = "Number of years with data") +
  coord_sf(xlim = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                    x_max + ((x_max - x_min)*percent_margin_ltr/100)),
           ylim = c(y_min - ((y_max - y_min)*percent_margin_ltr/100),
                    y_max + ((y_max - y_min)*percent_margin_ltr/100)),
           expand = FALSE) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.position = "top",
        legend.direction = "horizontal") +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, override.aes = list(size = 4)))

### 3.5.7 Export the plot ----

ggsave(filename = paste0("figs/02_part-2/fig-7/pria.png"), plot = plot_i, dpi = 600)

# 5. Plots of number of sites per interval class ----

## 5.1 Make the plot ----

data_benthic_sites %>% 
  st_drop_geometry() %>% 
  group_by(interval_class, territory) %>% 
  count() %>% 
  ungroup() %>% 
  complete(interval_class, territory, fill = list(n = 0)) %>% 
  group_by(territory) %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ungroup() %>% 
  filter(territory %in% unique(data_benthic_sites$territory)) %>%  
  ggplot(data = ., aes(x = reorder(interval_class, desc(interval_class)),
                       y = percent, fill = interval_class)) +
  geom_bar(stat = "identity", color = NA, show.legend = FALSE, width = 0.65) +
  scale_fill_manual(values = palette_second,
                    labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                    drop = FALSE, name = "Number of years with data") +
  labs(x = NULL, y = "Sites (%)") +
  coord_flip(clip = "off") +
  theme_graph() +
  facet_wrap(~territory, scales = "free", ncol = 5) +
  theme(strip.text = element_text(hjust = 0.5),
        strip.background = element_blank())

## 5.2 Save the plot ----

ggsave(filename = "figs/04_supp/01_data-explo/04_surveys_duration.png",
       width = 15, height = 12, dpi = 600)

# 6. Plots of number of surveys per year ----

## 6.1 Transform the data ----

load("data/09_misc/data-benthic.RData")

data_surveys <- data_benthic %>% 
  select(territory, decimalLatitude, decimalLongitude, eventDate, year) %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(year, territory) %>% 
  count() %>% 
  ungroup() %>% 
  complete(year, fill = list(n = 0)) %>% 
  group_by(territory) %>% 
  mutate(percent = n*100/sum(n)) %>% 
  ungroup()

## 6.2 Make the plot ----

data_surveys %>% 
  filter(territory %in% unique(data_surveys$territory)) %>%  
  ggplot(data = ., aes(x = year, y = percent)) +
  geom_bar(stat = "identity", show.legend = FALSE, width = 0.8, fill = palette_second[5]) +
  labs(x = "Year", y = "Surveys (%)") +
  theme_graph() +
  coord_cartesian(clip = "off") +
  facet_wrap(~territory, scales = "free", ncol = 5) +
  theme_graph() +
  theme(strip.text = element_text(hjust = 0.5),
        strip.background = element_blank()) +
  scale_x_continuous(limits = c(1985, 2025))

## 6.3 Save the plot ----

ggsave(filename = "figs/04_supp/01_data-explo/04_surveys_year.png",
       width = 15, height = 12, dpi = 600)

# 7. Extract descriptors ----

load("data/09_misc/data-benthic.RData")

## 7.1 Add subterritories ----

monitoring_descriptors <- data_benthic %>% 
  group_by(territory) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  # Add missing territories (those with no data)
  left_join(st_read("data/01_background-shp/03_eez/data_eez.shp") %>%
              select(TERRITORY1) %>% 
              st_drop_geometry() %>% 
              rename(territory = TERRITORY1),
            .) %>% 
  mutate(across(c("nb_sites", "nb_surveys", "nb_datasets"), .fns = ~replace_na(.,0))) %>% 
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

## 7.2 Add total ----

monitoring_descriptors <- data_benthic %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(territory = "Entire Pacific region") %>% 
  bind_rows(monitoring_descriptors, .)

## 7.3 Add total for two territories ----

monitoring_descriptors <- data_benthic %>% 
  mutate(territory = case_when(territory %in% c("Line Group", "Phoenix Group", 
                                                "Gilbert Islands") ~ "Kiribati",
                               territory %in% c("Jarvis Island", "Johnston Atoll", 
                                                "Wake Island", "Howland and Baker Islands",
                                                "Palmyra Atoll") ~ "Pacific Remote Island Area",
                               TRUE ~ territory)) %>% 
  filter(territory %in% c("Kiribati", "Pacific Remote Island Area")) %>% 
  group_by(territory) %>% 
  data_descriptors() %>% 
  ungroup() %>% 
  mutate(subterritory = "All") %>% 
  bind_rows(monitoring_descriptors, .) %>% 
  arrange(territory, subterritory) %>% 
  arrange(., territory == "Entire Pacific region")

## 7.4 Reformat the data and export the table ----

monitoring_descriptors <- monitoring_descriptors %>% 
  mutate(nb_sites = as.character(format(nb_sites, big.mark = ",", scientific = FALSE)),
         nb_surveys = as.character(format(nb_surveys, big.mark = ",", scientific = FALSE)))

## 7.5 Export the table ----

### 7.5.1 In .xlsx format ---- 

openxlsx::write.xlsx(monitoring_descriptors, file = "figs/01_part-1/table-4.xlsx")

### 7.5.2 In .tex format ---- 

latex_table_line <- function(i, subterritory){
  
  color <- ifelse(i %% 2 == 0, "white", "secondcolor")
  
  if(subterritory == FALSE){
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{2}{|l|}{", monitoring_descriptors[i, "territory"], "} &", monitoring_descriptors[i, "nb_sites"], "&",
                     monitoring_descriptors[i, "nb_surveys"], "&", monitoring_descriptors[i, "nb_datasets"],
                     "&", monitoring_descriptors[i, "first_year"], "&", monitoring_descriptors[i, "last_year"]," \\\\ \\hline"))
    
  }else{
    
    line <- c(paste0("\\rowcolor{", color, "}"),
              paste0("\\multicolumn{1}{|l}{} & ", monitoring_descriptors[i, "subterritory"], " &", monitoring_descriptors[i, "nb_sites"], "&",
                     monitoring_descriptors[i, "nb_surveys"], "&", monitoring_descriptors[i, "nb_datasets"],
                     "&", monitoring_descriptors[i, "first_year"], "&", monitoring_descriptors[i, "last_year"]," \\\\ \\hline"))
    
  }
  
  return(line)
  
}

writeLines(c("\\begin{center}",
             "\\begin{tabular}{|ll|R{1.5cm}|R{1.5cm}|R{1.5cm}|R{1.5cm}|R{1.5cm}|}",
             "\\hline",
             "\\rowcolor{firstcolor}",
             "\\multicolumn{2}{|l|}{\\textcolor{white}{Countries and territories}} & \\textcolor{white}{Sites} & \\textcolor{white}{Surveys}  & \\textcolor{white}{Datasets} & \\textcolor{white}{First year} & \\textcolor{white}{Last year}\\\\ \\hline",
             map(1:8, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(9:11, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(12:17, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             map(18:22, ~ latex_table_line(i = ., subterritory = TRUE)) %>% unlist(),
             map(23:32, ~ latex_table_line(i = ., subterritory = FALSE)) %>% unlist(),
             paste0("\\rowcolor{secondcolor}"),
             paste0("\\multicolumn{2}{|l|}{\\textbf{", monitoring_descriptors[33, "territory"], "}} &", monitoring_descriptors[33, "nb_sites"], "&",
                    monitoring_descriptors[33, "nb_surveys"], "&", monitoring_descriptors[33, "nb_datasets"],"&", monitoring_descriptors[33, "first_year"],
                    "&", monitoring_descriptors[33, "last_year"]," \\\\ \\hline"),
             "\\end{tabular}",
             "\\end{center}"),
           paste0("figs/01_part-1/table-4.tex"))

## 7.6 Export table for each territory ----

### 7.6.1 Create the function ----

map_tex <- function(territory_i){
  
  data_i <- monitoring_descriptors %>% 
    filter(territory == territory_i)
  
  writeLines(c("\\begin{center}",
               "\\begin{tabular}{|>{\\raggedleft\\arraybackslash}m{3.75cm}|m{2.75cm}|}",
               "\\hline",
               "\\rowcolor{secondcolor}",
               paste0("Sites & ", data_i[1, "nb_sites"], " \\\\ \\hline"),
               "\\rowcolor{white}",
               paste0("Surveys & ", data_i[1, "nb_surveys"], " \\\\ \\hline"),
               "\\rowcolor{secondcolor}",
               paste0("Datasets & ", data_i[1, "nb_datasets"], " \\\\ \\hline"),
               "\\rowcolor{white}",
               paste0("First year & ", data_i[1, "first_year"], " \\\\ \\hline"),
               "\\rowcolor{secondcolor}",
               paste0("Last year & ", data_i[1, "last_year"], " \\\\ \\hline"),
               "\\end{tabular}",
               "\\end{center}"),
             paste0("figs/02_part-2/tbl-2/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}

### 7.6.2 Map over the function ----

map(setdiff(unique(monitoring_descriptors$territory), "Entire Pacific region"),
    ~map_tex(territory_i = .))
