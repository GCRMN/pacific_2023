# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggrepel)
library(glue)
library(scales)
library(ggsflabel)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

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

### 3.2.4 Cyclones ----

load("data/05_cyclones/02_cyclones_extracted.RData")
load("data/05_cyclones/01_cyclones_lines.RData")
load("data/05_cyclones/01_cyclones_points.RData")

data_cyclones <- data_cyclones %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1) %>% 
  mutate(time_range = case_when(time > as_date("1980-01-01") & time <= as_date("1987-12-31") ~ "1980 - 1987",
                                time > as_date("1988-01-01") & time <= as_date("1995-12-31") ~ "1988 - 1995",
                                time > as_date("1996-01-01") & time <= as_date("2003-12-31") ~ "1996 - 2003",
                                time > as_date("2004-01-01") & time <= as_date("2011-12-31") ~ "2004 - 2011",
                                time > as_date("2012-01-01") & time <= as_date("2019-12-31") ~ "2012 - 2019",
                                time > as_date("2020-01-01") & time <= as_date("2023-12-31") ~ "2020 - 2023"),
         time_range = as.factor(time_range),
         time_range = fct_expand(time_range, "1980 - 1987", "1988 - 1995", "1996 - 2003", "2004 - 2011",
                                 "2012 - 2019", "2020 - 2023"),
         time_range = fct_relevel(time_range, c("1980 - 1987", "1988 - 1995", "1996 - 2003", "2004 - 2011",
                                                "2012 - 2019", "2020 - 2023")),
         name = str_to_sentence(name),
         max_saffir = as.factor(max_saffir)) %>% 
  # Add cyclone position by wind_speed
  arrange(territory, desc(wind_speed)) %>% 
  group_by(territory) %>% 
  mutate(position = row_number()) %>% 
  ungroup()

## 3.3 Create the function ----

map_eez <- function(territory_i){
  
  # 1. Filter ----
  
  data_eez_i <- data_eez %>% 
    filter(TERRITORY1 == territory_i)
  
  data_cyclones_i <- data_cyclones %>% 
    filter(territory == territory_i) %>% 
    # Add cyclone position by wind_speed
    arrange(time_range, desc(wind_speed)) %>% 
    group_by(time_range) %>% 
    mutate(position = row_number()) %>% 
    ungroup()
  
  data_ts_lines_i <- left_join(data_cyclones_i, data_ts_lines) %>% 
    st_as_sf() %>% 
    st_transform(crs = crs_selected)
  
  data_ts_points_i <- data_ts_points %>% 
    filter(ts_id %in% unique(data_cyclones_i$ts_id)) %>% 
    mutate(name = str_to_sentence(name)) %>% 
    left_join(., data_cyclones_i %>% select(ts_id, name, time_range, position)) %>% 
    st_transform(crs = crs_selected) %>% 
    mutate(saffir = case_when(wind_speed < 119 ~ 0,
                              wind_speed >= 119 & wind_speed <= 153 ~ 1,
                              wind_speed > 153 & wind_speed <= 177 ~ 2,
                              wind_speed > 177 & wind_speed <= 210 ~ 3,
                              wind_speed > 210 & wind_speed <= 251 ~ 4,
                              wind_speed > 251 ~ 5),
           saffir = as.factor(saffir),
           saffir = fct_expand(saffir, "0", "1", "2", "3", "4", "5"))
  
  data_label_i <- st_intersection(data_ts_lines_i, data_eez_i)
  
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
  
  # 3. Layer to mask external zone of eez_i ----
  
  data_alpha <- st_difference(data_bbox, data_eez_i)
  
  # 4. Select labels ----
  
  data_label_i <- st_intersection(data_ts_lines_i, data_bbox)
  
  # 5. Make the plot ----
  
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
    geom_sf(data = data_eez, color = "white", fill = NA) +
    geom_sf(data = data_eez_supp, color = "white", fill = NA) +
    geom_sf(data = data_land, fill = "grey", col = "darkgrey") +
    geom_sf(data = data_land_supp, fill = "grey", col = "darkgrey") +
    geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
    geom_sf(data = data_ts_lines_i %>% filter(position <= 3), col = "#6c7a89", size = 0.25) +
    geom_sf(data = data_ts_lines_i %>% filter(position > 3), col = "#6c7a89", size = 0.25) +
    geom_sf(data = data_ts_points_i %>% filter(position <= 3), aes(col = saffir), size = 1, show.legend = TRUE) +
    geom_sf_label_repel(data = data_label_i %>% filter(position <= 3), aes(label = name),
                        alpha = 0.75, size = 2.5, label.size  = NA) +
    scale_color_manual(breaks = c("0", "1", "2", "3", "4", "5"),
                       labels = c("Cat. 0", "Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                       values = c(palette_second[1:5], "black"),
                       name = "Saffir-Simpson category",
                       drop = FALSE) +
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
          legend.key = element_blank(),
          legend.direction = "horizontal") +
    guides(color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1,
                                override.aes = list(size = 4)))
  
  if(territory_i %in% c("Federated States of Micronesia", "Hawaii", "Tokelau", "Solomon Islands")){
    
    plot_i <- plot_i +
      facet_wrap(~time_range, ncol = 2, drop = FALSE)      
    
  }else{
    
    plot_i <- plot_i +
      facet_wrap(~time_range, ncol = 3, drop = FALSE)
    
  }
  
  # 6. Export the plot ----
  
  ggsave(filename = paste0("figs/02_part-2/fig-5/",
                           str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         plot = plot_i, dpi = fig_resolution)
  
}

map_eez(territory_i = "Pitcairn")

## 3.4 Map over the function (except PRIA) ----

map(setdiff(unique(data_eez$TERRITORY1),
            c("Palmyra Atoll", "Johnston Atoll",
              "Wake Island", "Jarvis Island",
              "Howland and Baker Islands")), # PRIA territories
    ~map_eez(territory = .))

## 3.5 Map for Pacific Remote Islands Area (PRIA) ----

### 3.5.1 Filter ----

data_eez_i <- data_eez %>% 
  filter(TERRITORY1 %in% c("Palmyra Atoll", "Johnston Atoll", "Wake Island", "Jarvis Island",
                           "Howland and Baker Islands"))

data_cyclones_i <- data_cyclones %>% 
  filter(territory %in% c("Palmyra Atoll", "Johnston Atoll", "Wake Island", "Jarvis Island",
                          "Howland and Baker Islands")) %>% 
  select(-territory) %>% 
  arrange(time_range, desc(wind_speed)) %>% 
  group_by(time_range) %>% 
  mutate(position = row_number()) %>% 
  ungroup() %>% 
  group_by(ts_id, time_range) %>% 
  filter(!(position != min(position))) %>% 
  ungroup()

data_ts_lines_i <- left_join(data_cyclones_i %>% select(ts_id, name, time_range, position), data_ts_lines) %>% 
  st_as_sf() %>% 
  st_transform(crs = crs_selected)

data_ts_points_i <- data_ts_points %>% 
  filter(ts_id %in% unique(data_cyclones_i$ts_id)) %>% 
  mutate(name = str_to_sentence(name)) %>% 
  left_join(., data_cyclones_i %>% select(ts_id, name, time_range, position)) %>% 
  st_transform(crs = crs_selected) %>% 
  mutate(saffir = case_when(wind_speed < 119 ~ 0,
                            wind_speed >= 119 & wind_speed <= 153 ~ 1,
                            wind_speed > 153 & wind_speed <= 177 ~ 2,
                            wind_speed > 177 & wind_speed <= 210 ~ 3,
                            wind_speed > 210 & wind_speed <= 251 ~ 4,
                            wind_speed > 251 ~ 5),
         saffir = as.factor(saffir),
         saffir = fct_expand(saffir, "0", "1", "2", "3", "4", "5"))

### 3.5.2 Create the bbox ----

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

### 3.5.3 Layer to mask external zone of eez_i ----

data_alpha <- data_eez_i %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_difference(data_bbox, .)

### 3.5.4 Select labels ----

data_label_i <- st_intersection(data_ts_lines_i %>% filter(position <= 3), data_bbox)

### 3.5.5 Make the plot ----

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
  geom_sf(data = data_eez, color = "white", fill = NA) +
  geom_sf(data = data_eez_supp, color = "white", fill = NA) +
  geom_sf(data = data_land, fill = "grey", col = "darkgrey") +
  geom_sf(data = data_land_supp, fill = "grey", col = "darkgrey") +
  geom_sf(data = data_alpha, fill = "white", alpha = 0.5) +
  geom_sf(data = data_ts_lines_i %>% filter(position <= 3), col = "#6c7a89", size = 0.25) +
  geom_sf(data = data_ts_lines_i %>% filter(position > 3), col = "#6c7a89", size = 0.25) +
  geom_sf(data = data_ts_points_i %>% filter(position <= 3), aes(col = saffir), size = 1, show.legend = TRUE) +
  geom_sf_label_repel(data = data_label_i, aes(label = name),
                      alpha = 0.75, size = 2.5, label.size  = NA) +
  scale_color_manual(breaks = c("0", "1", "2", "3", "4", "5"),
                     labels = c("Cat. 0", "Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                     values = c(palette_second[1:5], "black"),
                     name = "Saffir-Simpson category",
                     drop = FALSE) +
  coord_sf(xlim = c(x_min - ((x_max - x_min)*percent_margin_ltr/100),
                    x_max + ((x_max - x_min)*percent_margin_ltr/100)),
           ylim = c(y_min - ((y_max - y_min)*percent_margin_ltr/100),
                    y_max + ((y_max - y_min)*percent_margin_ltr/100)),
           expand = FALSE) +
  theme_minimal() +
  facet_wrap(~time_range, ncol = 2, drop = FALSE) +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null"),
        panel.background = element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        legend.position = "top",
        legend.key = element_blank(),
        legend.direction = "horizontal") +
  guides(color = guide_legend(title.position = "top", title.hjust = 0.5, nrow = 1,
                              override.aes = list(size = 4)))

### 3.5.6 Export the plot ----

ggsave(filename = "figs/02_part-2/fig-5/pria.png", plot = plot_i, dpi = fig_resolution)

# 4. Plots of cyclone maximum wind speed over time ----

## 4.1 Create the function ----

load("data/05_cyclones/02_cyclones_extracted.RData")

data_cyclones <- data_cyclones %>% 
  mutate(territory = case_when(territory %in% c("Gilbert Islands", "Line Group", "Phoenix Group") ~ "Kiribati",
                               territory %in% c("Palmyra Atoll", "Johnston Atoll",
                                                "Howland and Baker islands", "Wake Island",
                                                "Jarvis Island") ~ "PRIA",
                               TRUE ~ territory)) %>% 
  group_by(saffir) %>% 
  mutate(max_saffir = max(saffir)) %>% 
  ungroup() %>% 
  filter(max_saffir >= 1) %>% 
  mutate(name = str_to_sentence(name),
         max_saffir = as.factor(max_saffir)) %>% 
  # Add cyclone position by wind_speed
  arrange(territory, desc(wind_speed)) %>% 
  group_by(territory) %>% 
  mutate(position = row_number())

## 4.2 Create the function ----

map_cyclone_plot <- function(territory_i){
  
  # 1. Filter
  
  data_cyclones_i <- data_cyclones %>% 
    filter(territory == territory_i)
  
  # 2. Make the plot 
  
  plot_i <- ggplot(data = data_cyclones_i, aes(x = time, y = wind_speed)) +
    geom_point(aes(fill = max_saffir), color = "white", shape = 21, size = 4.5,
               show.legend = c(shape = TRUE)) +
    geom_label_repel(data = data_cyclones_i %>% filter(position %in% 1:15), # Label only the first 15 cyclones
                     aes(label = name, color = max_saffir), fill = "white", alpha = 0.9,
                     label.r = unit(0.4, "lines"), show.legend = FALSE) +
    scale_y_continuous(breaks = c(0, 50 ,100, 150, 200, 250, 300), limits = c(0, 300)) +
    scale_x_date(limits = c(ymd("1980-01-01"), ymd("2024-01-01"))) +
    coord_cartesian(ylim = c(14.25, 310)) +
    scale_fill_manual(breaks = c("1", "2", "3", "4", "5"),
                      labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                      values = c(palette_second[2:5], "black"),
                      name = "Saffir-Simpson\ncategory",
                      drop = FALSE) +
    scale_color_manual(breaks = c("1", "2", "3", "4", "5"),
                       labels = c("Cat. 1", "Cat. 2", "Cat. 3", "Cat. 4", "Cat. 5"),
                       values = c(palette_second[2:5], "black"),
                       name = "Saffir-Simpson\ncategory",
                       drop = FALSE) +
    guides(fill = guide_legend(override.aes = list(size = 4))) +
    labs(x = "Year", y = bquote("Wind speed (km."~h^-1*")")) +
    theme_graph() +
    theme(text = element_text(size = 13),
          legend.position = "right",
          legend.direction = "vertical",
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  # 3. Save the plot
  
  ggsave(filename = paste0("figs/02_part-2/fig-4/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         plot = plot_i, height = 3.5, width = 9, dpi = fig_resolution)
  
}

## 4.3 Map over the function ----

map(unique(data_cyclones$territory), ~map_cyclone_plot(territory_i = .))

# 5. Extract indicators of tropical storms per territory ----

## 5.1 Number of tropical storms ----

data_cyclone_a <- data_cyclones %>% 
  filter(saffir >= 1) %>% 
  group_by(territory) %>% 
  summarise(n_ts = n_distinct(ts_id)) %>% 
  ungroup()

## 5.2 Number of tropical storms with wind > 100 km.h ----

data_cyclone_b <- data_cyclones %>% 
  filter(saffir >= 1 & wind_speed >= 100) %>% 
  group_by(territory) %>% 
  summarise(n_ts_100 = n_distinct(ts_id)) %>% 
  ungroup()

## 5.3 Tropical storm with the highest wind speed ----

data_cyclone_summary <- data_cyclones %>% 
  group_by(territory) %>% 
  filter(wind_speed == max(wind_speed, na.rm = TRUE)) %>% 
  ungroup() %>% 
  select(territory, ts_id, name, time, wind_speed, saffir, dist) %>% 
  left_join(., data_cyclone_a) %>% 
  left_join(., data_cyclone_b)
