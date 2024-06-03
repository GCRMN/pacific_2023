# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")
source("code/function/theme_map.R")

# 3. Define CRS ----

## 3.1 Change CRS ----

crs_selected <- "+proj=eqc +lat_ts=0 +lat_0=0 +lon_0=160 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

## 3.2 Define the offset ----

correction_offset <- 180 - 160 # Here 160 is the same value than +lon_0 from crs_selected

## 3.3 Define a long and slim polygon that overlaps the meridian line ----

correction_polygon <- st_polygon(x = list(rbind(c(-0.0001 - correction_offset, 90),
                                                c(0 - correction_offset, 90),
                                                c(0 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, -90),
                                                c(-0.0001 - correction_offset, 90)))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# 4. Load data ----

## 4.1 EEZ ----

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

## 4.2 Land ----

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

## 4.3 Regional capital ----

data_place <- st_read("data/01_background-shp/01_ne/ne_10m_populated_places/ne_10m_populated_places.shp") %>% 
  st_transform(crs = crs_selected) %>% 
  st_filter(., data_eez) %>%  
  filter(FEATURECLA %in% c("Admin-0 region capital", "Admin-0 capital", "Admin-1 capital")) %>% 
  mutate(ADM0NAME = ifelse(ADM0NAME == "United States of America", ADM1NAME, ADM0NAME)) %>% 
  filter(!(ADM0NAME == "Papua New Guinea" & FEATURECLA == "Admin-1 capital"),
         !(ADM0NAME == "Palau" & FEATURECLA == "Admin-1 capital"))

## 4.4 Benthic data ----

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

## 4.5 Reef distribution ----

data_reefs <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp")

## 4.6 Labels ----

data_labels <- read.csv2("data/09_misc/labels_pos_maps.csv") %>% 
  mutate(across(c(legend_x, legend_y, fig_width, fig_height), ~as.numeric(.))) %>% 
  st_as_sf(coords = c("label_long", "label_lat"), crs = 4326) %>% 
  st_transform(crs = crs_selected)
  
# 5. Map of monitoring sites ----

## 5.1 Create the function ----

map_territory <- function(territory_i){
  
  data_labels_i <- data_labels %>% 
    filter(territory == territory_i)
  
  data_buffer <- data_land %>% 
    filter(TERRITORY1 == territory_i) %>% 
    st_buffer(dist = 75000) %>% 
    st_union()
  
  plot_i <- ggplot() +
    geom_sf(data = data_reefs %>% filter(TERRITORY1 == territory_i),
            color = palette_first[2], fill = palette_first[2]) +
    geom_sf(data = data_buffer, fill = NA, linetype = "dashed") +
    geom_sf(data = data_land %>% filter(TERRITORY1 == territory_i)) +
    geom_sf_text(data = data_labels %>% filter(territory == territory_i), aes(label = label),
                 family = font_choose_map, color = "#363737", size = 3) +
    geom_sf(data = data_benthic_sites %>% filter(territory == territory_i),
            aes(color = interval_class), size = 1.5, show.legend = TRUE)  +
    scale_color_manual(values = palette_second,
                       breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                       labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                       drop = FALSE,
                       name = "Number of years\nwith data") +
    guides(color = guide_legend(override.aes = list(size = 3.5))) +
    geom_sf(data = data_place %>% filter(ADM0NAME == territory_i),
            fill = palette_first[2], color = "white", shape = 23, size = 3.5) +
    theme_map() +
    theme(panel.grid.major = element_line(color = "#ecf0f1", linetype = "dashed", linewidth = 0.25),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.border = element_rect(color = 'black', fill = NA),
          plot.background = element_blank(),
          legend.background = element_rect(fill = "#ecf0f1"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          legend.position = "inside",
          legend.position.inside = c(unique(data_labels_i$legend_x), unique(data_labels_i$legend_y)),
          legend.direction = "vertical") +
    annotation_scale(location = unique(data_labels_i$scalebar), width_hint = 0.25, text_family = font_choose_map, 
                     text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))
  
  if(territory_i == "Papua New Guinea"){
    
    plot_i <- plot_i +
      coord_sf(xlim = c(142, 161))
    
    }else if (territory_i == "Vanuatu"){
      
      plot_i <- plot_i +
        scale_x_continuous(breaks = c(166, 168, 170)) +
        coord_sf(xlim = c(165, 171), ylim = c(-22, -12))

    }else if (territory_i == "Palau"){

      plot_i <- plot_i +
        scale_x_continuous(breaks = c(131, 133, 135)) +
        coord_sf(xlim = c(130.15, 135.35), ylim = c(2, 9))
      
    }else if (territory_i == "Tonga"){

      plot_i <- plot_i +
        scale_x_continuous(breaks = c(-177, -176, -175, -174, -173)) +
        coord_sf(xlim = c(-177.5, -173.5))
      
    }else if (territory_i == "Guam"){
      
      plot_i <- plot_i +
        scale_x_continuous(breaks = c(144.2, 144.6, 145)) +
        coord_sf(xlim = c(144, 145.2), ylim = c(12.3, 14))
      
    }else if (territory_i == "Northern Mariana Islands"){

      plot_i <- plot_i +
        scale_x_continuous(breaks = c(144, 145, 146, 147)) +
        scale_y_continuous(breaks = c(13, 14, 15, 16, 17, 18, 19, 20, 21)) +
        coord_sf(xlim = c(143.8, 147), y = c(13.5, 21))
      
    }else if (territory_i == "French Polynesia"){
  
      plot_i <- plot_i +
        coord_sf(xlim = c(-157.5, -132.5))
      
    }else if (territory_i == "Marshall Islands"){

      plot_i <- plot_i +
        coord_sf(xlim = c(160, 173))
      
    }

  ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         plot = plot_i,
         height = unique(data_labels_i$fig_height),
         width = unique(data_labels_i$fig_width),
         dpi = fig_resolution)
  
}

## 5.2 Map over the function ----

map(c("Solomon Islands", "New Caledonia", "Vanuatu",
      "Papua New Guinea", "Palau", "Tonga", "Guam",
      "Northern Mariana Islands", "French Polynesia",
      "Marshall Islands", "Pitcairn", "Cook Islands"), ~map_territory(territory_i = .))

# 6. Plots of number of sites per interval class ----

## 6.1 Make the plot ----

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

## 6.2 Save the plot ----

ggsave(filename = "figs/04_supp/01_data-explo/04_surveys_duration.png",
       width = 15, height = 12, dpi = 600)

# 7. Plots of number of surveys per year ----

## 7.1 Transform the data ----

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

## 7.2 Make the plot ----

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

## 7.3 Save the plot ----

ggsave(filename = "figs/04_supp/01_data-explo/04_surveys_year.png",
       width = 15, height = 12, dpi = 600)
