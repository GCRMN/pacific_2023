# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(ggspatial) # For annotation_scale function
library(patchwork)

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

rm(list_shp)

## 4.3 Benthic data ----

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

## 4.4 Reef distribution ----

data_reefs <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp")

data_reefs_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

## 4.5 Labels ----

data_labels <- read.csv2("data/09_misc/labels_pos_maps.csv", encoding = "latin1") %>% 
  st_as_sf(coords = c("label_long", "label_lat"), crs = 4326) %>% 
  st_transform(crs = crs_selected)
  
# 5. Map of monitoring sites ----

## 5.1 Create the function for the base map ----

base_map <- function(territory_i, legend_x, legend_y, scalebar_pos){
  
  data_labels_i <- data_labels %>% 
    filter(territory == territory_i)

  plot_i <- ggplot() +
    geom_sf(data = data_reefs %>% 
              filter(TERRITORY1 == territory_i),
            color = palette_first[2], fill = palette_first[2]) +
    geom_sf(data = data_reefs_buffer %>% filter(TERRITORY1 == territory_i),
            fill = NA, linetype = "dashed") +
    geom_sf(data = data_land %>% 
              filter(TERRITORY1 == territory_i)) +
    geom_sf_label(data = data_labels %>% 
                   filter(territory == territory_i),
                 aes(label = label),
                 family = font_choose_map, color = "#ecf0f1",
                 size = 3.5, fill = "#ecf0f1", alpha = 0.75) +
    geom_sf_text(data = data_labels %>% 
                    filter(territory == territory_i),
                  aes(label = label),
                  family = font_choose_map, color = "#363737", size = 3.5) +
    geom_sf(data = data_benthic_sites %>%
              filter(territory == territory_i) %>% 
              arrange(interval_class),
            aes(color = interval_class),
            size = 1.5, show.legend = ifelse(is.na(legend_x) == TRUE, FALSE, TRUE))  +
    scale_color_manual(values = palette_second,
                       breaks = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"),
                       labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                       drop = FALSE,
                       name = "Number of years\nwith data") +
    guides(color = guide_legend(override.aes = list(size = 3.5))) +
    theme_map() +
    theme(panel.grid.major = element_line(color = "#ecf0f1", linetype = "solid", linewidth = 0.25),
          panel.background = element_rect(fill = "white", color = "black"),
          panel.border = element_rect(color = 'black', fill = NA),
          plot.background = element_blank(),
          legend.background = element_rect(fill = "#ecf0f1"),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 9),
          legend.position = "inside",
          legend.position.inside = c(legend_x, legend_y),
          legend.direction = "vertical") +
    annotation_scale(location = scalebar_pos, width_hint = 0.25, text_family = font_choose_map, 
                     text_cex = 0.8, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                     pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))
  
  return(plot_i)
  
}

## 5.2 Create the function to produce the maps ----

map_territory <- function(territory_i){
  
  if(territory_i == "Kiribati"){
    
    plot_a <- base_map(territory_i = "Gilbert Islands", legend_x = NA, legend_y = 0.8, scalebar_pos = "bl") +
      labs(title = "Gilbert Islands")
    
    plot_b <- base_map(territory_i = "Phoenix Group", legend_x = NA, legend_y = 0.8, scalebar_pos = "tr") +
      labs(title = "Phoenix Group")
    
    plot_c <- base_map(territory_i = "Line Group", legend_x = 0.75, legend_y = 0.8, scalebar_pos = "bl") +
      coord_sf(label_axes = "-NE-") +
      labs(title = "Line Group")
    
    plot_i <- (plot_a / plot_b) | plot_c
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 7.5, width = 10, dpi = fig_resolution)
    
  }else if(territory_i == "PRIA"){
    
    plot_a <- base_map(territory_i = "Palmyra Atoll", legend_x = 0.5, legend_y = 0.8, scalebar_pos = "bl") +
      labs(title = "Palmyra Atoll")
    
    plot_b <- base_map(territory_i = "Johnston Atoll", legend_x = 0.5, legend_y = 0.8, scalebar_pos = "bl") +
      labs(title = "Johnston Atoll") +
      scale_x_continuous(breaks = c(-170.2, -169.8, -169.4, -169))
    
    plot_c <- base_map(territory_i = "Jarvis Island", legend_x = 0.5, legend_y = 0.8, scalebar_pos = "bl") +
      labs(title = "Jarvis Island") +
      scale_x_continuous(breaks = c(-160.6, -160.2, -159.8, -159.4))
    
    plot_d <- base_map(territory_i = "Howland and Baker Islands", legend_x = 0.5, legend_y = 0.8, scalebar_pos = "bl") +
      labs(title = "Howland and Baker Islands") +
      scale_x_continuous(breaks = c(-177, -176.6, -176.2)) +
      coord_sf(xlim = c(-177, -176), ylim = c(0, 1))
    
    plot_e <- base_map(territory_i = "Wake Island", legend_x = 0.5, legend_y = 0.8, scalebar_pos = "bl") +
      scale_x_continuous(breaks = c(166, 166.4, 166.8, 167.2)) +
      labs(title = "Wake Island")

    plot_i <- plot_a + plot_b + plot_c + plot_d + plot_e + guide_area() + plot_layout(guides = "collect", ncol = 3)
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 8, width = 12, dpi = fig_resolution)
    
  }else if(territory_i == "French Polynesia"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.15, legend_y = 0.825, scalebar_pos = "br") +
      coord_sf(xlim = c(-157.5, -132.5))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6.5, width = 7, dpi = fig_resolution)
    
  }else if(territory_i == "New Caledonia"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.35, legend_y = 0.3, scalebar_pos = "tr")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 5, width = 9, dpi = fig_resolution)
    
  }else if(territory_i == "Palau"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.275, legend_y = 0.8, scalebar_pos = "br")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6.25, width = 5, dpi = fig_resolution)
    
  }else if(territory_i == "Vanuatu"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.8, legend_y = 0.85, scalebar_pos = "bl") +
      coord_sf(xlim = c(165, 171.5))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 7, width = 5, dpi = fig_resolution)
    
  }else if(territory_i == "Solomon Islands"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.875, legend_y = 0.8, scalebar_pos = "br")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 8.5, dpi = fig_resolution)
    
  }else if(territory_i == "Cook Islands"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.75, legend_y = 0.55, scalebar_pos = "tl")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6.5, width = 5, dpi = fig_resolution)
    
  }else if(territory_i == "Tokelau"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.75, legend_y = 0.75, scalebar_pos = "bl")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 7.75, dpi = fig_resolution)
    
  }else if(territory_i == "Wallis and Futuna"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.15, legend_y = 0.825, scalebar_pos = "br") +
      coord_sf(crs = crs_selected) +
      scale_x_continuous(breaks = c(180, -179, -178, -177, -176, -175))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 7.25, dpi = fig_resolution)
    
  }else if(territory_i == "Marshall Islands"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.15, legend_y = 0.175, scalebar_pos = "tl")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 7, dpi = fig_resolution)
    
  }else if(territory_i == "Papua New Guinea"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.875, legend_y = 0.3, scalebar_pos = "bl") +
      coord_sf(xlim = c(142, 161))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 5.75, width = 8.5, dpi = fig_resolution)
    
  }else if(territory_i == "Northern Mariana Islands"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.25, legend_y = 0.5, scalebar_pos = "br") +
      scale_x_continuous(breaks = c(144, 145, 146, 147)) +
      scale_y_continuous(breaks = c(13, 14, 15, 16, 17, 18, 19, 20, 21)) +
      coord_sf(xlim = c(143.8, 147), y = c(13.5, 21))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 9, width = 4, dpi = fig_resolution)
    
  }else if(territory_i == "Federated States of Micronesia"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.125, legend_y = 0.3, scalebar_pos = "tr")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 4, width = 10, dpi = fig_resolution)
    
  }else if(territory_i == "Tonga"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.2, legend_y = 0.6, scalebar_pos = "br") +
      scale_x_continuous(breaks = c(-179, -177, -175, -173)) +
      coord_sf(xlim = c(-178, -172), ylim = c(-24, -14))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 5, dpi = fig_resolution)
    
  }else if(territory_i == "Samoa"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.85, legend_y = 0.8, scalebar_pos = "bl")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 5.5, width = 7.5, dpi = fig_resolution)
    
  }else if(territory_i == "American Samoa"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.8, legend_y = 0.8, scalebar_pos = "bl")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6.25, width = 5.5, dpi = fig_resolution)
    
  }else if(territory_i == "Pitcairn"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.65, legend_y = 0.5, scalebar_pos = "tr") +
      coord_sf(ylim = c(-26, -23))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 4.5, width = 9, dpi = fig_resolution)
    
  }else if(territory_i == "Guam"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.1775, legend_y = 0.825, scalebar_pos = "br")
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6.5, width = 5.75, dpi = fig_resolution)
    
  }else if(territory_i == "Hawaii"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.16, legend_y = 0.3, scalebar_pos = "tr") +
      coord_sf(crs = crs_selected) +
      scale_x_continuous(breaks = c(180, -175, -170, -165, -160, -155))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 4, width = 8.5, dpi = fig_resolution)
    
  }else if(territory_i == "Fiji"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.8, legend_y = 0.85, scalebar_pos = "bl") +
      coord_sf(crs = crs_selected) +
      scale_x_continuous(breaks = c(175, 177.5, 180, -177.5))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6.5, width = 5.25, dpi = fig_resolution)
    
  }else if(territory_i == "Tuvalu"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.2, legend_y = 0.2, scalebar_pos = "tr") +
      coord_sf(crs = crs_selected) +
      scale_x_continuous(breaks = c(176, 178, 180, -178))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 5.25, dpi = fig_resolution)
    
  }else if(territory_i == "Nauru"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.16, legend_y = 0.3, scalebar_pos = "tr") +
      coord_sf(xlim = c(166, 167.8), ylim = c(-1.4, 0.4))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 6, width = 6, dpi = fig_resolution)
    
  }else if(territory_i == "Niue"){
    
    plot_i <- base_map(territory_i = territory_i, legend_x = 0.85, legend_y = 0.25, scalebar_pos = "tl") +
      scale_x_continuous(breaks = c(-170.5, -169.5, -168.5, -167.5))
    
    ggsave(filename = paste0("figs/02_part-2/fig-6/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
           plot = plot_i, height = 5.25, width = 7, dpi = fig_resolution)
    
  }
  
}

## 5.3 Map over the function ----

map(c("Kiribati", "French Polynesia", "New Caledonia",
      "Palau", "Vanuatu", "Solomon Islands", "Cook Islands",
      "Tokelau", "Wallis and Futuna", "Marshall Islands",
      "Papua New Guinea", "Northern Mariana Islands",
      "Federated States of Micronesia", "Tonga", "PRIA",
      "Samoa", "American Samoa", "Pitcairn", "Guam", "Hawaii",
      "Fiji", "Tuvalu", "Nauru", "Niue"), ~map_territory(territory_i = .))

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
