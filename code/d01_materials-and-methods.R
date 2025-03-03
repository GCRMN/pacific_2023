# 1. Required packages ----

library(tidyverse) # Core tidyverse packages
library(sf)
sf_use_s2(FALSE)
library(patchwork)
library(ggtext)
library(ggspatial) # For annotation_scale function

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_graph.R")

# 3. Cyclones ----

## 3.1. Load data ----

### 3.1.1 Coral reef distribution ----

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  filter(TERRITORY1 == "Marshall Islands") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

### 3.1.2 EEZ ----

load("data/01_background-shp/03_eez/data_eez.RData")

data_eez <- data_eez %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid() %>% 
  filter(TERRITORY1 == "Marshall Islands")

### 3.1.3 Coral reef distribution 100 km buffer ----

data_reef_buffer <- st_read("data/03_reefs-area_wri/clean_buffer/reef_buffer.shp") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid() %>% 
  st_intersection(., data_eez) %>% 
  select(TERRITORY1) %>% 
  filter(TERRITORY1 == "Marshall Islands")

### 3.1.4 Cyclones lines ----

load("data/05_cyclones/01_cyclones_lines.RData")

data_ts_lines <- data_ts_lines %>% 
  st_transform(crs = 4326) %>% 
  filter(ts_id %in% c("1997333N06194", "2009238N12189"))

data_ts_lines <- st_intersection(data_ts_lines, data_eez)

### 3.1.5 Cyclones points ----

load("data/05_cyclones/01_cyclones_points.RData")

data_ts_points <- data_ts_points %>% 
  st_transform(crs = 4326) %>% 
  filter(ts_id %in% c("1997333N06194", "2009238N12189"))

data_ts_points <- st_intersection(data_ts_points, data_eez)

## 3.2. Make the plots ----

### 3.2.1 Plot A ----

plot_a <- ggplot() +
  geom_sf(data = data_eez, fill = NA, color = "darkgrey", linetype = "dashed") +
  geom_sf(data = data_reef_buffer, fill = "#f1a9a0", alpha = 0.5) +
  geom_sf(data = data_reef, col = "#d64541") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph, hjust = 0.5),
        panel.background = element_rect(colour = NA, fill = NA, linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**1.** Create 100 km coral reef buffer")

### 3.2.2 Plot B ----

plot_b <- ggplot() +
  geom_sf(data = data_eez, fill = NA, color = "darkgrey", linetype = "dashed") +
  geom_sf(data = data_reef_buffer) +
  geom_sf(data = data_reef, col = "#d64541") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "1997333N06194"), col = "#446cb3") +
  geom_sf(data = data_ts_points %>% filter(ts_id == "1997333N06194"), shape = 21, 
          col = "#446cb3", fill = "#446cb3") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "2009238N12189")) +
  geom_sf(data = data_ts_points %>% filter(ts_id == "2009238N12189"), shape = 21, fill = "white") +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph, hjust = 0.5),
        panel.background = element_rect(colour = NA, fill = NA, linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**2.** Filter cyclone crossing the buffer")

### 3.2.3 Plot C ----

plot_c <- ggplot() +
  geom_sf(data = data_eez, fill = NA, color = "darkgrey", linetype = "dashed") +
  geom_sf(data = data_reef_buffer) +
  geom_sf(data = data_reef, col = "#d64541") +
  geom_sf(data = data_ts_lines %>% filter(ts_id == "1997333N06194")) +
  geom_sf(data = data_ts_points %>% filter(ts_id == "1997333N06194"), shape = 21, fill = "white") +
  geom_sf(data = data_ts_points %>% 
            filter(ts_id == "1997333N06194") %>% 
            filter(row_number() == 11), shape = 21, fill = "#446cb3", size = 2) +
  theme_minimal() +
  theme(plot.title = element_markdown(family = font_choose_graph, hjust = 0.5),
        panel.background = element_rect(colour = NA, fill = NA, linewidth = 1),
        panel.grid = element_blank(),
        axis.text = element_blank()) +
  labs(title = "**3.** Extract cyclone distance and wind speed")

## 3.3. Combine the plots ----

plot_a + plot_b + plot_c + plot_layout(ncol = 3)

## 3.4. Export the plot ----

ggsave("figs/03_methods/fig-1.png", height = 5, width = 15, dpi = fig_resolution)

# 4. SST change and warming rate ----

## 4.1. Load data ----

load("data/09_misc/data-sst.RData")
load("data/01_background-shp/03_eez/data_eez.RData")

data_sst <- data_eez %>% 
  # Join to add TERRITORY1
  st_drop_geometry() %>% 
  select(TERRITORY1, GEONAME) %>% 
  left_join(data_sst, .) %>%
  filter(TERRITORY1 == "Fiji")

## 4.2 Calculate the warming rate ----

extract_coeff <- function(data){
  
  model <- lm(sst ~ date, data = data)
  
  results <- summary(model)$coefficients
  
  results <- tibble(intercept = results[1, "Estimate"],
                    slope = results[2, "Estimate"])
  
  return(results)
  
}

data_warming <- data_sst %>% 
  # Convert date as numeric
  mutate(date = as.numeric(as_date(date))) %>% 
  # Extract linear model coefficients
  group_by(TERRITORY1) %>% 
  group_modify(~extract_coeff(data = .x)) %>% 
  ungroup() %>% 
  # Calculate increase in SST over the period
  mutate(min_date = as.numeric(as_date(min(data_sst$date))),
         max_date = as.numeric(as_date(max(data_sst$date)))) %>% 
  mutate(sst_increase = ((max_date)*slope+intercept) - ((min_date)*slope+intercept)) %>% 
  select(-min_date, -max_date) %>% 
  # Calculate the warming rate (°C per year)
  mutate(warming_rate = sst_increase/(year(max(data_sst$date))-year(min(data_sst$date))))

## 4.3 Transform the data ----

data_sst <- data_sst %>% 
  mutate(date = as_date(date),
         slope = data_warming$slope,
         intercept = data_warming$intercept,
         date_num = as.numeric(date),
         sst_linear = slope*date_num+intercept)

data_sst_point <- data_sst %>% 
  filter(row_number() == 1 | row_number() == nrow(.))

## 4.4 Make the plot ----

ggplot(data = data_sst) +
  geom_line(aes(x = date, y = sst), color = "black", linewidth = 0.25) +
  geom_line(aes(x = date, y = sst_linear), color = "red") +
  geom_point(data = data_sst_point, aes(x = date, y = sst_linear), size = 3, color = "red") +
  annotate(geom = "text", x = as_date("1983-11-01"),
           y = as.numeric(data_sst_point[1,"sst_linear"]), size = 5, hjust = 1,
           label = "y[a]", family = font_choose_graph, color = "red", parse = TRUE) +
  annotate(geom = "text", x = as_date("2024-10-01"),
           y = as.numeric(data_sst_point[2,"sst_linear"]), size = 5, hjust = -0.25,
           label = "y[b]", family = font_choose_graph, color = "red", parse = TRUE) +
  labs(x = "Year", y = "SST (°C)") +
  lims(x = c(as_date("1983-11-01"), as_date("2025-06-01"))) +
  theme_graph()
  
## 4.5 Export the plot ----

ggsave("figs/03_methods/fig-2.png", height = 4.5, width = 5, dpi = fig_resolution)

# 5. Grid for ML predictions ----

## 5.1 Load data ----

data_land <- st_read("data/01_background-shp/02_princeton/palau/PLW_adm0.shp")

load("data/11_model-data/data_predictors_pred.RData")

data_predictors_pred <- data_predictors_pred %>% 
  filter(territory == "Palau") %>% 
  select(decimalLongitude, decimalLatitude) %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

load("data/09_misc/data-benthic.RData")

data_benthic <- data_benthic %>% 
  filter(territory == "Palau") %>% 
  select(decimalLatitude, decimalLongitude) %>% 
  distinct() %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

data_reef <- st_read("data/03_reefs-area_wri/clean/pacific_reef.shp") %>% 
  filter(TERRITORY1 == "Palau") %>% 
  st_transform(crs = 4326) %>% 
  st_wrap_dateline() %>% 
  st_make_valid()

## 5.2 Map of sites with observed data ----

plot_a <- ggplot() +
  geom_sf(data = data_reef, fill = "lightblue", color = "lightblue") +
  geom_sf(data = data_land, fill = "darkgrey") +
  geom_sf(data = data_benthic, size = 1, col = "red") +
  coord_sf(xlim = c(134, 134.8), y = c(6.75, 8.40)) +
  theme_minimal() +
  labs(title = "Sites with observed data") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = font_choose_map)) +
  annotation_scale(location = "br", width_hint = 0.25, text_family = font_choose_graph, 
                   text_cex = 0.7, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))

## 5.3 Map of sites on which to make the predictions ----

plot_b <- ggplot() +
  geom_sf(data = data_reef, fill = "lightblue", color = "lightblue") +
  geom_sf(data = data_land, fill = "darkgrey") +
  geom_sf(data = data_predictors_pred, size = 1, col = "red") +
  coord_sf(xlim = c(134, 134.8), y = c(6.75, 8.40)) +
  theme_minimal() +
  labs(title = "Sites for making predictions") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        text = element_text(family = font_choose_map)) +
  annotation_scale(location = "br", width_hint = 0.25, text_family = font_choose_graph, 
                   text_cex = 0.7, style = "bar", line_width = 1,  height = unit(0.045, "cm"),
                   pad_x = unit(0.5, "cm"), pad_y = unit(0.35, "cm"), bar_cols = c("black", "black"))

## 5.4 Combine the plots ----

plot_a + plot_spacer() + plot_b

## 5.5 Export the plot ----

ggsave("figs/03_methods/fig-3.png", height = 5, width = 8, dpi = fig_resolution)
