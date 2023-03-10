map_eez <- function(territory_i, data_parameters){
  
  if(territory_i %in% c("Fiji", "Wallis and Futuna")){
    
    # 1. Filter EEZ data ----
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(crs = 4326) %>% 
      st_transform(., crs = 3460) %>% 
      st_union(.)
    
    # 2. Filter benthic data ----
    
    data_benthic_i <- data_benthic %>% 
      filter(territory == territory_i) %>% 
      st_transform(., crs = 3460)
    
    # 3. Load boundary data ----
    
    data_land <- st_read(list.files(path = paste0("data/01_background-shp/02_princeton/", 
                                                  str_replace_all(str_to_lower(territory_i), 
                                                                  " ", "-"), "/"), 
                                    pattern = ".shp$", 
                                    full.names = TRUE))
    
    data_eez_i %<>% # Special pipe from magrittr
      st_buffer(10) # To join polygon (remove vertical line)
    
    # 4. Misc parameters ----
    
    scale_bar_pos <- data_parameters %>% 
      filter(TERRITORY1 == territory_i) %>% 
      select(scale_pos) %>% 
      pull()
    
    plot_width <- data_parameters %>% 
      filter(TERRITORY1 == territory_i) %>% 
      select(width) %>% 
      pull()
    
    plot_height <- data_parameters %>% 
      filter(TERRITORY1 == territory_i) %>% 
      select(height) %>% 
      pull()
    
    # 5. Make the plot ----
    
    ggplot() +
      geom_sf(data = data_eez_i, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
      geom_sf(data = data_land, fill = "#363737", col = "grey") +
      geom_sf(data = data_benthic_i, col = "#d64541") +
      annotation_scale(location = scale_bar_pos, width_hint = 0.3, text_family = font_choose_graph, 
                       text_cex = 0.75, style = "bar", line_width = 0.5,  height = unit(0.05, "cm"),
                       pad_x = unit(0.75, "cm"), pad_y = unit(0.75, "cm"), bar_cols = c("black", "black")) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }else{
    
    # 1. Filter EEZ data ----
    
    data_eez_i <- data_eez %>% 
      filter(TERRITORY1 == territory_i) %>% 
      st_transform(crs = 4326)
    
    # 2. Filter benthic data ----
    
    data_benthic_i <- data_benthic %>% 
      filter(territory == territory_i)
    
    # 3. Load boundary data ----
    
    data_land <- st_read(list.files(path = paste0("data/01_background-shp/02_princeton/", 
                                                  str_replace_all(str_to_lower(territory_i), 
                                                                  " ", "-"), "/"), 
                                    pattern = ".shp$", 
                                    full.names = TRUE))
    
    # 4. Misc parameters ----
    
    scale_bar_pos <- data_parameters %>% 
      filter(TERRITORY1 == territory_i) %>% 
      select(scale_pos) %>% 
      pull()
    
    plot_width <- data_parameters %>% 
      filter(TERRITORY1 == territory_i) %>% 
      select(width) %>% 
      pull()
    
    plot_height <- data_parameters %>% 
      filter(TERRITORY1 == territory_i) %>% 
      select(height) %>% 
      pull()
    
    # 5. Make the plot ----
    
    ggplot() +
      geom_sf(data = data_eez_i, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
      geom_sf(data = data_land, fill = "#363737", col = "grey") +
      geom_sf(data = data_benthic_i, col = "#d64541") +
      annotation_scale(location = scale_bar_pos, width_hint = 0.3, text_family = font_choose_graph, 
                       text_cex = 0.75, style = "bar", line_width = 0.5,  height = unit(0.05, "cm"),
                       pad_x = unit(0.75, "cm"), pad_y = unit(0.75, "cm"), bar_cols = c("black", "black")) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }
  
  # 5. Export the plot ----
  
  ggsave(filename = paste0("figs/02_eez-bathy/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = plot_width, height = plot_height, dpi = 600)
  
}