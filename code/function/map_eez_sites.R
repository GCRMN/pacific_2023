map_eez_sites <- function(territory_i){
  
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
    
   # 4. Make the plot ----
    
    ggplot() +
      geom_sf(data = data_eez_i, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
      geom_sf(data = data_land, fill = "#363737", col = "grey") +
      geom_sf(data = data_benthic_i, aes(color = interval_class)) +
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
    
    # 4. Make the plot ----
    
    ggplot() +
      geom_sf(data = data_eez_i, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
      geom_sf(data = data_land, fill = "#363737", col = "grey") +
      geom_sf(data = data_benthic_i, aes(color = interval_class)) +
      theme_minimal() +
      scale_color_manual(values = c("red", "blue", "green", "purple", "pink"),
                        labels = c("1 year", "2-5 years", "6-10 years", "11-15 years", ">15 years"), 
                        drop = FALSE, name = "Number of years with data") +
      theme(axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  }
  
  # 5. Export the plot ----
  
  ggsave(filename = paste0("figs/03_eez-sites/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"), dpi = 600)
  
}