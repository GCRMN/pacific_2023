map_ortho <- function(territory_i){
  
  data_eez_i <- data_eez %>% 
    filter(TERRITORY1 == territory_i) %>% 
    st_as_sfc() %>% 
    st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")
  
  data_eez_all <- data_eez %>% 
    filter(TERRITORY1 != territory_i) %>% 
    st_as_sfc() %>% 
    st_transform(., "+proj=ortho +lat_0=0 +lon_0=-175")
  
  ggplot() +
    geom_sf(data = b, fill = "#363737", col = "grey") +
    geom_sf(data = i, fill = "#ebf5fd") +
    geom_sf(data = data_eez, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
    geom_sf(data = data_eez_i, color = "#d64541", fill = "#e08283", alpha = 0.75) +
    theme_minimal()
  
  ggsave(filename = paste0("figs/01_ind-map_", str_replace_all(str_to_lower(territory_i), " ", "-"), "_01.png"),
         width = 4, height = 4, dpi = 600)

}

