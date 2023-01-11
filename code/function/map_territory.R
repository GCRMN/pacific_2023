map_territory <- function(territory_i){
  
  data_eez_all <- data_eez %>% 
    filter(TERRITORY1 != territory_i)
  
  data_eez_i <- data_eez %>% 
    filter(TERRITORY1 == territory_i)
  
  ggplot() +
    # EEZ
    geom_sf(data = data_eez_all, color = "#5c97bf", fill = "#bbd9eb", alpha = 0.75) +
    geom_sf(data = data_eez_i, color = "#d64541", fill = "#e08283", alpha = 0.75) +
    # Background map
    geom_sf(data = data_map, fill = "#363737", col = "grey") +
    # Country boundaries
    geom_sf(data = data_countries, fill = "#363737", col = "grey") +
    # Graphical aspects
    coord_sf(ylim = c(-4000000, 4000000), xlim = c(-3500000, 11000000), expand = FALSE) +
    scale_x_continuous(breaks = c(180, 160, 140, -160, -140, -120)) +
    theme(text = element_text(family = font_choose_map),
          panel.background = element_rect(fill = "white"),
          panel.grid = element_blank(),
          panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
          axis.title = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x.top = element_text())
  
    ggsave(filename = paste0("../figs/01_map-pacific_", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 6, height = 3.4, dpi = 600)
  
}