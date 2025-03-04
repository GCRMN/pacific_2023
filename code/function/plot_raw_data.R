plot_raw_data <- function(i, nb_subplots){
  
  data_benthic_cover_i <- data_benthic_cover %>%  
    arrange(territory) %>% 
    mutate(nb = ceiling(as.numeric(as.factor(territory))/nb_subplots)) %>% 
    filter(nb == i)
  
  data_benthic_cover_mean_i <- data_benthic_cover_i %>%  
    group_by(year, category, territory, color) %>% 
    summarise(mean = mean(measurementValue))
  
  ggplot(data = data_benthic_cover_i, aes(x = year, y = measurementValue, fill = color, color = color)) +
    geom_point(alpha = 0.08, shape = 16, show.legend = FALSE) +
    geom_point(data = data_benthic_cover_mean_i, aes(x = year, y = mean), shape = 23, color = "black") +
    facet_grid(territory~category) +
    theme_graph() +
    scale_fill_identity() +
    scale_color_identity() +
    lims(x = c(1980, 2023), y = c(-1, 101)) +
    theme(strip.background = element_blank(),
          axis.text = element_text(size = 10),
          strip.text.y = element_text(size = 8, face = "bold"), 
          strip.text.x = element_text(face = "bold"),
          panel.border = element_rect(color = "grey", fill = NA, linewidth = 0.5)) +
    labs(x = "Year", y = "Cover (%)")
  
  ggsave(paste0("../figs/04_supp/01_data-explo/raw-data_", i, ".png"), width = 10, height = 12)
  
}
