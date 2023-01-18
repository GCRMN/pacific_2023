map_cyclone <- function(territory_i){
  
  data_ts_event_i <- data_ts_event %>% 
    filter(TERRITORY1 == territory_i)
  
  ggplot(data = data_ts_event_i, aes(x = time, y = wind_speed)) +
    geom_segment(aes(x = time, y = 0, xend = time, yend = wind_speed), linewidth = 0.75, col = "#2e3131") +
    geom_point(shape = 21, fill = "#d91e18", col = "white", size = 4) +
    geom_text(aes(y = wind_speed + 22, label = name), size = 3, family = font_choose_graph, col = "#d91e18") +
    geom_text(aes(y = wind_speed + 12, label = ts_dist), size = 2, family = font_choose_graph, col = "#2e3131") +
    lims(x = c(as.Date("1980-01-01"), as.Date("2023-12-31")), y = c(0, 300)) +
    labs(y = bquote("Wind speed (km."~h^-1*")"), x = "Time (years)") +
    theme_graph()
  
  ggsave(filename = paste0("figs/04_cyclones/cyclone-events_", str_replace_all(str_to_lower(territory_i), " ", "-"), "_01.png"),
         width = 7, height = 4, dpi = 600)
  
}

