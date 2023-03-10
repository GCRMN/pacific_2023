map_sst <- function(territory_i){
  
  # 1. Filter SST data ----
  
  data_sst_i <- data_sst %>% 
    filter(TERRITORY1 == territory_i) 
  
  # 2. Make the plot ----
  
  # 2.1 SST --
  
  plot_a <- ggplot(data = data_sst_i, aes(x = date, y = sst)) +
    geom_line(color = "black", linewidth = 0.25) +
    geom_smooth(method = "lm", se = FALSE, color = "#446CB3") +
    geom_hline(yintercept = unique(data_sst_i$mean_sst), linetype = "dashed", color = "#d64541", linewidth = 1) +
    labs(x = NULL, y = "SST (°C)", title = "A")
  
  # 2.2 SST anomaly --
  
  plot_b <- ggplot(data = data_sst_i, aes(x = date, y = sst_anom_mean)) +
    geom_line(color = "black", linewidth = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", linewidth = 1) +
    labs(x = "Year", y = "SST anomaly (°C)", title = "B")
  
  # 2.3 Combine the plot --
  
  plot_a + plot_b + plot_layout(ncol = 1)
  
  # 3. Export the plot ----
  
  ggsave(filename = paste0("figs/05_sst/sst_", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 4.5, height = 6.5, dpi = 600)
  
}