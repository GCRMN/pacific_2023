plot_sst_region <- function(raster_path, raster_year){
  
  raster_i <- rast(raster_path)*0.01
  
  plot_i <- ggplot() +
    geom_spatraster(data = raster_i) +
    geom_sf(data = data_map) +
    geom_sf(data = data_eez, fill = NA) +
    scale_fill_gradientn(colors = rev(brewer.pal(n = 11, name = "RdBu")), 
                         breaks = seq(-5, 5, 1), 
                         limits = c(-5, 5)) +
    coord_sf(ylim = c(-30, 30), xlim = c(-170, -100)) +
    labs(title = raster_year, fill = "SST anomaly (°C)") +
    theme(text = element_text(family = "Open Sans"),
          legend.text = element_text(family = "Open Sans"),
          plot.title = element_text(face = "bold", hjust = 0.5),
          panel.border = element_rect(colour = "black", fill = NA))
  
  return(plot_i)
  
}