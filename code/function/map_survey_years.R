map_survey_years <- function(territory_i){
  
  # 1. Filter data ----
  
  data_benthic_i <- data_benthic %>% 
    filter(territory == territory_i)
  
  # 2. Make the plot ----
  
  ggplot(data = data_benthic_i, aes(x = year)) +
    # By default its density, width*density*100 gives percentage
    geom_histogram(binwidth = 1, aes(y = after_stat(width*density*100)),
                   color = "black", fill = "#5c97bf", color = "#446cb3") +
    lims(x = c(1970, 2024)) +
    labs(x = "Year", y = "Surveys (%)", title = "B") +
    theme_graph() +
    theme(plot.title = element_text(size = 20))
  
  # 3. Export the plot ----
  
  ggsave(filename = paste0("figs/08_survey-years/", str_replace_all(str_to_lower(territory_i), " ", "-"), ".png"),
         width = 4, height = 3, dpi = 600)
  
}