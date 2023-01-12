export_tabular <- function(territory_i){
  
  data_maritime_area_i <- data_maritime_area %>% 
    filter(TERRITORY1 == territory_i) %>% 
    select(AREA_KM2) %>% 
    pull()
  
  data_reef_area_i <- data_reef_area %>% 
    filter(TERRITORY1 == territory_i) %>% 
    select(area) %>% 
    pull()
  
  data_population_i <- data_population %>% 
    filter(TERRITORY1 == territory_i) %>% 
    select(population) %>% 
    pull()
  
  writeLines(c("\\begin{tabular}{>{\\bfseries}>{\\color{color1}}rl}",
               paste0("Maritime area & ", data_maritime_area_i, " km\\textsuperscript{2} \\\\"),
               "Land area & 12,000 km\\textsuperscript{2} \\\\",
               paste0("Reef area & ", data_reef_area_i, " km\\textsuperscript{2} \\\\"),
               paste0("Population (2020) & ", data_population_i, " \\\\"),
               "\\end{tabular}"),
             paste0("figs/02_geographic-inf_", str_replace_all(str_to_lower(territory_i), " ", "-"), ".tex"))
  
}