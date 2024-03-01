slice_sample_group <- function(data, territory_i){
  
  data_i <- data %>% 
    filter(territory == territory_i)
  
  nb <- nrow(data_i)
  
  results <- slice_sample(.data = data_i, n = nb, replace = TRUE)
  
  return(results)
  
} 