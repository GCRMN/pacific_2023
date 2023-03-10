ncdf_extract <- function(ncdf_i){
  
  ncdf <- rast(ncdf_i)
  
  my_summary <- function(x) c(mean = mean(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))
  
  sst_i <- terra::extract(x = ncdf, y = data_reef, fun = my_summary) %>% 
    select(-starts_with("sea")) %>% 
    mutate(date = unique(time(ncdf)))
  
  return(sst_i)
  
}