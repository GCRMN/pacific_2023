extract_ts_eez <- function(territory_i){
  
  # 1. Extract the EEZ i ----
  
  data_reef_buffer_i <- data_reef_buffer %>% 
    filter(TERRITORY1 == territory_i)
  
  # 2. Filter TS lines crossing the buffer ----
  
  data_ts_lines_i <- st_filter(data_ts_lines, data_reef_buffer_i, join = st_intersects)
  
  # 3. Extract TS points based on those filtered for TS lines ----
  
  data_ts_points_i <- data_ts_points %>% 
    filter(ts_id %in% unique(data_ts_lines_i$ts_id))
  
  # 5. Extract information for each TS ----
  
  results <- future_map_dfr(unique(data_ts_lines_i$ts_id), 
                            ~extract_ts_event(ts_id_i = .,
                                              territory_i = territory_i, 
                                              ts_lines = data_ts_lines_i, 
                                              ts_points = data_ts_points_i), 
                            .options = furrr_options(seed = TRUE))
  
  return(results)
  
}