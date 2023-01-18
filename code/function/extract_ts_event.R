extract_ts_event <- function(territory_i, ts_id_i, ts_lines, ts_points){
  
  data_ts_lines_i <- ts_lines %>% filter(ts_id == ts_id_i)
  data_ts_points_i <- ts_points %>% filter(ts_id == ts_id_i)
  data_reef_i <- data_reef %>% filter(TERRITORY1 == territory_i)
  
  results <- st_join(data_reef_i, data_ts_points_i, join = st_nearest_feature) %>% # TS speed and wind at nearest point
    st_drop_geometry() %>% 
    mutate(ts_dist = as.numeric(st_distance(data_reef_i, data_ts_lines_i))) # distance between site and TS path
  
  return(results)
  
}