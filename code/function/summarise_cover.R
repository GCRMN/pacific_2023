summarise_cover <- function(data, category_i){
  
  data <- data %>% 
    # 1. Filter the category i
    filter(category == category_i) %>% 
    # 2. Sum of benthic cover per sampling unit
    group_by(datasetID, higherGeography, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Remove useless variables
    select(-higherGeography, -country, -locality, -habitat, -eventDate) %>% 
    # 4. Convert to factors
    mutate_if(is.character, factor)
  
  return(data)
  
}
