prepare_benthic_data <- function(data){
  
  result <- data %>% 
    # 1. Define and filter categories
    mutate(category = case_when(subcategory == "Macroalgae" ~ "Macroalgae",
                                subcategory == "Turf algae" ~ "Turf algae",
                                subcategory == "Coralline algae" ~ "Coralline algae",
                                TRUE ~ category)) %>% 
    filter(category %in% c("Hard coral", "Macroalgae", "Turf algae", "Coralline algae")) %>% 
    # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Regenerate 0 values
    group_by(datasetID) %>% 
    complete(category,
             nesting(region, subregion, ecoregion, country, territory, locality,
                     habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                     year, month, day, eventDate),
             fill = list(measurementValue = 0)) %>%
    ungroup() %>% 
    # 4. Average of benthic cover per transect (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 5. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100)
    
  result <- data %>% 
    # 1. Define and filter categories
    mutate(category = case_when(family == "Acroporidae" ~ "Acroporidae",
                                family == "Pocilloporidae" ~ "Pocilloporidae",
                                family == "Poritidae" ~ "Poritidae",
                                TRUE ~ category)) %>% 
    filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
    # 2. Sum of benthic cover per sampling unit (site, transect, quadrat) and category
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, eventID, category) %>% 
    summarise(measurementValue = sum(measurementValue)) %>% 
    ungroup() %>% 
    # 3. Regenerate 0 values
    group_by(datasetID) %>% 
    complete(category,
             nesting(region, subregion, ecoregion, country, territory, locality,
                     habitat, parentEventID, decimalLatitude, decimalLongitude, verbatimDepth,
                     year, month, day, eventDate),
             fill = list(measurementValue = 0)) %>%
    ungroup() %>% 
    # 4. Average of benthic cover per transect (i.e. mean of photo-quadrats)
    # This avoid getting semi-quantitative data (e.g. when there is only 10 points per photo-quadrat)
    group_by(datasetID, region, subregion, ecoregion, country, territory, locality, habitat, parentEventID,
             decimalLatitude, decimalLongitude, verbatimDepth, year, month, day, eventDate, category) %>% 
    summarise(measurementValue = mean(measurementValue)) %>% 
    ungroup() %>% 
    # 5. Remove values greater than 100 (unlikely but included to avoid any issues later)
    filter(measurementValue <= 100) %>% 
    # Bind with main benthic categories
    bind_rows(., result)

  return(result)
  
}
