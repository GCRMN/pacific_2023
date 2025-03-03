combine_model_data <- function(model = "xgb"){
  
  # 1. List of RData files to combine
  
  data_files <- tibble(path = list.files("data/12_model-output/", full.names = TRUE)) %>% 
    filter(str_detect(path, model) == TRUE & str_detect(path, "result") == TRUE & str_detect(path, "_all") == FALSE)
  
  # 2. Create a function to load RData files
  
  load_rdata <- function(path){
    
    load(file = path)
    
    return(model_results)
    
  }
  
  # 3. Combine files
  
  model_results <- map(data_files$path, ~load_rdata(path = .)) %>% 
    map_df(., ~ as.data.frame(map(.x, ~ unname(nest(.))))) %>% 
    map(., bind_rows)
  
  # 4. Return the result
  
  save(model_results, file = "data/12_model-output/model_results_all.RData")
  
  # 5. Add colors
  
  # 5.1 Create the function
  
  add_colors <- function(data){
    
    data <- data %>% 
      mutate(color = case_when(category == "Hard coral" ~ palette_second[2],
                               category == "Macroalgae" ~ palette_second[4],
                               category == "Turf algae" ~ palette_second[5],
                               category == "Coralline algae" ~ palette_second[3],
                               category == "Acroporidae" ~ palette_second[4],
                               category == "Pocilloporidae" ~ palette_second[3],
                               category == "Poritidae" ~ palette_second[2]),
             text_title = case_when(category == "Hard coral" ~ 
                                      glue("**A.**<span style='color:{color}'> {category}</span>"),
                                    category == "Coralline algae" ~ 
                                      glue("**B.**<span style='color:{color}'> {category}</span>"),
                                    category == "Macroalgae" ~ 
                                      glue("**C.**<span style='color:{color}'> {category}</span>"),
                                    category == "Turf algae" ~ 
                                      glue("**D.**<span style='color:{color}'> {category}</span>"),
                                    category == "Other fauna" ~ 
                                      glue("**E.**<span style='color:{color}'> {category}</span>"),
                                    category == "Acroporidae" ~ 
                                      glue("**A.**<span style='color:{color}'> {category}</span>"),
                                    category == "Pocilloporidae" ~ 
                                      glue("**B.**<span style='color:{color}'> {category}</span>"),
                                    category == "Poritidae" ~ 
                                      glue("**C.**<span style='color:{color}'> {category}</span>")))
    
    return(data)
    
  }
  
  ## 5.2 Add colors ----
  
  model_results <- model_results %>% 
    map(., ~ .x %>% add_colors)
  
  ## 6. Return results ----
  
  return(model_results)
  
}
