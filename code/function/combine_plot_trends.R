combine_plot_trends <- function(territory_i, categ_type){
  
  if(categ_type == "categories"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(territory == territory_i) %>% 
      filter(category %in% c("Hard coral", "Coralline algae", "Macroalgae",
                             "Turf algae", "Other fauna")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Hard coral", "Coralline algae", "Macroalgae",
                                   "Turf algae", "Other fauna"),
             category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae",
                                    "Turf algae", "Other fauna"))
    
    plot_list <- map(unique(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, ncol = 2)
    
    if(territory_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-13.png", plot = plot_i, height = 11, width = 9, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 12, width = 9, dpi = fig_resolution)
      
    }
    
  }else if(categ_type == "families"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(territory == territory_i) %>% 
      filter(category %in% c("Acroporidae", "Merulinidae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Acroporidae", "Merulinidae"),
             category = fct_relevel(category, "Acroporidae", "Merulinidae"))
    
    plot_list <- map(unique(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon"))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(territory_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-14.png", plot = plot_i, height = 4, width = 8, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-7/",
                               str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 8, dpi = fig_resolution)
      
    }
    
  }else{
    
    stop("categ_type argument can only take 'categories' or 'families'")
    
  }
}
