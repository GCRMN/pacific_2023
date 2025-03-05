combine_plot_trends <- function(territory_i, categ_type){
  
  if(categ_type == "categories"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(year >= 1990 & year <= 2022) %>% 
      filter(territory == territory_i) %>% 
      filter(category %in% c("Hard coral", "Coralline algae", "Macroalgae",
                             "Turf algae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Hard coral", "Coralline algae", "Macroalgae",
                                   "Turf algae"),
             category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae",
                                    "Turf algae"))
    
    max_y <- max(as.numeric(data_trends_i$upper_ci_95), na.rm = TRUE)
    
    plot_list <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon", max_y = max_y))
    
    plot_i <- wrap_plots(plot_list, ncol = 2)
    
    if(territory_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-13.png", plot = plot_i, height = 7, width = 9, dpi = fig_resolution)  
      
    }else if(territory_i %in% c("Gilbert Islands", "Line Group", "Phoenix Group")){
      
      data_trends_i <- data_trends$smoothed_trends %>% 
        filter(year >= 1990 & year <= 2022) %>% 
        filter(territory_i %in% c("Gilbert Islands", "Line Group", "Phoenix Group")) %>% 
        filter(category %in% c("Hard coral", "Coralline algae", "Macroalgae",
                               "Turf algae")) %>% 
        mutate(category = as.factor(category),
               category = fct_expand(category, "Hard coral", "Coralline algae", "Macroalgae",
                                     "Turf algae"),
               category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae",
                                      "Turf algae"))
      
      max_y <- max(as.numeric(data_trends_i$upper_ci_95), na.rm = TRUE)
      
      plot_list1 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Gilbert Islands"),
                                     show_obs_data = "ribbon", max_y = max_y))
      
      plot_list2 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Line Group"),
                                     show_obs_data = "ribbon", max_y = max_y))
      
      plot_list3 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Phoenix Group"),
                                     show_obs_data = "ribbon", max_y = max_y)) 
      
      plot_i <- wrap_plots(wrap_plots((ggplot() + theme_void() + labs(title = "1. Gilbert Islands") +
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list1, ncol = 4), 
                           wrap_plots((ggplot() + theme_void() + labs(title = "2. Line Group") + 
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list2, ncol = 4),
                           wrap_plots((ggplot() + theme_void() + labs(title = "3. Phoenix Group") + 
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list3, ncol = 4),
                           nrow = 6) +
        plot_layout(heights = c(0.1, 1, 0.1, 1, 0.1, 1))
      
      ggsave(filename = "figs/02_part-2/fig-6/kiribati.png", plot = plot_i, height = 11, width = 13, dpi = fig_resolution)
      
    }else if(territory_i %in% c("Howland and Baker Islands", "Jarvis Island", "Johnston Atoll",
                                "Palmyra Atoll", "Wake Island")){
      
      data_trends_i <- data_trends$smoothed_trends %>% 
        filter(year >= 1990 & year <= 2022) %>% 
        filter(territory_i %in% c("Howland and Baker Islands", "Jarvis Island", "Johnston Atoll",
                                  "Palmyra Atoll", "Wake Island")) %>% 
        filter(category %in% c("Hard coral", "Coralline algae", "Macroalgae",
                               "Turf algae")) %>% 
        mutate(category = as.factor(category),
               category = fct_expand(category, "Hard coral", "Coralline algae", "Macroalgae",
                                     "Turf algae"),
               category = fct_relevel(category, "Hard coral", "Coralline algae", "Macroalgae",
                                      "Turf algae"))
      
      max_y <- max(as.numeric(data_trends_i$upper_ci_95), na.rm = TRUE)
      
      plot_list1 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Howland and Baker Islands"),
                                     show_obs_data = "ribbon", max_y = max_y))
      
      plot_list2 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Johnston Atoll"),
                                     show_obs_data = "ribbon", max_y = max_y))
      
      plot_list3 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Palmyra Atoll"),
                                     show_obs_data = "ribbon", max_y = max_y))
      
      plot_list4 <- map(c("Hard coral", "Coralline algae", "Macroalgae", "Turf algae"),
                        ~plot_trends(category_i = ., data_trends_i = data_trends_i %>%
                                       filter(territory == "Wake Island"),
                                     show_obs_data = "ribbon", max_y = max_y))
      
      plot_i <- wrap_plots(wrap_plots((ggplot() + theme_void() + labs(title = "1. Howland and Baker Islands") +
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list1, ncol = 4), 
                           wrap_plots((ggplot() + theme_void() + labs(title = "2. Johnston Atoll") + 
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list2, ncol = 4),
                           wrap_plots((ggplot() + theme_void() + labs(title = "3. Palmyra Atoll") +
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list3, ncol = 4),
                           wrap_plots((ggplot() + theme_void() + labs(title = "4. Wake Island") +
                                         theme_graph() + theme(plot.title = element_text(hjust = 0, face = "bold"),
                                                               plot.margin = margin(30, 0, 0, 0))),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(),
                                      ggplot() + theme_void(), nrow = 1),
                           wrap_plots(plot_list4, ncol = 4),
                           nrow = 10) +
        plot_layout(heights = c(0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1, 0.1, 1))
      
      ggsave(filename = "figs/02_part-2/fig-6/pria.png", plot = plot_i, height = 14.5, width = 13, dpi = fig_resolution)
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-6/",
                               str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 7, width = 9, dpi = fig_resolution)
      
    }
    
  }else if(categ_type == "families"){
    
    data_trends_i <- data_trends$smoothed_trends %>% 
      filter(year >= 1990 & year <= 2022) %>% 
      filter(territory == territory_i) %>% 
      filter(category %in% c("Acroporidae", "Pocilloporidae", "Poritidae")) %>% 
      mutate(category = as.factor(category),
             category = fct_expand(category, "Acroporidae", "Pocilloporidae", "Poritidae"),
             category = fct_relevel(category, "Acroporidae", "Pocilloporidae", "Poritidae"))
    
    max_y <- max(as.numeric(data_trends_i$upper_ci_95), na.rm = TRUE)
    
    plot_list <- map(unique(data_trends_i$category),
                     ~plot_trends(category_i = ., data_trends_i = data_trends_i, show_obs_data = "ribbon", max_y = max_y))
    
    plot_i <- wrap_plots(plot_list, nrow = 1)
    
    if(territory_i == "All"){
      
      ggsave(filename = "figs/01_part-1/fig-14.png", plot = plot_i, height = 4, width = 12, dpi = fig_resolution)  
      
    }else{
      
      ggsave(filename = paste0("figs/02_part-2/fig-7/",
                               str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"), "---", "-"), ".png"),
             plot = plot_i, height = 4, width = 12, dpi = fig_resolution)
      
    }
    
  }else{
    
    stop("categ_type argument can only take 'categories' or 'families'")
    
  }
}
