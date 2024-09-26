# 1. Load packages ----

library(tidyverse)
library(googledrive)

# 2. Get the files to upload on Google Drive ----

data_paths <- tibble(media = list.files("figs/02_part-2/", recursive = TRUE, full.names = TRUE)) %>% 
  filter(str_detect(media, "case-studies") == FALSE & str_detect(media, "tbl") == FALSE) %>% 
  mutate(fig = str_split_fixed(media, "/", 4)[,3],
         territory = str_split_fixed(media, "/", 4)[,4],
         territory = str_split_fixed(territory, "\\.", 2)[,1],
         territory = str_replace_all(territory, "pria", "pacific-remote-island-area")) %>%
  filter(!(territory %in% c("gilbert-islands", "line-group", "phoenix-group", "howland-and-baker-islands",
                          "johnston-atoll", "palmyra-atoll", "wake-island"))) %>% 
  filter(fig != "fig-7")

# 3. Define the path and file names on Google Drive ----

territories <- tibble(territory = unique(data_paths$territory)) %>% 
  arrange(territory) %>% 
  mutate(number = paste0(str_pad(row_number(), pad = 0, width = 2)))

data_paths <- left_join(data_paths, territories) %>% 
  mutate(path = paste0("GCRMN Pacific report/07_part-2_syntheses-countries-territories/",
                       number, "_", territory, "/", fig, ".png")) %>% 
  select(-number) %>% 
  arrange(territory)

# 4. Upload all the files ----

## 4.1 Create a function to upload a file ----

upload_drive <- function(index_i){

  drive_put(media = as.character(data_paths[index_i,"media"]),
            path = as.character(data_paths[index_i,"path"]))
  
}

## 4.2 Map over the function ----

map(1:nrow(data_paths), ~upload_drive(index_i = .))
