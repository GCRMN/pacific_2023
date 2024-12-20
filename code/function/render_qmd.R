render_qmd <- function(territory_i, upload_drive = FALSE){
  
  require(rmarkdown)
  require(googledrive)
  
  territory_name <- str_replace_all(str_replace_all(str_to_lower(territory_i), " ", "-"),  "---", "-")

  nb_chapter <- data_eez %>% filter(TERRITORY1 == territory_i) %>% select(nb) %>% pull()
  
  file_name <- paste0(str_pad(nb_chapter, width = 2, pad = "0"), "_", territory_name, ".docx")
  
  if(file.exists(paste0("doc/", file_name)) == FALSE){
    
    render("code/function/create_chapter_doc.qmd", 
           output_file = file_name,
           output_dir = "doc/",
           quiet = TRUE)
    
  }
  
  if(upload_drive == TRUE){
    
    drive_put(media = paste0("doc/", file_name),
              path = paste0("GCRMN Pacific report/07_part-2_syntheses-countries-territories/", file_name))
    
  }
  
}
