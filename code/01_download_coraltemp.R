# 1. Required packages ----

library(tidyverse)

# 2. Download SST NetCDF4 files ----

# 2.1 List files to download ----

list_url <- data.frame(date = seq(from = ymd("1985-01-01"), to = ymd("2023-11-30"), by = "1 day")) %>% 
  mutate(year = year(date),
         date = str_remove_all(date, "-"),
         url = paste0("https://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/sst/",
                      year,
                      "/coraltemp_v3.1_",
                      date,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,16])

# 2.2 Download files ----

for(i in 1:nrow(list_url)){
  
  if(file.exists(paste0("data/06_sst/", list_url[i, "filename"])) == FALSE){
    
    download.file(url = list_url[i, "url"],
                  destfile = paste0("data/06_sst/", list_url[i, "filename"]))
    
  }
  
}

# 3. Download DHW NetCDF4 files ----

# 3.1 List files to download ----

list_url <- data.frame(date = seq(from = ymd("1985-03-25"), to = ymd("2023-11-30"), by = "1 day")) %>% 
  mutate(year = year(date),
         date = str_remove_all(date, "-"),
         url = paste0("https://www.star.nesdis.noaa.gov/pub/sod/mecb/crw/data/5km/v3.1_op/nc/v1.0/daily/dhw/",
                      year,
                      "/ct5km_dhw_v3.1_",
                      date,
                      ".nc"),
         filename = str_split_fixed(url, "/", Inf)[,16])

# 3.2 Download files ----

for(i in 1:nrow(list_url)){

  if(file.exists(paste0("data/09_dhw/", list_url[i, "filename"])) == FALSE){
    
    download.file(url = list_url[i, "url"],
                  destfile = paste0("data/09_dhw/", list_url[i, "filename"]))
    
  }
  
}
