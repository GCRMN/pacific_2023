# **Status and Trends of Coral Reefs of the Pacific**


## 1. Introduction

## 2. Code

### A. Cleaning and selection (`01_`)

* `01_benthic_data_sources.R` Extract data sources from [gcrmndb_benthos](https://github.com/JWicquart/gcrmndb_benthos).
* `01_clean_bathy.R` Clean bathymetry data from [Natural Earth Data](https://www.naturalearthdata.com/downloads/10m-physical-vectors/).
* `01_clean_cyclones.R` Clean cyclone dataset from [IBTrACS](https://www.ncei.noaa.gov/products/international-best-track-archive).
* `01_clean_eez.R` Select and clean economic exclusive zones (EEZ) from [marineregions](https://marineregions.org/downloads.php).
* `01_clean_land.R` Clean Pacific land data (high spatial resolution) from [Princeton University](https://maps.princeton.edu/).
* `01_clean-reef-distribution.R` Select Pacific coral reefs distribution from the [Tropical Coral Reefs of the World](https://datasets.wri.org/dataset/tropical-coral-reefs-of-the-world-500-m-resolution-grid) World Resources Institute (WRI).
* `01_select_benthic-data.R` Select [gcrmndb_benthos](https://github.com/JWicquart/gcrmndb_benthos) data to use in the analyses.

### B. Extraction and models (`02_`)

* `02_extract_cyclones.R` Extract cyclones for each EEZ.
* `02_extract_dhw-percent.R` Extract percent of coral reefs under DHW for each EEZ.
* `02_extract_dhw-time-series.R` Extract maximum DHW within each EEZ.
* `02_extract_elevation.js` Extract mean land elevation for each territory using Google Earth Engine (GEE). Estimated values are used as geographic information's.
* `02_extract_land-area.js` Extract total land area for each territory using GEE. Estimated values are used as geographic information's.
* `02_extract_map-dhw.R` Create raster with the mean annual DHW for each cell.
* `02_extract_population.js` Extract human population for each territory using GEE. Estimated values are used as geographic information's.
* `02_reef-buffer.js` Create coral reef distribution buffer of 100 km. Used to extract cyclones occurrence.
* `02_extract_sst-time-series.R` Extract SST and SST anomaly on coral reefs of each EEZ.
* `02_model_benthic-data_grid.R` Create weights for the benthic model based on coral reef extent.
* `02_model_benthic-data.Rmd` Machine learning model to estimate temporal trends of hard coral and algae cover. 
* `02_model_data-exploration.Rmd` Data exploration for benthic cover.

### C. Figures and tables (`03_`)

* `03_benthic-cover-trends.Rmd` Produce figures of hard coral and algae cover trends.
* `03_geographic-informations.R` Combine and export geographic information's.
* `03_materials-and-methods.R` Produce figures for the Materials and Methods section of the report.
* `03_pacific_cyclones.R` Produce figure and map of cyclones that occurred over the Pacific region.
* `03_pacific_enso.R` Produce figure of ENSO.
* `03_pacific_map.R` Produce map of the entire Pacific GCRMN region with description of EEZ.
* `03_pacific_map-dhw.R` Produce map of DHW for each year.
* `03_pacific_population.R` Produce figure of human population change between 2000 and 2020.
* `03_pacific_spatio-temporal.R` Produce figure and map of spatio-temporal distribution of benthic cover monitoring for the Pacific region.
* `03_pacific_sst.R` Produce figures of maximum DHW and DHW percent for the Pacific region.
* `03_territories_cyclones.R` Produce figures and maps of cyclones that occurred on each EEZ.
* `03_territories_map-bathy.R` Produce EEZ maps.
* `03_territories_map-sphere.R` Produce hemisphere maps for each EEZ.
* `03_territories_spatio-temporal.R` Produce figure and map of spatio-temporal distribution of benthic cover monitoring for each EEZ.
* `03_territories_sst.R` Produce figures of SST, SST anomaly and DHW for each EEZ.

### D. Functions

* `data_descriptors.R` Get number of sites, surveys, datasets, first and last year of monitoring.
* `graphical_par.R` Graphical parameters, including colors and fonts.
* `theme_graph.R` Main ggplot theme for the maps of the reports.
* `theme_map.R` Main ggplot theme for the plots of the reports.


## 3. Reproducibility parameters

```
R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 18363)

Matrix products: default

locale:
[1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
[3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
[5] LC_TIME=French_France.utf8    

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] s2_1.1.2        ggspatial_1.1.7 magrittr_2.0.3  extrafont_0.19 
 [5] sf_1.0-9        lubridate_1.9.1 forcats_1.0.0   stringr_1.5.0  
 [9] dplyr_1.0.10    purrr_1.0.1     readr_2.1.3     tidyr_1.3.0    
[13] tibble_3.1.8    ggplot2_3.4.0   tidyverse_1.3.2

loaded via a namespace (and not attached):
 [1] Rcpp_1.0.10         class_7.3-21        assertthat_0.2.1   
 [4] utf8_1.2.3          R6_2.5.1            cellranger_1.1.0   
 [7] backports_1.4.1     reprex_2.0.2        e1071_1.7-13       
[10] httr_1.4.4          pillar_1.8.1        nngeo_0.4.6        
[13] rlang_1.0.6         googlesheets4_1.0.1 readxl_1.4.1       
[16] rstudioapi_0.14     data.table_1.14.6   extrafontdb_1.0    
[19] textshaping_0.3.6   googledrive_2.0.0   munsell_0.5.0      
[22] proxy_0.4-27        broom_1.0.3         compiler_4.2.2     
[25] modelr_0.1.10       systemfonts_1.0.4   pkgconfig_2.0.3    
[28] tidyselect_1.2.0    fansi_1.0.4         crayon_1.5.2       
[31] tzdb_0.3.0          dbplyr_2.3.0        withr_2.5.0        
[34] wk_0.7.1            grid_4.2.2          jsonlite_1.8.4     
[37] Rttf2pt1_1.3.12     gtable_0.3.1        lifecycle_1.0.3    
[40] DBI_1.1.3           units_0.8-1         scales_1.2.1       
[43] KernSmooth_2.23-20  cli_3.6.0           stringi_1.7.12     
[46] farver_2.1.1        fs_1.6.0            xml2_1.3.3         
[49] ellipsis_0.3.2      ragg_1.2.5          generics_0.1.3     
[52] vctrs_0.5.2         tools_4.2.2         glue_1.6.2         
[55] hms_1.1.2           timechange_0.2.0    colorspace_2.1-0   
[58] gargle_1.3.0        classInt_0.4-8      rvest_1.0.3        
[61] haven_2.5.1  
```