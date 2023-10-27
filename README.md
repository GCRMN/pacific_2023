
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **Status and Trends of Coral Reefs of the Pacific**

## 1. Introduction

## 2. Code

### A. Cleaning and selection (`01_`)

- `01_benthic_data_sources.R` Extract data sources from
  [gcrmndb_benthos](https://github.com/JWicquart/gcrmndb_benthos).
- `01_clean_bathy.R` Clean bathymetry data from [Natural Earth
  Data](https://www.naturalearthdata.com/downloads/10m-physical-vectors/).
- `01_clean_cyclones.R` Clean cyclone dataset from
  [IBTrACS](https://www.ncei.noaa.gov/products/international-best-track-archive).
- `01_clean_eez.R` Select and clean economic exclusive zones (EEZ) from
  [marineregions](https://marineregions.org/downloads.php).
- `01_clean_land.R` Clean Pacific land data (high spatial resolution)
  from [Princeton University](https://maps.princeton.edu/).
- `01_clean-reef-distribution.R` Select Pacific coral reefs distribution
  from the [Tropical Coral Reefs of the
  World](https://datasets.wri.org/dataset/tropical-coral-reefs-of-the-world-500-m-resolution-grid)
  World Resources Institute (WRI).
- `01_select_benthic-data.R` Select
  [gcrmndb_benthos](https://github.com/JWicquart/gcrmndb_benthos) data
  to use in the analyses.

### B. Extraction and models (`02_`)

#### B1. Indicators

- `02_extract_indicator_cyclones` Extract cyclones for each EEZ.
- `02_extract_dhw-percent.R` Extract percent of coral reefs under DHW
  for each EEZ.
- `02_extract_dhw-time-series.R` Extract maximum DHW within each EEZ.
- `02_extract_elevation.js` Extract mean land elevation for each
  territory using Google Earth Engine (GEE). Estimated values are used
  as geographic information’s.
- `02_extract_land-area.js` Extract total land area for each territory
  using GEE. Estimated values are used as geographic information’s.
- `02_extract_map-dhw.R` Create raster with the mean annual DHW for each
  cell.
- `02_extract_population.R` Extract human population for each territory.
  Estimated values are used as geographic information’s.
- `02_reef-buffer.js` Create coral reef distribution buffer of 100 km.
  Used to extract cyclones occurrence.
- `02_extract_sst-time-series.R` Extract SST and SST anomaly on coral
  reefs of each EEZ.

#### B2. Predictors

- `02_extract_predictor_population.js` Extract total human population
  within 10 km from each site as a predictor, using GEE.
- `02_extract_predictor_elevation.js` Extract mean land elevation within
  10 km from each site as a predictor, using GEE.
- `02_extract_predictor_reef-extent.js` Extract total coral reef extent
  within 10 km from each site as a predictor, using GEE.

#### B3. Models

- `02_model_data-exploration.Rmd` Data exploration for benthic cover.
- `02_model_benthic-data_grid.R` Create weights for the benthic model
  based on coral reef extent.
- `02_model_benthic-cover.R` Machine learning model to estimate temporal
  trends of hard coral and algae cover.

### C. Figures and tables (`03_`)

- `03_benthic-cover-trends.Rmd` Produce figures of hard coral and algae
  cover trends.
- `03_geographic-informations.R` Combine and export geographic
  information’s.
- `03_materials-and-methods.R` Produce figures for the Materials and
  Methods section of the report.
- `03_pacific_cyclones.R` Produce figure and map of cyclones that
  occurred over the Pacific region.
- `03_pacific_enso.R` Produce figure of ENSO.
- `03_pacific_map.R` Produce map of the entire Pacific GCRMN region with
  description of EEZ.
- `03_pacific_map-dhw.R` Produce map of DHW for each year.
- `03_pacific_population.R` Produce figure of human population change
  between 2000 and 2020.
- `03_pacific_rel-reef-area.R` Produce figure of relative reef area of
  Pacific territories.
- `03_pacific_spatio-temporal.R` Produce figure and map of
  spatio-temporal distribution of benthic cover monitoring for the
  Pacific region.
- `03_pacific_sst.R` Produce figures of maximum DHW and DHW percent for
  the Pacific region.
- `03_territories_cyclones.R` Produce figures and maps of cyclones that
  occurred on each EEZ.
- `03_territories_map-bathy.R` Produce EEZ maps.
- `03_territories_map-sphere.R` Produce hemisphere maps for each EEZ.
- `03_territories_spatio-temporal.R` Produce figure and map of
  spatio-temporal distribution of benthic cover monitoring for each EEZ.
- `03_territories_sst.R` Produce figures of SST, SST anomaly and DHW for
  each EEZ.

### D. Functions

- `data_descriptors.R` Get number of sites, surveys, datasets, first and
  last year of monitoring.
- `graphical_par.R` Graphical parameters, including colors and fonts.
- `taxonomic_levels.R` Produce plot of percentage of rows per taxonomic
  level.
- `theme_graph.R` Main ggplot theme for the maps of the reports.
- `theme_map.R` Main ggplot theme for the plots of the reports.

## 3. Reproducibility parameters

    ─ Session info ───────────────────────────────────────────────────────────────
     setting  value
     version  R version 4.3.1 (2023-06-16 ucrt)
     os       Windows 10 x64 (build 18363)
     system   x86_64, mingw32
     ui       RTerm
     language (EN)
     collate  French_France.utf8
     ctype    French_France.utf8
     tz       Europe/Paris
     date     2023-10-27
     pandoc   3.1.1 @ C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools/ (via rmarkdown)

    ─ Packages ───────────────────────────────────────────────────────────────────
     package       * version    date (UTC) lib source
     askpass         1.2.0      2023-09-03 [1] CRAN (R 4.3.1)
     backports       1.4.1      2021-12-13 [1] CRAN (R 4.3.0)
     base64enc       0.1-3      2015-07-28 [1] CRAN (R 4.3.0)
     bit             4.0.5      2022-11-15 [1] CRAN (R 4.3.0)
     bit64           4.0.5      2020-08-30 [1] CRAN (R 4.3.0)
     blob            1.2.4      2023-03-17 [1] CRAN (R 4.3.0)
     broom           1.0.5      2023-06-09 [1] CRAN (R 4.3.0)
     bslib           0.5.1      2023-08-11 [1] CRAN (R 4.3.1)
     cachem          1.0.8      2023-05-01 [1] CRAN (R 4.3.0)
     callr           3.7.3      2022-11-02 [1] CRAN (R 4.3.0)
     cellranger      1.1.0      2016-07-27 [1] CRAN (R 4.3.0)
     class           7.3-22     2023-05-03 [1] CRAN (R 4.3.1)
     classInt        0.4-10     2023-09-05 [1] CRAN (R 4.3.1)
     cli             3.6.1      2023-03-23 [1] CRAN (R 4.3.0)
     clipr           0.8.0      2022-02-22 [1] CRAN (R 4.3.0)
     clock           0.7.0      2023-05-15 [1] CRAN (R 4.3.1)
     codetools       0.2-19     2023-02-01 [1] CRAN (R 4.3.1)
     colorspace      2.1-0      2023-01-23 [1] CRAN (R 4.3.0)
     conflicted      1.2.0      2023-02-01 [1] CRAN (R 4.3.0)
     cpp11           0.4.6      2023-08-10 [1] CRAN (R 4.3.1)
     crayon          1.5.2      2022-09-29 [1] CRAN (R 4.3.0)
     curl            5.1.0      2023-10-02 [1] CRAN (R 4.3.1)
     data.table      1.14.8     2023-02-17 [1] CRAN (R 4.3.0)
     DBI             1.1.3      2022-06-18 [1] CRAN (R 4.3.0)
     dbplyr          2.3.4      2023-09-26 [1] CRAN (R 4.3.1)
     diagram         1.6.5      2020-09-30 [1] CRAN (R 4.3.0)
     dials           1.2.0      2023-04-03 [1] CRAN (R 4.3.1)
     DiceDesign      1.9        2021-02-13 [1] CRAN (R 4.3.1)
     digest          0.6.33     2023-07-07 [1] CRAN (R 4.3.1)
     dplyr           1.1.3      2023-09-03 [1] CRAN (R 4.3.1)
     dtplyr          1.3.1      2023-03-22 [1] CRAN (R 4.3.0)
     e1071           1.7-13     2023-02-01 [1] CRAN (R 4.3.1)
     ellipsis        0.3.2      2021-04-29 [1] CRAN (R 4.3.0)
     evaluate        0.22       2023-09-29 [1] CRAN (R 4.3.1)
     fansi           1.0.4      2023-01-22 [1] CRAN (R 4.3.0)
     farver          2.1.1      2022-07-06 [1] CRAN (R 4.3.0)
     fastmap         1.1.1      2023-02-24 [1] CRAN (R 4.3.0)
     fontawesome     0.5.2      2023-08-19 [1] CRAN (R 4.3.1)
     forcats         1.0.0      2023-01-29 [1] CRAN (R 4.3.0)
     foreach         1.5.2      2022-02-02 [1] CRAN (R 4.3.1)
     fs              1.6.3      2023-07-20 [1] CRAN (R 4.3.1)
     furrr           0.3.1      2022-08-15 [1] CRAN (R 4.3.1)
     future          1.33.0     2023-07-01 [1] CRAN (R 4.3.1)
     future.apply    1.11.0     2023-05-21 [1] CRAN (R 4.3.1)
     gargle          1.5.2      2023-07-20 [1] CRAN (R 4.3.1)
     generics        0.1.3      2022-07-05 [1] CRAN (R 4.3.0)
     ggplot2         3.4.4      2023-10-12 [1] CRAN (R 4.3.1)
     globals         0.16.2     2022-11-21 [1] CRAN (R 4.3.0)
     glue            1.6.2      2022-02-24 [1] CRAN (R 4.3.0)
     googledrive     2.1.1      2023-06-11 [1] CRAN (R 4.3.0)
     googlesheets4   1.1.1      2023-06-11 [1] CRAN (R 4.3.0)
     gower           1.0.1      2022-12-22 [1] CRAN (R 4.3.0)
     GPfit           1.0-8      2019-02-08 [1] CRAN (R 4.3.1)
     gtable          0.3.4      2023-08-21 [1] CRAN (R 4.3.1)
     hardhat         1.3.0      2023-03-30 [1] CRAN (R 4.3.1)
     haven           2.5.3      2023-06-30 [1] CRAN (R 4.3.1)
     highr           0.10       2022-12-22 [1] CRAN (R 4.3.0)
     hms             1.1.3      2023-03-21 [1] CRAN (R 4.3.0)
     htmltools       0.5.6.1    2023-10-06 [1] CRAN (R 4.3.1)
     httr            1.4.7      2023-08-15 [1] CRAN (R 4.3.1)
     ids             1.0.1      2017-05-31 [1] CRAN (R 4.3.0)
     infer           1.0.5      2023-09-06 [1] CRAN (R 4.3.1)
     ipred           0.9-14     2023-03-09 [1] CRAN (R 4.3.1)
     isoband         0.2.7      2022-12-20 [1] CRAN (R 4.3.0)
     iterators       1.0.14     2022-02-05 [1] CRAN (R 4.3.1)
     jquerylib       0.1.4      2021-04-26 [1] CRAN (R 4.3.0)
     jsonlite        1.8.7      2023-06-29 [1] CRAN (R 4.3.1)
     KernSmooth      2.23-22    2023-07-10 [1] CRAN (R 4.3.1)
     knitr           1.44       2023-09-11 [1] CRAN (R 4.3.1)
     labeling        0.4.3      2023-08-29 [1] CRAN (R 4.3.1)
     lattice         0.21-9     2023-10-01 [1] CRAN (R 4.3.1)
     lava            1.7.2.1    2023-02-27 [1] CRAN (R 4.3.1)
     lhs             1.1.6      2022-12-17 [1] CRAN (R 4.3.1)
     lifecycle       1.0.3      2022-10-07 [1] CRAN (R 4.3.0)
     listenv         0.9.0      2022-12-16 [1] CRAN (R 4.3.1)
     lubridate       1.9.3      2023-09-27 [1] CRAN (R 4.3.1)
     magrittr        2.0.3      2022-03-30 [1] CRAN (R 4.3.0)
     MASS            7.3-60     2023-05-04 [1] CRAN (R 4.3.1)
     Matrix          1.6-1.1    2023-09-18 [1] CRAN (R 4.3.1)
     memoise         2.0.1      2021-11-26 [1] CRAN (R 4.3.0)
     mgcv            1.9-0      2023-07-11 [1] CRAN (R 4.3.1)
     mime            0.12       2021-09-28 [1] CRAN (R 4.3.0)
     modeldata       1.2.0      2023-08-09 [1] CRAN (R 4.3.1)
     modelenv        0.1.1      2023-03-08 [1] CRAN (R 4.3.1)
     modelr          0.1.11     2023-03-22 [1] CRAN (R 4.3.0)
     munsell         0.5.0      2018-06-12 [1] CRAN (R 4.3.0)
     nlme            3.1-163    2023-08-09 [1] CRAN (R 4.3.1)
     nnet            7.3-19     2023-05-03 [1] CRAN (R 4.3.1)
     numDeriv        2016.8-1.1 2019-06-06 [1] CRAN (R 4.3.0)
     openssl         2.1.1      2023-09-25 [1] CRAN (R 4.3.1)
     parallelly      1.36.0     2023-05-26 [1] CRAN (R 4.3.0)
     parsnip         1.1.1      2023-08-17 [1] CRAN (R 4.3.1)
     patchwork       1.1.3      2023-08-14 [1] CRAN (R 4.3.1)
     pillar          1.9.0      2023-03-22 [1] CRAN (R 4.3.0)
     pkgconfig       2.0.3      2019-09-22 [1] CRAN (R 4.3.0)
     prettyunits     1.2.0      2023-09-24 [1] CRAN (R 4.3.1)
     processx        3.8.2      2023-06-30 [1] CRAN (R 4.3.1)
     prodlim         2023.08.28 2023-08-28 [1] CRAN (R 4.3.1)
     progress        1.2.2      2019-05-16 [1] CRAN (R 4.3.0)
     progressr       0.14.0     2023-08-10 [1] CRAN (R 4.3.1)
     proxy           0.4-27     2022-06-09 [1] CRAN (R 4.3.1)
     ps              1.7.5      2023-04-18 [1] CRAN (R 4.3.0)
     purrr           1.0.2      2023-08-10 [1] CRAN (R 4.3.1)
     R6              2.5.1      2021-08-19 [1] CRAN (R 4.3.0)
     ragg            1.2.6      2023-10-10 [1] CRAN (R 4.3.1)
     rappdirs        0.3.3      2021-01-31 [1] CRAN (R 4.3.0)
     RColorBrewer    1.1-3      2022-04-03 [1] CRAN (R 4.3.0)
     Rcpp            1.0.11     2023-07-06 [1] CRAN (R 4.3.1)
     readr           2.1.4      2023-02-10 [1] CRAN (R 4.3.0)
     readxl          1.4.3      2023-07-06 [1] CRAN (R 4.3.1)
     recipes         1.0.8      2023-08-25 [1] CRAN (R 4.3.1)
     rematch         2.0.0      2023-08-30 [1] CRAN (R 4.3.1)
     rematch2        2.1.2      2020-05-01 [1] CRAN (R 4.3.0)
     reprex          2.0.2      2022-08-17 [1] CRAN (R 4.3.0)
     rlang           1.1.1      2023-04-28 [1] CRAN (R 4.3.0)
     rmarkdown       2.25       2023-09-18 [1] CRAN (R 4.3.1)
     rpart           4.1.21     2023-10-09 [1] CRAN (R 4.3.1)
     rsample         1.2.0      2023-08-23 [1] CRAN (R 4.3.1)
     rstudioapi      0.15.0     2023-07-07 [1] CRAN (R 4.3.1)
     rvest           1.0.3      2022-08-19 [1] CRAN (R 4.3.0)
     s2              1.1.4      2023-05-17 [1] CRAN (R 4.3.1)
     sass            0.4.7      2023-07-15 [1] CRAN (R 4.3.1)
     scales          1.2.1      2022-08-20 [1] CRAN (R 4.3.0)
     selectr         0.4-2      2019-11-20 [1] CRAN (R 4.3.0)
     sf              1.0-14     2023-07-11 [1] CRAN (R 4.3.1)
     shape           1.4.6      2021-05-19 [1] CRAN (R 4.3.0)
     slider          0.3.1      2023-10-12 [1] CRAN (R 4.3.1)
     SQUAREM         2021.1     2021-01-13 [1] CRAN (R 4.3.0)
     stringi         1.7.12     2023-01-11 [1] CRAN (R 4.3.0)
     stringr         1.5.0      2022-12-02 [1] CRAN (R 4.3.0)
     survival        3.5-7      2023-08-14 [1] CRAN (R 4.3.1)
     sys             3.4.2      2023-05-23 [1] CRAN (R 4.3.0)
     systemfonts     1.0.5      2023-10-09 [1] CRAN (R 4.3.1)
     terra           1.7-55     2023-10-13 [1] CRAN (R 4.3.1)
     textshaping     0.3.7      2023-10-09 [1] CRAN (R 4.3.1)
     tibble          3.2.1      2023-03-20 [1] CRAN (R 4.3.0)
     tidymodels      1.1.1      2023-08-24 [1] CRAN (R 4.3.1)
     tidyr           1.3.0      2023-01-24 [1] CRAN (R 4.3.0)
     tidyselect      1.2.0      2022-10-10 [1] CRAN (R 4.3.0)
     tidyverse       2.0.0      2023-02-22 [1] CRAN (R 4.3.0)
     timechange      0.2.0      2023-01-11 [1] CRAN (R 4.3.0)
     timeDate        4022.108   2023-01-07 [1] CRAN (R 4.3.0)
     tinytex         0.48       2023-10-13 [1] CRAN (R 4.3.1)
     tune            1.1.2      2023-08-23 [1] CRAN (R 4.3.1)
     tzdb            0.4.0      2023-05-12 [1] CRAN (R 4.3.0)
     units           0.8-4      2023-09-13 [1] CRAN (R 4.3.1)
     utf8            1.2.3      2023-01-31 [1] CRAN (R 4.3.0)
     uuid            1.1-1      2023-08-17 [1] CRAN (R 4.3.1)
     vctrs           0.6.4      2023-10-12 [1] CRAN (R 4.3.1)
     viridisLite     0.4.2      2023-05-02 [1] CRAN (R 4.3.0)
     vroom           1.6.4      2023-10-02 [1] CRAN (R 4.3.1)
     warp            0.2.0      2020-10-21 [1] CRAN (R 4.3.1)
     withr           2.5.1      2023-09-26 [1] CRAN (R 4.3.1)
     wk              0.8.0      2023-08-25 [1] CRAN (R 4.3.1)
     workflows       1.1.3      2023-02-22 [1] CRAN (R 4.3.1)
     workflowsets    1.0.1      2023-04-06 [1] CRAN (R 4.3.1)
     xfun            0.40       2023-08-09 [1] CRAN (R 4.3.1)
     xml2            1.3.5      2023-07-06 [1] CRAN (R 4.3.1)
     yaml            2.3.7      2023-01-23 [1] CRAN (R 4.3.0)
     yardstick       1.2.0      2023-04-21 [1] CRAN (R 4.3.1)

     [1] C:/Users/jwicquart/AppData/Local/Programs/R/R-4.3.1/library

    ──────────────────────────────────────────────────────────────────────────────
