# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(gbm)

# 2. Source functions ----

source("code/function/graphical_par.R")
source("code/function/theme_map.R")
source("code/function/theme_graph.R")

# 3. Load gcrmndb_benthos data ----

load("data/04_data-benthic.RData")

# 4. Extract HCC and basic dataviz ----

data_hcc <- data_benthic %>% 
  filter(category == "Hard coral")

hist(data_hcc$measurementValue/100)

# 5. GBM ----

# 5.1 Test with Tiahura (for comparison with Lamy et al, 2016) --

data_hcc_gbm <- data_hcc %>% 
  filter(datasetID == "0006") %>% 
  select(measurementValue, year, day, month)

gbm_res <- gbm(formula = measurementValue ~ .,
               data = data_hcc_gbm,
               distribution = "gaussian",
               n.trees = 10000)

plot(gbm_res, i.var = "year")

# 5.2 Test with all data --

data_hcc_gbm <- data_hcc %>% 
  select(measurementValue, territory, datasetID, decimalLatitude, decimalLongitude, year, day, month) %>% 
  mutate_at(c("datasetID", "territory"), ~as.factor(.))

gbm_res <- gbm(formula = measurementValue ~ .,
               data = data_hcc_gbm,
               distribution = "gaussian",
               n.trees = 10000)

plot(gbm_res, i.var = "year")
