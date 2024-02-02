# 1. Load packages ----

library(tidyverse) # Core tidyverse packages
library(tidymodels) # Core tidymodels packages
library(sf)
library(DALEX)
library(DALEXtra)
library(caret)
library(xgboost)
library(vip)
library(pdp)
library(future)
library(furrr)

plan(multisession(workers = 4)) # Set parallelization with 4 cores

# 2. Load data ----

load("data/16_model-data/data_benthic_prepared.RData")
load("data/16_model-data/data_predictors_pred.RData")
