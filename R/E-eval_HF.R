## ---------------------------
##
## Script name: eval_HF
##
## Purpose of script: predict nutrient data on high frequency data
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-07-12
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


# 1. Setup ----------------------------------------------------------------

# Clean workspace
rm(list = ls())

# Load packages
require(pacman)
p_load(tidyverse, 
       corrplot, 
       ggplot2, 
       gridExtra, 
       cowplot, 
       lubridate, 
       tidymodels,
       splitTools, 
       ggthemes, 
       parallel, 
       ggpubr, 
       hydroGOF, 
       pdp, 
       DALEXtra, 
       kableExtra)

#Load in necessary functions to plot
source("Functions/eval_HF_functions.R")

#Variables treated as constants
source("Constants/initial_model_constants.R")

load("Model/randomForestOWC.RData")
load("Model/randomForestCBV.RData")

#1. Calculate CBV predictions --------------------------------------------------------

  #1. Read in data
  hf_cbv_data_prep <- preProcessData("data_NERR/output/cbv_hf_wq.csv", wq_predictors)
  #2. Filter out correct predictors
  hf_cbv_data_all <- prepData(hf_cbv_data_prep, wq_predictors)
  #3. Match predictions to no NA dataframe
  hf_data_cbv <- matchPredictions(result_cbv_rf, hf_cbv_data_all)

#2. Calculate OWC predictions ----------------------------------------------------

  #1. Read in data
  hf_owc_data_prep <- preProcessData("data_NERR/output/owc_hf_wq.csv", wq_predictors)
  #2. Filter out correct predictors
  hf_owc_data_all <- prepData(hf_owc_data_prep, wq_predictors)
  #3. Match predictions to no NA dataframe
  hf_data_owc <- matchPredictions(result_owc_rf, hf_owc_data_all)

#3. Save HF prediction data ------------------------------------------------------------
  
  write_csv(hf_data_cbv, "data_NERR/output/cbv_hf_wq_predictions.csv")
  write_csv(hf_data_owc, "data_NERR/output/owc_hf_wq_predictions.csv")
  
  

