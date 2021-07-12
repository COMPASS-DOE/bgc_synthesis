## ---------------------------
##
## Script name: HF_events
##
## Purpose of script: subset HF data to event specified
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
  
  #Functions for HF events
  source("Functions/HF_events_functions.R")
  
  #Variables treated as constants
  source("Constants/initial_model_constants.R")
  
  #HF predictions
  hf_data_cbv <- read_csv("data_NERR/output/cbv_hf_wq_predictions.csv")
  hf_data_owc <- read_csv("data_NERR/output/owc_hf_wq_predictions.csv")


# Seiches -----------------------------------------------------------------

  #subset HF data to Nov 1st seiche event
  oct272019seiche <- hf_data_owc_site[hf_data_owc$datetime_round > "2019-10-30 20:00" & hf_data_owc$datetime_round < "2019-11-2 3:00", ]
  
  createPlots(oct272019seiche, c(hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2019-11-1 0:00"))[1]]))

# Hurricanes --------------------------------------------------------------

  sandy <- hf_data_cbv_site[hf_data_cbv_site$datetime_round > "2012-10-24 22:00" & hf_data_cbv$datetime_round < "2012-10-31 4:00", ]
  
  createPlots(sandy, c(hf_data_cbv$datetime_round[which(hf_data_cbv$datetime_round == as.Date("2012-10-29 1:00"))[1]]))

