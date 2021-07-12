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
  
  #Variables treated as constants
  source("Constants/initial_model_constants.R")
  
  #HF predictions
  hf_data_cbv <- read_csv("data_NERR/output/cbv_hf_wq_predictions.csv")
  hf_data_owc <- read_csv("data_NERR/output/owc_hf_wq_predictions.csv")


#2. Scatterplots of Predictions ---------------------------------------------

  #CBV Add site information
  hf_data_cbv_site <- hf_cbv_data_prep %>% 
    mutate(site = as.factor(site)) %>% 
    select(site) %>% 
    cbind(hf_data_cbv)
  
  #CBV Create individual nutrient plots
  cbv_pred_plot_nh4 <- ggplot(hf_data_cbv_site, aes(x=datetime_round, y = nh4))+
    geom_point()
  cbv_pred_plot_no3 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = no3))+
    geom_point()
  cbv_pred_plot_po4 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = po4))+
    geom_point()
  cbv_pred_plot_chla <- ggplot(hf_data_cbv, aes(x=datetime_round, y = chla))+
    geom_point()
  
  #OWC Add site information
  hf_data_owc_site <- hf_owc_data_prep %>% 
    mutate(site = as.factor(site)) %>% 
    select(site) %>% 
    cbind(hf_data_owc)
  
  
  #OWC Create individual nutrient plots
  owc_pred_plot_nh4 <- ggplot(hf_data_owc, aes(x=datetime_round, y = nh4))+
    geom_point()
  owc_pred_plot_no3<- ggplot(hf_data_owc, aes(x=datetime_round, y = no3))+
    geom_point()
  owc_pred_plot_po4 <- ggplot(hf_data_owc, aes(x=datetime_round, y = po4))+
    geom_point()
  owc_pred_plot_chla <- ggplot(hf_data_owc, aes(x=datetime_round, y = chla))+
    geom_point()


#3. Grouped CBV and OWC predictions ------------------------------------------------

  ggarrange(ggarrange(
    cbv_pred_plot_nh4, 
    owc_pred_plot_nh4,
    ncol = 1),
    ggarrange(
      cbv_pred_plot_no3, 
      owc_pred_plot_no3,
      ncol = 1),
    ggarrange(
      cbv_pred_plot_po4, 
      owc_pred_plot_po4,
      ncol = 1),
    ggarrange(
      cbv_pred_plot_chla, 
      owc_pred_plot_chla,
      ncol = 1),
    ncol = 1)
  





