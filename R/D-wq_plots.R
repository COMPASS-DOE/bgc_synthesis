## ---------------------------
##
## Script name: 
##
## Purpose of script:
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
  source("Functions/wq_plots_functions.R")
  
  #Load in necessary functions to train 
  source("Functions/train_Rforest_functions.R")
  
  #Variables treated as constants
  source("Constants/initial_model_constants.R")
  
  #indexes of wq 
  wq_ind <- seq(2,11,3)

# 2. Read location data ---------------------------------------------------

  #remove outliers from cbv
  cbv_all <- read_station("./data_NERR/output/cbv_for_models.csv") %>% 
    filter(is.na(no3) | no3 < 1, 
           is.na(po4) | po4 < 0.15, 
           is.na(chla) | chla < 200)
  
  #remove outliers from owc
  owc_all <- read_station("./data_NERR/output/owc_for_models.csv") %>% 
    filter(is.na(no3) | no3 < 8, 
           is.na(po4) | po4 < 0.1)


# 3. Load in trained models -----------------------------------------------

  #load models
  load("Model/randomForestOWC.RData")
  load("Model/randomForestCBV.RData")
  load("Model/rangerOWC.RData")
  load("Model/rangerCBV.RData")
  load("Model/referenceTable.RData")


# 4. Correlation plots ---------------------------------------------------------

  #Create comparison chart between cbv and owc
  par(mfrow=c(1, 2))
  
  #define columns
  pred <- c(wq_predictors, "nh4", "no3", "po4", "chla")
  
  createCorrComps(cbv_all, owc_all, pred)


# 5. Table of training metrics --------------------------------------------

  #Make a chart by chemical signature, predictors, RMSE, MAE and NSE
  sumTable <- data.frame()
  
  for(i in 1:nrow(reference_table)){
    sumTable[i,c(1:14)] <- tibble(reference_table[i,c(1,2)]) %>% 
      c(result_cbv_ranger[[i]]$metrics[c(2, 4, 9),])  %>%
      c(result_owc_ranger[[i]]$metrics[c(2, 4, 9),]) %>% 
      c(result_cbv_rf[[i]]$metrics[c(2, 4, 9),])  %>%
      c(result_owc_rf[[i]]$metrics[c(2, 4, 9),]) %>% 
      data.frame() %>% 
      mutate(predictor = strsplit(predictor, "_")[[1]][1])
  }
  
  #subset just water quality indexes
  wqTable <- sumTable[wq_ind,]
  colnames(wqTable) <- c("Signal", "Predictor", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE")
  
  kable(wqTable) %>%  
    remove_column(1) %>% 
    add_header_above(c(" " = 2, "CBV" = 3, "OWC" = 3, "CBV" = 3, "OWC" = 3)) %>% 
    add_header_above(c(" " = 2, "ranger" = 6, "randomForest" = 6)) %>% 
    kable_classic() %>% 
    column_spec(c(3:5, 9:11), 
                background = "lightgrey")


# Actual v Predicted Plots ------------------------------------------------



cbv_avp <- makeAVPPlots(result_cbv_rf)
owc_avp <- makeAVPPlots(result_owc_rf)

ggarrange(plotlist=c(cbv_avp, owc_avp), ncol = 4, nrow=2)


# Feature Importance Plots ------------------------------------------------

cbv_fi <- makeFIPlots(result_cbv_rf)
owc_fi <- makeFIPlots(result_owc_rf)
ggarrange(plotlist=c(cbv_fi, owc_fi), ncol=4, nrow=2, labels = c("cbv", rep("",3), "owc"))


# Partial Dependency Plots ---------------------------------------------

  #Partial Dependency Information with DALEX package
  pdp_cbvnh4 <- sapply(wq_predictors, function(x) model_profile(result_cbv_rf[[2]]$finalModelDT, 
                                                                N = NULL, 
                                                                variables = x))
  
  pdp_owcnh4 <- sapply(wq_predictors, function(x) model_profile(result_owc_rf[[2]]$finalModelDT, 
                                                                N = NULL, 
                                                                variables = x))
  
  pdp_cbvpo4 <- sapply(wq_predictors, function(x) model_profile(result_cbv_rf[[5]]$finalModelDT, 
                                                                N = NULL, 
                                                                variables = x))
  
  pdp_owcpo4 <- sapply(wq_predictors, function(x) model_profile(result_owc_rf[[5]]$finalModelDT, 
                                                                N = NULL, 
                                                                variables = x))
  
  pdp_cbvno3 <- sapply(wq_predictors, function(x) model_profile(result_cbv_rf[[8]]$finalModelDT, 
                                                                N = NULL, 
                                                                variables = x))
  
  pdp_owcno3 <- sapply(wq_predictors, function(x) model_profile(result_owc_rf[[8]]$finalModelDT, 
                                                                N = NULL, 
                                                                variables = x))
  pdp_cbvchla <- sapply(wq_predictors, function(x) model_profile(result_cbv_rf[[11]]$finalModelDT, 
                                                                 N = NULL, 
                                                                 variables = x))
  pdp_owcchla <- sapply(wq_predictors, function(x) model_profile(result_owc_rf[[11]]$finalModelDT, 
                                                                 N = NULL, 
                                                                 variables = x))
  
  
  #Extract plots
  plotPDPcbvnh4 <- formPDP(pdp_cbvnh4, "nh4")
  plotPDPowcnh4 <- formPDP(pdp_owcnh4, "nh4")
  plotPDPcbvpo4 <- formPDP(pdp_cbvpo4, "po4")
  plotPDPowcpo4 <- formPDP(pdp_owcpo4, "po4")
  plotPDPcbvno3 <- formPDP(pdp_cbvno3, "no3")
  plotPDPowcno3 <- formPDP(pdp_owcno3, "no3")
  plotPDPcbvchla <- formPDP(pdp_cbvchla, "chla")
  plotPDPowcchla <- formPDP(pdp_owcchla, "chla")
  ggarrange(plotPDPcbvnh4, 
            plotPDPowcnh4, 
            plotPDPcbvpo4, 
            plotPDPowcpo4, 
            plotPDPcbvno3, 
            plotPDPowcno3, 
            plotPDPcbvchla, 
            plotPDPowcchla, 
            ncol = 2, 
            nrow = 4)



