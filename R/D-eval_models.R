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
  source("Functions/eval_models_functions.R")
  
  #Load in necessary functions to train 
  source("Functions/train_Rforest_functions.R")
  
  #Variables treated as constants
  source("Constants/initial_model_constants.R")

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

  #load plots
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
  

# 5. Feature Importance Plots --------------------------------------------------
  
  #plot importance
  importance_cbv_rf <- grabAllImportance(result_cbv_rf, "randomForest")
  importance_owc_rf <- grabAllImportance(result_owc_rf, "randomForest")
  
  #group importance by predictors
  group_importance_cbv_rf <- clusterChartModel(importance_cbv_rf, reference_table)
  group_importance_owc_rf <- clusterChartModel(importance_owc_rf, reference_table)
  
  #Create importance plots of predictors for owc and cbv of the best architecture:
  #randomForest
  ggarrange(createSiteImportancePlots(group_importance_cbv_rf, "cbv", 2),
            createSiteImportancePlots(group_importance_owc_rf, "owc", 2), nrow = 2, ncol=1)
  



  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  