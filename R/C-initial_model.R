
##
## Script name: prep_model_data.R
##
## Purpose: Initial testing of models: what set of parameters works best?
##
## Authors: Matt Duggan and Peter Regier
##
## Created: 2021-05-27
## Updated: 2021-11-30
##
## Contact: peter.regier@pnnl.gov
##
## Notes:
##   
## This script creates the models and associated information used in this 
## manuscript. Models are created using the 'randomForest' package, and we only
## use the water quality models, although other models are created for initial
## comparisons that helped us select a parsimonious set of predictors that balance
## model strength and simplicity (i.e., all parameters from a single instrument). 

## Outputs
## figure comparing different predictor inputs (see plot call below)
## figure comparing performance of randomForest, ranger, and iRF::randomForest


# 1. setup ----------------------------------------------------------------

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
  
  #Load in necessary functions to train 
  source("Functions/train_Rforest_functions.R")
  
  #Variables treated as constants
  source("Constants/initial_model_constants.R")


# 2. Read location data ---------------------------------------------------
  
  cbv_all <- read_csv("./data_NERR/output/cbv_for_models.csv") %>% 
    mutate(sin_doy = sin(yday(lubridate::date(datetime_round)) / (365.25 * pi))) %>%
    select(-datetime_round, -site) %>% 
    mutate(across(where(is.character), as.numeric)) %>% 
    #select_if(is.double) %>% 
    filter(is.na(no3) | no3 < 1, 
           is.na(po4) | po4 < 0.15, 
           is.na(chla) | chla < 200)
  
  owc_all <- read_csv("./data_NERR/output/owc_for_models.csv") %>% 
    mutate(sin_doy = sin(yday(lubridate::date(datetime_round)) / (365.25 * pi))) %>%
    select(-datetime_round, -site) %>% 
    mutate(across(where(is.character), as.numeric)) %>% 
    #select_if(is.double) %>% 
    filter(is.na(no3) | no3 < 1, 
           is.na(po4) | po4 < 0.15, 
           is.na(chla) | chla < 200)

# 3. Reference table ------------------------------------------------------

  #Place to save models to be trained 
  reference_table <- tibble(dep = character(), predictor = character()) %>% 
                      add_row(dep = rep("nh4",3), predictor = c("met_predictors", 
                                                                "wq_predictors", 
                                                                "all_predictors")) %>% 
                      add_row(dep = rep("po4",3), predictor = c("met_predictors", 
                                                                "wq_predictors", 
                                                                "all_predictors")) %>% 
                      add_row(dep = rep("no3",3), predictor = c("met_predictors", 
                                                                "wq_predictors", 
                                                                "all_predictors")) %>% 
                      add_row(dep = rep("chla",3), predictor = c("met_predictors", 
                                                                 "wq_predictors", 
                                                                 "all_predictors")) %>% 
                      add_column(paste0(.$dep, "-", .$predictor)) %>% 
                        rename(name = 3)
  
  save(reference_table, file = "Model/referenceTable.RData")  

# 4. Parallel processing setup -------------------------------------------


  #Create an apply function for parrallel computing
  numCores <- detectCores()-1
  
  #START cluster (note: 'setup_strategy' used as 'setup' didn't work on Mac)
  cl <- makeCluster(numCores, outfile ='', setup_strategy = "sequential")
  
  #export required constants
  clusterExport(cl, 
                c("cbv_all", 
                  "owc_all"))
  
  #Export necessary libraries
  clusterEvalQ(cl, {
    library(ggplot2)
    library(tidyverse)
    library(tidymodels)
    library(tidyverse)
    library(lubridate)
    library(ggthemes)
    library(hydroGOF)
    library(DALEXtra)
    source("Functions/train_Rforest_functions.R")
    source("Constants/initial_model_constants.R")
  })


# 5. Train RF models ------------------------------------------------------

  #Train data on cbv location with random forest
  result_cbv_rf <- parApply(cl,reference_table,1,
                            function(x) choose_inputs(
                              cbv_all,
                              x[1],
                              eval(parse(text = x[2])),
                              x[3],
                              modelType = "randomForest",
                              importance = TRUE,
                              prop = 8/10))

  #Train data on owc location with random forest
  result_owc_rf <- parApply(cl,reference_table,1,
                            function(x) choose_inputs(
                              owc_all,
                              x[1],
                              eval(parse(text = x[2])),
                              x[3],
                              modelType = "randomForest",
                              importance = TRUE,
                              prop = 8/10))

  #END parrallel processing
  stopCluster(cl)


# 6. Save models ----------------------------------------------------------

  #Save models
  save(result_owc_rf, file = "Model/randomForestOWC.RData")
  save(result_cbv_rf, file = "Model/randomForestCBV.RData")

toc()

# 7. Table of training metrics --------------------------------------------

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
  
  colnames(sumTable) <- c("Signal", "Predictor", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE")
  kable(sumTable) %>%
    add_header_above(c(" " = 2, "CBV" = 3, "OWC" = 3, "CBV" = 3, "OWC" = 3)) %>% 
    add_header_above(c(" " = 2, "ranger" = 6, "randomForest" = 6)) %>% 
    kable_classic() %>% 
    column_spec(c(3:5, 9:11), 
                background = "lightgrey")

