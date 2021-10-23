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
         kableExtra, 
         grid)
  
  #Load in necessary functions to plot
  source("Functions/wq_plots_functions.R")
  
  #Load in necessary functions to train 
  source("Functions/all_plots_functions.R")
  
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

# Feature Importance Plots --------------------------------------------------
  
  #find importance for each random forest model in reference table
  importance_cbv_rf <- grabAllImportance(result_cbv_rf, "randomForest")
  importance_owc_rf <- grabAllImportance(result_owc_rf, "randomForest")
  
  #group importance by predictors (1- met, 2 - wq, 3 - all)
  group_importance_cbv_rf <- clusterChartModel(importance_cbv_rf, reference_table)
  group_importance_owc_rf <- clusterChartModel(importance_owc_rf, reference_table)
  
  #Create importance plots of predictors for owc and cbv of the best architecture:
  #randomForest
  ggarrange(createSiteImportancePlots(group_importance_cbv_rf, "cbv", 2),
            createSiteImportancePlots(group_importance_owc_rf, "owc", 2), nrow = 2, ncol=1)
  
  
# Feature Importance Data -------------------------------------------------

  #Collect data
  cbv_fi_data <- getFIData(result_cbv_rf)
  owc_fi_data <- getFIData(result_owc_rf)
  
  #Create Table
  table <- cbind(cbv_fi_data, owc_fi_data)
  colnames(table) <- 1:8
  rownames(table) <- c("Water Temperature", 
                       "Specific Conductivity", 
                       "Dissolved Oxygen", 
                       "Depth", 
                       "pH", 
                       "Turbidity", 
                       "Discharge", 
                       "Time of Day")
  sums <- apply(table, 1, sum) 
  table <- table %>% as.data.frame() %>% relocate(1, 5, 2, 6, 3, 7, 4, 8) 
  kable(table, col.names = NULL) %>% 
    kable_classic() %>% 
    add_header_above(c(" " = 1, "Ammonia" = 2, "Phosphate" = 2, "Nitrate" = 2, "Chlorophyll-a" = 2)) %>% 
    column_spec(seq(2,9,2), 
                background = "lightgrey")
  

  diffTable <- matrix(nrow =8, ncol=4)
  j<-1
  for(i in seq(1,7,2)){
    diffTable[,j] <- table[,i]-table[,i+1]
    j<-j+1
  }
  rownames(diffTable) <- c("Water Temperature", 
                       "Specific Conductivity", 
                       "Dissolved Oxygen", 
                       "Depth", 
                       "pH", 
                       "Turbidity", 
                       "Discharge", 
                       "Time of Day")
  diffTable
  max(abs(diffTable))
  
  diffTableBind <- rbind(cbind(-diffTable[,1], "ammonia"),
                         cbind(-diffTable[,2],"phosphate"),
                         cbind(-diffTable[,3], "nitrate"),
                         cbind(-diffTable[,4], "chlorophyll a")) %>% 
                  cbind(rownames(.)) %>% as.data.frame()
                         
  
  colnames(diffTableBind) <- c("value", "nutrient", "predictor")
  
  diffTableBind <- diffTableBind %>% 
                    mutate(value = as.numeric(value))
  diffPlots <-list()
  j <- 1
  colorsDiff <- c("#56B4E9", "#F0E442", "#D55E00", "#CC79A7" )
  for(val in unique(diffTableBind$nutrient)){
    diffPlots[[j]] <- ggplot(diffTableBind[diffTableBind$nutrient == val,], 
                             aes(x=value, y = predictor, fill = unique(diffTableBind$predictor)))+
                    geom_bar(stat="identity", position = "dodge") +
                    theme_classic()+
                    xlim(-0.3, 0.3)+
                    xlab("% MSE")+
                    theme(axis.title.y=element_blank(),
                          axis.text.y=element_blank(),
                          axis.ticks.y=element_blank(),
                          axis.line.y = element_blank())
    j<-j+1
  }
  
  ggarrange(plotlist = diffPlots[c(1,3,2,4)], nrow =1)
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



