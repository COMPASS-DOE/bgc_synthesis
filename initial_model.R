# ---------------------------

# Purpose: Initial testing of models: what set of parameters works best?

# This script helps us decide what set of parameters is most useful for predicting
# nutrients/chla, and which package to use. This same workflow should be useful for 
# tuning models and other optimization. 

# Next steps: 
## integrate iRF into model selection loop
## select ranger v randomForest v iRF; tune mtry, ntrees, etc
## streamline / bulletproof the model creation workflow
## once those are done, bonus would be starting on 

# Outputs
## figure comparing different predictor inputs (see plot call below)
## figure comparing performance of randomForest, ranger, and iRF::randomForest

# I think we'll want to use iRF at some point regardless of how it compares to
# rF and ranger because it returns interactions. Ideal is formatting output of
# script as Rmd

# ---------------------------

# Clean workspace
rm(list = ls())

# Load packages
require(pacman)
p_load(tidyverse, ggplot2, gridExtra, cowplot, lubridate, tidymodels,splitTools, ggthemes, parallel, ggpubr, hydroGOF, pdp, DALEXtra, kableExtra)

#Load in necessary functions to train 
source("Functions/train_Rforest_functions.R")

#Load in necessary functions to plot
source("Functions/plot_Model_Functions.R")

#Variables treated as constants
source("Constants/initial_model_constants.R")

#######################################
#####Code
#######################################

#remove outliers from cbv
cbv_all <- read_station("./data_NERR/output/cbv_for_models.csv") %>% 
  filter(is.na(no3) | no3 < 1, 
         is.na(po4) | po4 < 0.15, 
         is.na(chla) | chla < 200)

#remove outliers from owc
owc_all <- read_station("./data_NERR/output/owc_for_models.csv") %>% 
  filter(is.na(no3) | no3 < 8, 
         is.na(po4) | po4 < 0.1)

#Create an apply function for parrallel computing
numCores <- detectCores()-1

#START cluster
cl <- makeCluster(numCores, outfile ='', setup = "sequential")

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

#Train data on cbv location with ranger
result_cbv_ranger <- parApply(cl,reference_table,1, 
                              function(x) choose_inputs(
                                cbv_all, 
                                x[1], 
                                eval(parse(text = x[2])), 
                                x[3],
                                modelType = "ranger",
                                importance = "impurity_corrected", 
                                prop = 8/10))
#Train data on owc location with ranger
result_owc_ranger <- parApply(cl,reference_table,1, 
                              function(x) choose_inputs(
                                owc_all, 
                                x[1], 
                                eval(parse(text = x[2])), 
                                x[3],
                                modelType = "ranger",
                                importance = "impurity_corrected", 
                                prop = 8/10))
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

#Correlations of initial predictors
par(mfrow=c(1, 2))
corrplot(result_cbv_rf[[1]]$correlations, method = "circle",title = "CBV", mar=c(0,0,4,0), tl.col = "black")
corrplot(result_owc_rf[[1]]$correlations, title = "OWC", mar=c(0,0,4,0), tl.col = "black")

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



#plot importance
importance_cbv_ranger <- grabAllImportance(result_cbv_ranger, "ranger")
importance_owc_ranger <- grabAllImportance(result_owc_ranger, "ranger")
importance_cbv_rf <- grabAllImportance(result_cbv_rf, "randomForest")
importance_owc_rf <- grabAllImportance(result_owc_rf, "randomForest")

#group importance by predictors
group_importance_cbv_ranger <- clusterChartModel(importance_cbv_ranger, reference_table)
group_importance_owc_ranger <- clusterChartModel(importance_owc_ranger, reference_table)
group_importance_cbv_rf <- clusterChartModel(importance_cbv_rf, reference_table)
group_importance_owc_rf <- clusterChartModel(importance_owc_rf, reference_table)


##########################
#create importance plots
##########################


#Create importance plots of predictors for owc and cbv of the best architecture:
#randomForest
ggarrange(createSiteImportancePlots(group_importance_cbv_rf, "cbv"),
          createSiteImportancePlots(group_importance_owc_rf, "owc"), nrow = 2, ncol=1)


#Predict on High Frequency Data


#construct plot with cbv 
hf_cbv_data_all <- preProcessData("data_NERR/output/cbv_hf_wq.csv", wq_predictors)
hf_data_cbv <- matchPredictions(result_cbv_rf, hf_cbv_data_all)
cbv_pred_plot_nh4 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = nh4))+geom_point()
cbv_pred_plot_no3 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = no3))+geom_point()
cbv_pred_plot_po4 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = po4))+geom_point()
cbv_pred_plot_chla <- ggplot(hf_data_cbv, aes(x=datetime_round, y = chla))+geom_point()



#construct plot with owc
hf_owc_data_all <- preProcessData("data_NERR/output/owc_hf_wq.csv", wq_predictors)
hf_data_owc <- matchPredictions(result_owc_rf, hf_owc_data_all)
owc_pred_plot_nh4 <- ggplot(hf_data_owc, aes(x=datetime_round, y = nh4))+geom_point()
owc_pred_plot_no3<- ggplot(hf_data_owc, aes(x=datetime_round, y = no3))+geom_point()
owc_pred_plot_po4 <- ggplot(hf_data_owc, aes(x=datetime_round, y = po4))+geom_point()
owc_pred_plot_chla <- ggplot(hf_data_owc, aes(x=datetime_round, y = chla))+geom_point()

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


write_csv(hf_data_cbv, "data_NERR/output/cbv_hf_wq_predictions.csv")
write_csv(hf_data_owc, "data_NERR/output/owc_hf_wq_predictions.csv")


#Partial Dependency plots

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
          nrow = 2)

#Observe Seiche & Hurricane Events
createPlots <- function(x, day){
  N132003nh4 <- ggplot(x, aes(x=datetime_round, y=nh4))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept = day, color = "red")
  
  N132003no3 <- ggplot(x, aes(x=datetime_round, y=no3))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept = day, color = "red")
  
  N132003po4 <- ggplot(x, aes(x=datetime_round, y=po4))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept = day, color = "red")
  
  N132003chla <- ggplot(x, aes(x=datetime_round, y=chla))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept = day, color = "red")
  
  N132003temp <- ggplot(x, aes(x=datetime_round, y=Temp.mean))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept = day, color = "red")
  
  N132003spCond <- ggplot(x, aes(x=datetime_round, y=SpCond.mean))+
    geom_point()+
    geom_smooth()+
    geom_vline(xintercept = day, color = "red")
  
  return(ggarrange(N132003nh4, N132003no3, N132003po4, N132003chla, N132003temp, N132003spCond, nrow = 1, ncol=6))
}


nov132003seiche <- hf_data_owc[hf_data_owc$datetime_round > "2003-11-6" & hf_data_owc$datetime_round < "2003-11-23", ]

oct272019seiche <- hf_data_owc[hf_data_owc$datetime_round > "2019-10-20" & hf_data_owc$datetime_round < "2019-11-8", ]

ggarrange(createPlots(nov132003seiche, c(hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2003-11-13"))[1]],
                                         hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2003-11-16"))[1]])), 
          createPlots(oct272019seiche, c(hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2019-10-27"))[1]],
                                         hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2019-11-1"))[1]])), 
          ncol = 1, nrow =2 )

isabelle <- hf_data_cbv[hf_data_cbv$datetime_round > "2003-9-10" & hf_data_cbv$datetime_round < "2003-9-24", ]

melissa <- hf_data_cbv[hf_data_cbv$datetime_round > "2019-10-4" & hf_data_cbv$datetime_round < "2019-10-18", ]

irene <- hf_data_cbv[hf_data_cbv$datetime_round > "2003-8-21" & hf_data_cbv$datetime_round < "2003-9-4", ]

ggarrange(createPlots(isabelle, c(hf_data_cbv$datetime_round[which(hf_data_cbv$datetime_round == as.Date("2003-9-17"))[1]])), 
          createPlots(melissa, c(hf_data_cbv$datetime_round[which(hf_data_cbv$datetime_round == as.Date("2019-10-11"))[1]])),
          createPlots(irene, c(hf_data_cbv$datetime_round[which(hf_data_cbv$datetime_round == as.Date("2003-8-28"))[1]])),
          ncol = 1, nrow =3 )

