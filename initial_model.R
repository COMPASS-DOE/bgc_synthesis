# ---------------------------

# : Initial testing of models: what set of parameters works best?

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
p_load(tidyverse, corrplot, ggplot2, gridExtra, cowplot, lubridate, tidymodels,splitTools, ggthemes, parallel, ggpubr, hydroGOF, pdp, DALEXtra, kableExtra)

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

save(reference_table, file = "Model/referenceTable.RData")

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

#Save models
save(result_owc_rf, file = "Model/randomForestOWC.RData")
save(result_cbv_rf, file = "Model/randomForestCBV.RData")
save(result_owc_ranger, file = "Model/rangerOWC.RData")
save(result_cbv_ranger, file = "Model/rangerCBV.RData")

#load plots
load("Model/randomForestOWC.RData")
load("Model/randomForestCBV.RData")
load("Model/rangerOWC.RData")
load("Model/rangerCBV.RData")
load("Model/referenceTable.RData")

#Correlations of initial predictors
par(mfrow=c(1, 1))

corr_cbv <- cbv_all %>% 
            select(wq_predictors, nh4, no3, po4, chla) %>% 
            na.omit() %>% 
            cor() %>% 
            as.matrix()

corr_owc <- owc_all %>% 
  select(wq_predictors, nh4, no3, po4, chla) %>% 
  na.omit() %>% 
  cor() %>% 
  as.matrix()

cor.mtest <- function(mat) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j])
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(corr_cbv)

corrplot(corr_cbv,
         type = "upper",
         method = "circle",
         tl.col = "black", 
         sig.level = 0.05, 
         insig = "blank", 
         p.mat = p.mat, 
         tl.srt=45)

p.mat <- cor.mtest(corr_owc)

corrplot(corr_owc,
         type = "upper",
         method = "circle",
         tl.col = "black", 
         sig.level = 0.05, 
         insig = "blank", 
         p.mat = p.mat, 
         tl.srt=45)



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
importance_cbv_rf <- grabAllImportance(result_cbv_rf, "randomForest")
importance_owc_rf <- grabAllImportance(result_owc_rf, "randomForest")

#group importance by predictors
group_importance_cbv_rf <- clusterChartModel(importance_cbv_rf, reference_table)
group_importance_owc_rf <- clusterChartModel(importance_owc_rf, reference_table)


##########################
#create importance plots
##########################


#Create importance plots of predictors for owc and cbv of the best architecture:
#randomForest
ggarrange(createSiteImportancePlots(group_importance_cbv_rf, "cbv", 2),
          createSiteImportancePlots(group_importance_owc_rf, "owc", 2), nrow = 2, ncol=1)


#Predict on High Frequency Data


#construct plot with cbv 
#1. Read in data
hf_cbv_data_prep <- preProcessData("data_NERR/output/cbv_hf_wq.csv", wq_predictors)
#2. Filter out correct predictors
hf_cbv_data_all <- prepData(hf_cbv_data_prep, wq_predictors)
#3. Match predictions to no NA dataframe
hf_data_cbv <- matchPredictions(result_cbv_rf, hf_cbv_data_all)

#Add color by site
hf_data_cbv_site <- hf_cbv_data_prep %>% 
  mutate(site = as.factor(site)) %>% 
  select(site) %>% 
  cbind(hf_data_cbv)


#4. Create plots
cbv_pred_plot_nh4 <- ggplot(hf_data_cbv_site, aes(x=datetime_round, y = nh4))+
  geom_point()
cbv_pred_plot_no3 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = no3))+
  geom_point()
cbv_pred_plot_po4 <- ggplot(hf_data_cbv, aes(x=datetime_round, y = po4))+
  geom_point()
cbv_pred_plot_chla <- ggplot(hf_data_cbv, aes(x=datetime_round, y = chla))+
  geom_point()



#construct plot with owc
#1. Read in data
hf_owc_data_prep <- preProcessData("data_NERR/output/owc_hf_wq.csv", wq_predictors)
#2. Filter out correct predictors
hf_owc_data_all <- prepData(hf_owc_data_prep, wq_predictors)
#3. Match predictions to no NA dataframe
hf_data_owc <- matchPredictions(result_owc_rf, hf_owc_data_all)

#Add color by site
hf_data_owc_site <- hf_owc_data_prep %>% 
  mutate(site = as.factor(site)) %>% 
  select(site) %>% 
  cbind(hf_data_owc)


#4. Create plots
owc_pred_plot_nh4 <- ggplot(hf_data_owc, aes(x=datetime_round, y = nh4))+
  geom_point()
owc_pred_plot_no3<- ggplot(hf_data_owc, aes(x=datetime_round, y = no3))+
  geom_point()
owc_pred_plot_po4 <- ggplot(hf_data_owc, aes(x=datetime_round, y = po4))+
  geom_point()
owc_pred_plot_chla <- ggplot(hf_data_owc, aes(x=datetime_round, y = chla))+
  geom_point()

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
  N132003nh4 <- ggplot(x, aes(x=datetime_round, y=nh4, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003no3 <- ggplot(x, aes(x=datetime_round, y=no3, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003po4 <- ggplot(x, aes(x=datetime_round, y=po4, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003chla <- ggplot(x, aes(x=datetime_round, y=chla, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003spCond <- ggplot(x, aes(x=datetime_round, y=SpCond.mean, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    xlab("Date")+
    ylab("Conductivity")
  
  return(ggarrange(N132003nh4, N132003no3, N132003po4, N132003chla,  N132003spCond, nrow = 1, ncol=5))
}


nov132003seiche <- hf_data_owc_site[hf_data_owc$datetime_round > "2003-4-4 7:00" & hf_data_owc$datetime_round < "2003-4-8 2:00", ]

oct272019seiche <- hf_data_owc_site[hf_data_owc$datetime_round > "2019-10-30 20:00" & hf_data_owc$datetime_round < "2019-11-2 3:00", ]

ggarrange(createPlots(nov132003seiche, c(hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2003-4-5 5:00"))[1]],
                                         hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2003-4-7 18:00"))[1]])), 
          createPlots(oct272019seiche, c(hf_data_owc$datetime_round[which(hf_data_owc$datetime_round == as.Date("2019-11-1 0:00"))[1]])), 
          ncol = 1, nrow =2 )

isabel <- hf_data_cbv_site[hf_data_cbv_site$datetime_round > "2003-9-16 18:00" & hf_data_cbv$datetime_round < "2003-9-20 22:00", ]

sandy <- hf_data_cbv_site[hf_data_cbv_site$datetime_round > "2012-10-24 22:00" & hf_data_cbv$datetime_round < "2012-10-31 4:00", ]



ggarrange(createPlots(isabel, c(hf_data_cbv$datetime_round[which(hf_data_cbv$datetime_round == as.Date("2003-9-18 21:00"))[1]])), 
          createPlots(sandy, c(hf_data_cbv$datetime_round[which(hf_data_cbv$datetime_round == as.Date("2012-10-29 1:00"))[1]])),
          ncol = 1, nrow =2 )

