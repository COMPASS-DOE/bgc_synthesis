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
p_load(tidyverse, ggplot2, cowplot, lubridate, tidymodels,splitTools, ggthemes, parallel, ggpubr, hydroGOF)

#Load in necessary packages
source("Functions/train_Rforest_functions.R")
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
cl <- makeCluster(numCores, outfile ='')

#export required functions
clusterExport(cl, c("cbv_all",
                    "owc_all",
                    "wq_predictors",
                    "met_predictors",
                    "all_predictors"))

#Export necessary libraries
clusterEvalQ(cl, {
  library(ggplot2)
  library(tidyverse)
  library(tidymodels)
  library(tidyverse)
  library(lubridate)
  library(ggthemes)
  library(hydroGOF)
  source("Functions/train_Rforest_functions.R")
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
                                                      importance = "impurity"))
#Train data on owc location with ranger
result_owc_ranger <- parApply(cl,reference_table,1, 
                              function(x) choose_inputs(
                                                      owc_all, 
                                                      x[1], 
                                                      eval(parse(text = x[2])), 
                                                      x[3],
                                                      modelType = "ranger",
                                                      importance = "impurity"))
#Train data on cbv location with random forest
result_cbv_rf <- parApply(cl,reference_table,1, 
                          function(x) choose_inputs(
                                                  cbv_all, 
                                                  x[1], 
                                                  eval(parse(text = x[2])), 
                                                  x[3], 
                                                  modelType = "randomForest", 
                                                  importance = TRUE))
#Train data on owc location with random forest
result_owc_rf <- parApply(cl,reference_table,1, 
                          function(x) choose_inputs(
                                                  owc_all, 
                                                  x[1], 
                                                  eval(parse(text = x[2])), 
                                                  x[3],
                                                  modelType = "randomForest", 
                                                  importance = TRUE))

#END parrallel processing
stopCluster(cl)

source("Functions/plot_Model_Functions.R")

#Compare ranger and random forest metrics in cbv location
gatheredMetrics_cbv <- gatherMetrics(result_cbv_ranger, result_cbv_rf)

sumMetrics_CBV <- summarizeMetrics(gatheredMetrics_cbv)

sumMetrics_CBV


#Compare ranger and random forest metrics in owc location
gatheredMetrics_owc <- gatherMetrics(result_owc_ranger, result_owc_rf)

sumMetrics_OWC <- summarizeMetrics(gatheredMetrics_owc) 

sumMetrics_OWC




