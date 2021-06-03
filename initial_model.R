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
p_load(tidyverse, ggplot2, cowplot, lubridate, tidymodels,splitTools, ggthemes, parallel)

#######################################
#####Functions
#######################################


# Read in data
read_station <- function(dt){
  dt_all <- read_csv(dt) %>% 
    mutate(sin_doy = sin(yday(lubridate::date(datetime_round)) / (365.25 * pi))) %>%
    select(-datetime_round) %>% 
    select_if(is.double)
    return(dt_all)
}

findProp <- function(dt){
  
}

# Create model, and return useful things (data, plots, stats)
choose_inputs <- function(all_data, dep, pred) {
  
  source <- str_split(deparse(substitute(df)), "_")[[1]][1]
  type <- str_split(deparse(substitute(pred)), "_")[[1]][1]
  
  dt_all <- all_data %>% 
    select(dep, pred) %>% 
    drop_na() %>% 
    rename(actual = dep)
  correlations <- cor(dt_all)
  
  
  set.seed(42)
  
  #Time-series cross-validation and block partitioning
  initSplit <- partition(dt_all$actual, p=c(train=0.8, test = 0.2), type = "blocked")
  #str(initSplit)
  
  
  df_train <- dt_all[initSplit$train, ]
  df_test <- dt_all[initSplit$test, ]
  
  #Create time folds
  folds <- create_timefolds(df_train$actual, k = 10)
  
  #Tune mtry by GridSearchCV
  valid_mtry <- numeric(ncol(df_train) - 1)
  all_cv_mtry <- list()
  
  
  f <- as.formula(paste(names(df_train)[1], "~", paste(names(df_train)[-1], collapse=" + ")))
  
  for (i in seq_along(valid_mtry)) {
    cv_mtry <- numeric()
    for (fold in folds) {
      
      fit <- rand_forest(mode = "regression", trees = 1000, mtry = i) %>%
        set_engine("ranger") %>%
        fit(formula = f, data=df_train[fold$insample, ])
      
      
      cv_mtry <- c(cv_mtry, 
                   rmse(df_train[fold$outsample, "actual"], 
                        predict(fit, df_train[fold$outsample, ])))
    }
    all_cv_mtry[[i]] <- cv_mtry
    valid_mtry[i] <- mean(cv_mtry)
  }
  
  (best_mtry <- which.min(valid_mtry))
  
  
  final_fit <- rand_forest(mode = "regression", trees = 1000, mtry = best_mtry) %>%
    set_engine("ranger") %>%
    fit(formula = f, data=df_train)
  
  test_pred <- predict(fit, df_test)
  
  mean_average_error <- mae(df_test$actual, test_pred, length(pred))
  root_mean <- rmse(df_test$actual, test_pred)
  
  dat <- data.frame(seq_along(dt_all$actual))
  
  test_dat <- c(test_pred, df_test$actual)
  
  #Create a plot of the test data with predicted metrics
  df_test_pred <- cbind(df_test, test_pred)
  Fig <- ggplot(df_test_pred, aes(1:nrow(df_test), actual))+
          geom_point(color = "red")+
          geom_line(aes(1:nrow(df_test),.pred), size = 1.2)+
          xlab("Time (15 minutes)")+
          labs(subtitle = paste0("RMSE: ", round(root_mean, digits = 4), 
                                 "\nMAE: ", round(mean_average_error, digits = 4)))+
          theme_tufte()

  
  #TODO create models 
  Fig_result <- list(Fig, final_fit, valid_mtry, all_cv_mtry)
  return(Fig_result)
}

rmse <- function(y, pred) {
  return(sqrt(colMeans((y - pred)^2)))
}

mae <- function(y, pred, n) {
  return((colMeans(abs(y - pred)))/n)
}

#######################################
#####Constants
#######################################

# Set columns for independent and dependent variables
dependents <- c("chla", "nh4", "no3", "po4")
wq_predictors <- c("Temp.mean", 
                   "SpCond.mean", 
                   "DO_mgl.mean", 
                   "Depth.mean", 
                   "pH.mean", 
                   "Turb.mean", 
                   "q_cfs.mean.5", 
                   "sin_doy")
met_predictors <- c("ATemp.mean", 
                    "RH.mean", 
                    "BP.mean", 
                    "WSpd.mean", 
                    "Wdir.mean", 
                    "TotPAR.mean", 
                    "TotPrcp.mean.5", 
                    "sin_doy")
all_predictors <- unique(append(wq_predictors, met_predictors))

predicters <- list(wq_predictors, met_predictors, all_predictors)

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

cl <- makeCluster(numCores, outfile ='')

clusterExport(cl, c("cbv_all",
                    "owc_all",
                    "rmse",
                    "mae",
                    "choose_inputs",
                    "wq_predictors",
                    "met_predictors",
                    "all_predictors"))

clusterEvalQ(cl, {
  library(ggplot2)
  library(tidyverse)
  library(splitTools)
  library(tidymodels)
  library(tidyverse)
  library(lubridate)
  library(ggthemes)
})


reference_table <- tibble(dep = character(), predicter = character()) %>% 
  add_row(dep = rep("nh4",3), predicter = c("met_predictors", 
                                        "wq_predictors", 
                                        "all_predictors")) %>% 
  add_row(dep = rep("po4",3), predicter = c("met_predictors", 
                                            "wq_predictors", 
                                            "all_predictors")) %>% 
  add_row(dep = rep("no3",3), predicter = c("met_predictors", 
                                            "wq_predictors", 
                                            "all_predictors")) %>% 
  add_row(dep = rep("chla",3), predicter = c("met_predictors", 
                                            "wq_predictors", 
                                            "all_predictors"))


result_cbv <- parApply(cl,reference_table,1, function(x) choose_inputs(cbv_all, x[1], eval(parse(text = x[2]))))
result_owc <- parApply(cl,reference_table,1, function(x) choose_inputs(owc_all, x[1], eval(parse(text = x[2]))))
stopCluster(cl)

#record rootMSE
rootMSE <- vector()
meanAE <- vector()
for(i in 1:length(result_cbv)){
  val<- as.double(str_split(str_split(result_cbv[[i]][[1]]$labels$subtitle, "\n", 2)[[1]][1], " ", 2)[[1]][2])
  rootMSE <- rootMSE %>% append(val)
  val<- as.double(str_split(str_split(result_cbv[[i]][[1]]$labels$subtitle, "\n", 2)[[1]][2], " ", 2)[[1]][2])
  meanAE <- meanAE %>% append(val)
}

reference_table_rmse_cbv <- cbind(reference_table, rootMSE, meanAE)%>% 
  mutate(dep = as.factor(dep))


rootMSE <- vector()
meanAE <- vector()
for(i in 1:length(result_owc)){
  val<- as.double(str_split(str_split(result_owc[[i]][[1]]$labels$subtitle, "\n", 2)[[1]][1], " ", 2)[[1]][2])
  rootMSE <- rootMSE %>% append(val)
  val<- as.double(str_split(str_split(result_owc[[i]][[1]]$labels$subtitle, "\n", 2)[[1]][2], " ", 2)[[1]][2])
  meanAE <- meanAE %>% append(val)
}

reference_table_rmse_owc <- cbind(reference_table, rootMSE, meanAE) %>% 
  mutate(dep = as.factor(dep))

meanRSME <- vector(length = 10)
for(i in 1:12){
  meanRSME <- as.numeric(meanRSME) + as.numeric(result_cbv[[i]][[4]][[1]])
}
meanRSME <- data.frame(meanRSME/10)

ggplot(meanRSME, aes(x = c(1:10), y = meanRSME[,1]))+
  geom_bar(stat="identity")

#ggplot(reference_table_rmse_cbv, aes(x = dep, 
                                     #y = rootMSE, 
                                     #fill = predicter))+
  #geom_bar(stat="identity", 
           #position=position_dodge())






