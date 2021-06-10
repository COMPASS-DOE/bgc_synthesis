gatherMetrics <- function(rang, rand){
  gatheredMetrics <- list()
  for(i in 1:length(rang)){
    gatheredMetrics[[i]] <- as_tibble(cbind(rang[[i]][[4]], 
                    rand[[i]][[4]], 
                    rownames(rand[[i]][[4]]))) %>% 
      rename("ranger" = 1,
             "randomForest" = 2, 
             "gof" = 3)
  }
  gatheredMetrics <- bind_rows(gatheredMetrics)
  return(gatheredMetrics)
}

summarizeMetrics <- function(gatheredMetrics){
  sumMetrics <- gatheredMetrics %>% 
    group_by(gof) %>% 
    summarize(ranger = mean(as.numeric(ranger)), randomForest = mean(as.numeric(randomForest)))
    
  return(sumMetrics)
}



grabAllImportance <- function(r, t){
  perImpAll <- list()
  for(i in 1:length(r)){
    if(t == "ranger"){
      importance <- r[[i]][[2]]$fit$fit$fit$variable.importance
    } else if (t == "randomForest"){
      importance <- r[[i]][[2]]$fit$fit$fit$importance[,1]
    } else {
      stop("Not a valid architecture type: choose 'ranger' or 'randomForest'.")
    }
    perImpAll[[i]] <- importance
  }
  return(perImpAll)
}

clusterChart <- function(importance_result, deps, ref_table = reference_table){
  nums <- vector()
  nums <- which(ref_table$predictor == deps)
  importance_res <- matrix(nrow = length(importance_result[[nums[1]]]) * length(nums),
                           ncol = 3)
  
  values <- vector()
  temp <- vector()
  for(i in 1:length(nums)){
    temp<- append(temp, rep(ref_table[nums[i], 1], length(importance_result[[nums[1]]])))
    values <- append(values, unname(importance_result[[nums[i]]]))
  }
  importance_res[,1] <- values
  importance_res[,2] <- as.character(temp)
  importance_res[,3] <- unlist(rep(list(names(importance_result[[nums[1]]])),
                               length(nums)))
  colnames(importance_res) <- c("values", "group", "metric")
  
  importance_res <- data.frame(importance_res) %>% 
    mutate(group = as.factor(group), 
           metric = as.factor(metric), 
           values = as.numeric(values))
  return(data.frame(importance_res))
}

clusterChartModel <- function(imp_model, ref_table = reference_table){
  metPred_metrics <- clusterChart(imp_model, "met_predictors", ref_table)
  wqPred_metrics <- clusterChart(imp_model, "wq_predictors", ref_table)
  allPred_metrics <- clusterChart(imp_model, "all_predictors", ref_table)
  return(list(metPred_metrics, wqPred_metrics, allPred_metrics))
}

#Make predictions of the four signatures and bind it to the hf data
matchPredictions <- function(x, y){
  
  #split data into date, predictors, and no nas
  predData <- y %>% 
    na.omit() %>% 
    select(-last_col())
  
  #Find predictions fo four ssignatures
  nh4 <- predict(x[[3]]$finalModel, predData)
  po4 <- predict(x[[6]]$finalModel, predData)
  no3 <- predict(x[[9]]$finalModel, predData)
  chla <- predict(x[[12]]$finalModel, predData)
  
  
  #Combine predictions with date into one date frame
  predictions <- cbind(nh4, po4, no3, chla)
  names(predictions) <- c("nh4", "po4", "no3", "chla")
  
  #Merge the predictions with the original no na data set and then merge with 
  #original dataset
  
  y[-unique(which(is.na(y), arr.ind = TRUE)[,1]), (ncol(y)+1):(ncol(y)+4)] <- predictions
  
  return(y)
}


preProcessData <- function(x){
  
  #Preprocess high frequency dataset for predicitions to be added
  processed_data <- read_csv(x)%>% 
    mutate(sin_doy = sin(yday(lubridate::date(datetime_round)) / (365.25 * pi))) %>% 
    select(-c(2)) %>% 
    rename(Temp.mean = Temp, 
           SpCond.mean = SpCond, 
           DO_mgl.mean = DO_mgl, 
           Depth.mean = Depth, 
           pH.mean = pH, 
           Turb.mean = Turb, 
           q_cfs.mean.5 = q_cfs, 
           sin_doy = sin_doy, 
           ATemp.mean = ATemp, 
           RH.mean = RH, 
           BP.mean = BP, 
           WSpd.mean = WSpd, 
           Wdir.mean= Wdir, 
           TotPAR.mean = TotPAR, 
           TotPrcp.mean.5 = TotPrcp) %>% 
    relocate(all_predictors)
  
  return(processed_data)
}