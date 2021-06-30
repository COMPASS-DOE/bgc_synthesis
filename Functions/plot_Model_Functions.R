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


#by signature
createImpPlot <- function(x, y, label, pred){
  temp <- x[[pred]][which(x[[pred]]$group == y),]
  colRef <- x[[pred]][which(x[[pred]]$group == y),]
  temp$metric <- factor(temp$metric, levels = temp$metric[order(temp$values)])
  sumValues <- sum(temp$values)
  tempG <- ggplot(temp, aes(x = metric, y = (values/sumValues * 100), fill = colRef$metric))+
    geom_bar(position="dodge", stat="identity")+
    coord_flip()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(), 
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background = element_rect(fill = "white",
                                          colour = "white"), 
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    theme(legend.position = "none")
  
  title.grob <- textGrob(
    label = paste(label, temp$group[1]),
    x = unit(0, "lines"), 
    y = unit(0, "lines"),
    hjust = 0, vjust = 0,
    gp = gpar(fontsize = 16))
  
  p1 <- arrangeGrob(tempG, top = title.grob)
  return(p1)
}



#by Site
createSiteImportancePlots <- function(x, label, pred){
  
  nh4 <- createImpPlot(x, "nh4", "", pred)
  no3 <- createImpPlot(x, "no3", "", pred)
  po4 <- createImpPlot(x, "po4", "", pred)
  chla <- createImpPlot(x, "chla", "", pred)
  

  figure <- ggarrange(nh4, no3, po4, chla, ncol = 4, nrow = 1)
  annotate_figure(figure, left = text_grob("Predictor", color = "black", rot = 90), 
                  bottom = "Importance")
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
    na.omit() 
  
  #Find predictions for four signatures
  nh4 <- predict(x[[2]]$finalModel, predData)
  po4 <- predict(x[[5]]$finalModel, predData)
  no3 <- predict(x[[8]]$finalModel, predData)
  chla <- predict(x[[11]]$finalModel, predData)
  
  
  #Combine predictions with date into one date frame
  predictions <- cbind(nh4, po4, no3, chla)
  names(predictions) <- c("nh4", "po4", "no3", "chla")
  
  #Merge the predictions with the original no na data set and then merge with 
  #original dataset
  
  y[-unique(which(is.na(y), arr.ind = TRUE)[,1]), (ncol(y)+1):(ncol(y)+4)] <- predictions
  
  return(y)
}


preProcessData <- function(x, pred){
  
  #Preprocess high frequency dataset for predicitions to be added
  processed_data <- read_csv(x)%>% 
    mutate(sin_doy = sin(yday(lubridate::date(datetime_round)) / (365.25 * pi))) %>% 
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
           TotPrcp.mean.5 = TotPrcp) 
  
  return(processed_data)
}

prepData <- function(x, y){
  x %>% 
    relocate(y) %>% 
    select(y, "datetime_round")
}


formPDP <- function(pdp, lab){
  TempNH4 <- as_tibble(pdp[[2]])  %>% 
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`)) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "Temp", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  spCondNH4 <- as_tibble(pdp[[5]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "spCond", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  DO_mglNH4 <- as_tibble(pdp[[8]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "DO_mgl", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  DepthNH4 <- as_tibble(pdp[[11]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "Depth", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  pHNH4 <- as_tibble(pdp[[14]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "pH", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  TurbNH4 <- as_tibble(pdp[[17]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "Turb", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  q_cfsNH4 <- as_tibble(pdp[[20]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "q_cfs", 
         y = lab)+
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  sin_doyNH4 <- as_tibble(pdp[[23]]) %>%
    mutate(`_label_` = stringr::str_remove(`_label_`, "random forest_")) %>%
    ggplot(aes(`_x_`, `_yhat_`, color = "#6dcad8")) +
    geom_line(size = 1.2, alpha = 0.8, color = "#6dcad8") +
    labs(x = "sin_doy", 
         y = lab) +
    geom_rug(sides = "b", color = "#6dcad8")+
    theme(legend.position = "none", 
          panel.background = element_rect(fill = "white"))
  
  pdp_all <- ggarrange(TempNH4, spCondNH4, DO_mglNH4, DepthNH4, pHNH4, TurbNH4, q_cfsNH4, sin_doyNH4, ncol = 2, nrow = 4)
  
  
  return(pdp_all)
  
}
