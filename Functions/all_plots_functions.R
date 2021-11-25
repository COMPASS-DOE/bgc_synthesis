
# Correlation Plots -------------------------------------------------------

#run a p test on the correlation plot
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

#Create an individual correlation plot
createCorr <- function(dt, pred){
  corr_dt <- dt %>% 
    select(pred) %>% 
    na.omit() %>% 
    cor() %>% 
    as.matrix() 
  
  rownames(corr_dt) <- c("Water Temperature",
                            "Conductivity",
                            "Dissolved Oxygen",
                            "Depth",
                            "pH",
                            "Turbidity",
                            "Discharge",
                            "Time of Day",
                            "Ammonia",
                            "Nitrate",
                            "Phosphate",
                            "Chlorophyll a")
  colnames(corr_dt) <- c("Water Temperature",
                            "Conductivity",
                            "Dissolved Oxygen",
                            "Depth",
                            "pH",
                            "Turbidity",
                            "Discharge",
                            "Time of Day",
                            "Ammonia",
                            "Nitrate",
                            "Phosphate",
                            "Chlorophyll a")
  
  p.mat <- cor.mtest(corr_dt)
  
  return(corrplot(corr_dt,
                  type = "upper",
                  method = "circle",
                  tl.col = "black", 
                  sig.level = 0.05, 
                  insig = "blank", 
                  p.mat = p.mat, 
                  tl.srt=45))
  
}

#Create a comparison correlation plot
createCorrComps <- function(dt1, dt2, pred){
  
  corr_cbv <- createCorr(dt1, pred)
  
  corr_owc <- createCorr(dt2, pred)
  
  return(c(corr_cbv, corr_owc))
}


# Feature Importance Plots ------------------------------------------------

#grabs importance for each group of predictors in the model
grabAllImportance <- function(r, modelType = "randomForest"){
  perImpAll <- list()
  for(i in 1:length(r)){
    if(modelType == "ranger"){
      importance <- r[[i]][[2]]$fit$fit$fit$variable.importance
    } else if (modelType == "randomForest"){
      importance <- r[[i]][[2]]$fit$fit$fit$importance[,1]
    } else {
      stop("Not a valid architecture type: choose 'ranger' or 'randomForest'.")
    }
    perImpAll[[i]] <- importance
  }
  return(perImpAll)
}

#Takes the importance from each predictor
clusterChart <- function(importance_result, deps, ref_table = reference_table){
  
  #placeholders for returned data frame
  nums <- which(ref_table$predictor == deps)
  importance_res <- matrix(nrow = length(importance_result[[nums[1]]]) * length(nums),
                           ncol = 3)
  
  #grabbing each row in reference table for importance metric
  values <- vector()
  temp <- vector()
  for(i in 1:length(nums)){
    temp <- append(temp, rep(ref_table[nums[i], 1], length(importance_result[[nums[1]]])))
    values <- append(values, unname(importance_result[[nums[i]]]))
  }
  
  #combines collected data into one dataframe
  importance_res[,1] <- values
  importance_res[,2] <- as.character(temp)
  importance_res[,3] <- unlist(rep(list(names(importance_result[[nums[1]]])),
                                   length(nums)))
  colnames(importance_res) <- c("values", "group", "metric")
  
  #Reformat columns before returning 
  importance_res <- data.frame(importance_res) %>% 
    mutate(group = as.factor(group), 
           metric = as.factor(metric), 
           values = as.numeric(values))
  
  return(data.frame(importance_res))
}

#Create dataframe for each group of predictors of importance
clusterChartModel <- function(imp_model, ref_table = reference_table){
  metPred_metrics <- clusterChart(imp_model, "met_predictors", ref_table)
  wqPred_metrics <- clusterChart(imp_model, "wq_predictors", ref_table)
  allPred_metrics <- clusterChart(imp_model, "all_predictors", ref_table)
  return(list(metPred_metrics, wqPred_metrics, allPred_metrics))
}

#by signature: a single feature importance chart
createImpPlot <- function(x, y, label, pred){
  
  #group nutrient data by y (ex. "nh4")
  temp <- x[[pred]][which(x[[pred]]$group == y),]
  
  #color reference
  colRef <- temp
  
  #set order of the bars to be largest to smallest
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
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
          legend.position = "none")+
    scale_fill_manual(values=c("#E69F00", 
                               "#009E73",
                               "#F0E442",
                               "#56B4E9",
                              
                               
                               "#D55E00",
                               
                               "#0072B2",
                               
                               "#000000",
                               
                               "#CC79A7"))
  
  title.grob <- textGrob(
    label = paste(label, temp$group[1]),
    x = unit(0, "lines"), 
    y = unit(0, "lines"),
    hjust = 0, vjust = 0,
    gp = gpar(fontsize = 16))
  
  p1 <- arrangeGrob(tempG, top = title.grob)
  return(p1)
}

#by Site: a row of length(nutrients) charts
createSiteImportancePlots <- function(x, label, pred){
  
  nh4 <- createImpPlot(x, "nh4", "", pred)
  no3 <- createImpPlot(x, "no3", "", pred)
  po4 <- createImpPlot(x, "po4", "", pred)
  chla <- createImpPlot(x, "chla", "", pred)
  
  
  figure <- ggarrange(nh4, no3, po4, chla, ncol = 4, nrow = 1)
  annotate_figure(figure, left = text_grob("Predictor", color = "black", rot = 90), 
                  bottom = "Importance")
  
  return(figure)
}


