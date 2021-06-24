#Load in reference table
load("Model/referenceTable.Rdata")

#Load in trained models
load("Model/randomForestOWC.RData")
load("Model/randomForestCBV.RData")
load("Model/rangerOWC.RData")
load("Model/rangerCBV.RData")

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

#indexes of wq 
wq_ind <- seq(2,11,3)

#1. table with just wq 

wqTable <- sumTable[wq_ind,]
colnames(wqTable) <- c("Signal", "Predictor", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE", "MAE", "RMSE", "NSE")

kable(wqTable) %>%  
  remove_column(1) %>% 
  add_header_above(c(" " = 2, "CBV" = 3, "OWC" = 3, "CBV" = 3, "OWC" = 3)) %>% 
  add_header_above(c(" " = 2, "ranger" = 6, "randomForest" = 6)) %>% 
  kable_classic() %>% 
  column_spec(c(3:5, 9:11), 
              background = "lightgrey")

#2. Actual versus predicted Plots

makeAVPPlots <- function(x){
  #Actual versus predicted Plots
  avpPlots <- list()
  j <- 1
  for(i in wq_ind){
    avp <-x[[i]]$plot$data[,c(2,4)]
    
    
    
    avpPlots[[j]] <- ggplot(avp, aes(x=actual, y=.pred))+
      geom_point()+
      geom_abline(intercept = 0, slope = 1, color="red")+
      geom_text(x = (mean(avp$actual)/2), y = (max(avp$.pred)*4/5),
                label = paste0("r2: ",as.character(cor(avp$actual, avp$.pred)^2)),
                parse = TRUE)
    j <- j+1
  }
  return(avpPlots)
}

cbv_avp <- makeAVPPlots(result_cbv_rf)
owc_avp <- makeAVPPlots(result_owc_rf)

ggarrange(plotlist=c(cbv_avp, owc_avp), ncol = 4, nrow=2)

#3. Feature Importance Plots for just wq
makeFIPlots <- function(x){
  fiPlots <- list()
  j<-1
  for(i in wq_ind){
    fiPlots[[j]] <- x[[i]]$importancePlot
    j<-j+1
  }
  return(fiPlots)
}

cbv_fi <- makeFIPlots(result_cbv_rf)
owc_fi <- makeFIPlots(result_owc_rf)
ggarrange(plotlist=c(cbv_fi, owc_fi), ncol=4, nrow=2)


#4. Color code seiche and hurricane events by station



