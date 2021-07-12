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
  
  #Combine predictions into one date frame
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
    relocate(all_of(y)) %>% 
    select(all_of(y), "datetime_round")
}



