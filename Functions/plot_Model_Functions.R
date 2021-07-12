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
