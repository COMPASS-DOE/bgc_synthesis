#Actual versus predicted Plots
makeAVPPlots <- function(x){
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

#Feature Importance plots just for water quality
makeFIPlots <- function(x){
  fiPlots <- list()
  j<-1
  for(i in wq_ind){
    fiPlots[[j]] <- x[[i]]$importancePlot
    j<-j+1
  }
  return(fiPlots)
}

# Partial Dependency Plots ------------------------------------------------

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

