getSummary <- function(x, label){
  summaryByDM <- x %>% 
    mutate(hour = format(datetime_round, "%H")) %>% 
    group_by(hour) %>% 
    na.omit() %>% 
    summarise(nh4.hour.mean = mean(nh4), 
              no3.hour.mean = mean(no3), 
              po4.hour.mean = mean(po4), 
              chla.hour.mean = mean(chla)) %>% 
    add_column(label) %>% 
    mutate(
      nh4.hour.mean = scale(nh4.hour.mean), 
      no3.hour.mean = scale(no3.hour.mean), 
      po4.hour.mean = scale(po4.hour.mean), 
      chla.hour.mean = scale(chla.hour.mean)
    )
  return(summaryByDM)
}

getSummaryMonthH <- function(x1, label1, x2, label2){
  
  #read in cbv data
  summaryByDM1 <- x1 %>% 
    add_column(label1) %>% 
    rename(label=label1)
  
  #read in owc data
  summaryByDM2 <- x2 %>% 
    add_column(label2) %>% 
    rename(label = label2)
  
  #Combine data and add hour and month columns
  combinedSummary <- rbind(summaryByDM1, summaryByDM2) %>% 
    mutate(month = format(datetime_round, "%h"), hour = format(datetime_round, "%H"))
  
  
  #Group data by label
  summaryLabel <- combinedSummary %>% 
    group_split(label)
  
  #Group Data by month
  summaryMonth <- lapply(summaryLabel, function(x) return(group_split(x, month)))
  
  #scale each month in CBV & OWC
  summaryMonthScaledCBV <- lapply(summaryMonth[[1]], function(x) return(x %>% 
                                                                          mutate(nh4 = scale(nh4),
                                                                                 no3 = scale(no3),
                                                                                 po4 = scale(po4),
                                                                                 chla = scale(chla))))
  summaryMonthScaledOWC <- lapply(summaryMonth[[2]], function(x) return(x %>% 
                                                                          mutate(nh4 = scale(nh4),
                                                                                 no3 = scale(no3),
                                                                                 po4 = scale(po4),
                                                                                 chla = scale(chla))))
  
  #group by hour then 
  summaryByHourEachMonthCBV <- lapply(summaryMonthScaledCBV, 
                                      function(x) return(x %>% 
                                                           group_by(hour,label,month) %>% 
                                                           na.omit() %>% 
                                                           summarise(nh4 = mean(nh4), 
                                                                     no3 = mean(no3), 
                                                                     po4 = mean(po4), 
                                                                     chla = mean(chla))))
  
  #group by hour then 
  summaryByHourEachMonthOWC <- lapply(summaryMonthScaledOWC, 
                                      function(x) return(x %>% 
                                                           group_by(hour,label,month) %>% 
                                                           na.omit() %>% 
                                                           summarise(nh4 = mean(nh4), 
                                                                     no3 = mean(no3), 
                                                                     po4 = mean(po4), 
                                                                     chla = mean(chla))))
  
  #order properly
  summaryByHourEachMonthCBVO <- summaryByHourEachMonthCBV[c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)]
  summaryByHourEachMonthOWCO <- summaryByHourEachMonthOWC[c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)]
  
  summaryCBV <- do.call("rbind",  summaryByHourEachMonthCBVO)
  summaryOWC <- do.call("rbind",  summaryByHourEachMonthOWCO)
  
  summaryall<- rbind(summaryCBV, summaryOWC)
  
  return(summaryall)
}

makePolarPlots <- function(dt){
  
  nh4PPcbv <- ggplot(dt, aes(x=hour, y=nh4, color = label))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"), 
          legend.position = "none")
  
  no3PPcbv <- ggplot(dt, aes(x=hour, y=no3, color = label))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"), 
          legend.position = "none")
  
  po4PPcbv <- ggplot(dt, aes(x=hour, y=po4, color = label))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"), 
          legend.position = "none")
  
  chlaPPcbv <- ggplot(dt, aes(x=hour, y=chla, color = label))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"), 
          legend.position = "none")
  
  plot_all <- list(nh4PPcbv, no3PPcbv, po4PPcbv, chlaPPcbv)
  
  return(plot_all)
}


getSummaryMonth <- function(x){
  summaryByDM <- x %>% 
    mutate(month = format(datetime_round, "%h")) %>% 
    group_by(month) %>% 
    na.omit() %>% 
    summarise(nh4.month.mean = mean(nh4), 
              no3.month.mean = mean(no3), 
              po4.month.mean = mean(po4), 
              chla.month.mean = mean(chla))
  
  summaryByDM <- summaryByDM[c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3),]
  summaryByDM$month <- factor(summaryByDM$month, levels = summaryByDM$month)
  return(summaryByDM)
}
