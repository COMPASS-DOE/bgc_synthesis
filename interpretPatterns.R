## ---------------------------
##
## Script name: interpretPatterns.R
##
## Purpose of script: Evaluate daily and yearly fluxes in high frequency water
## data
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-06-10
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

getSummary <- function(x, label){
  summaryByDM <- read_csv(x) %>% 
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



hf_data_cbv_summary <- getSummary("data_NERR/output/cbv_hf_wq_predictions.csv", "CBV")
  
hf_data_owc_summary <- getSummary("data_NERR/output/owc_hf_wq_predictions.csv", "OWC")

hf_data_all <- rbind(hf_data_cbv_summary, hf_data_owc_summary)

#All
nh4PPall <- ggplot(hf_data_all, aes(x=hour, y=nh4.hour.mean, color = label))+
  geom_point()+coord_polar()+ theme(legend.position = "none")

no3PPall <- ggplot(hf_data_all, aes(x=hour, y=no3.hour.mean, color = label))+
  geom_point()+coord_polar()+ theme(legend.position = "none")

po4PPall <- ggplot(hf_data_all, aes(x=hour, y=po4.hour.mean, color = label))+
  geom_point()+coord_polar()+ theme(legend.position = "none")


chlaPPall <- ggplot(hf_data_all, aes(x=hour, y=chla.hour.mean, color = label))+
  geom_point()+coord_polar()+ theme()

ggarrange(nh4PPall, no3PPall, po4PPall, chlaPPall, nrow = 1, ncol = 4)



getSummaryMonthH <- function(x1, label1, x2, label2){
  
  #read in cbv data
  summaryByDM1 <- read_csv(x1) %>% 
    add_column(label1) %>% 
    rename(label=label1)
  
  #read in owc data
  summaryByDM2 <- read_csv(x2) %>% 
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

hf_data_cbv_summaryAll <- getSummaryMonthH("data_NERR/output/cbv_hf_wq_predictions.csv", "CBV",
                                          "data_NERR/output/owc_hf_wq_predictions.csv", "OWC") %>% 
                            as.data.frame() %>% 
                            group_split(month)
hf_data_cbv_summaryAll<-hf_data_cbv_summaryAll[c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)]

allPolarPlots <- lapply(hf_data_cbv_summaryAll, makePolarPlots)

allPolarPlotsSign <- do.call(c, allPolarPlots)


nh4PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(1, 48, 4))], nrow = 1, ncol=12)

no3PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(2, 48, 4))], nrow = 1, ncol=12)

po4PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(3, 48, 4))], nrow = 1, ncol=12)

chlaPP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(4, 48, 4))], nrow = 1, ncol=12)






getSummaryMonth <- function(x){
  summaryByDM <- read_csv(x) %>% 
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

hf_data_cbv_summaryBP <- getSummaryMonth("data_NERR/output/cbv_hf_wq_predictions.csv") %>% 
                          add_column("CBV") %>% 
                          rename(label = 6)  %>% 
  mutate(nh4.month.mean = scale(nh4.month.mean),
         no3.month.mean = scale(no3.month.mean),
         po4.month.mean = scale(po4.month.mean),
         chla.month.mean =scale(chla.month.mean)
  )

hf_data_owc_summaryBP <- getSummaryMonth("data_NERR/output/owc_hf_wq_predictions.csv") %>% 
                          add_column("OWC") %>% 
                          rename(label = 6) %>% 
  mutate(nh4.month.mean = scale(nh4.month.mean),
         no3.month.mean = scale(no3.month.mean),
         po4.month.mean = scale(po4.month.mean),
         chla.month.mean =scale(chla.month.mean)
  )

hf_data_all <- rbind(hf_data_cbv_summaryBP, hf_data_owc_summaryBP) %>% 
  na.omit() 

barnh4 <- ggplot(hf_data_all, aes(x = month, y=nh4.month.mean, fill=label))+
  geom_bar(position = "dodge", stat = "identity")+
  theme(legend.position = "none")

barno3 <- ggplot(hf_data_all, aes(x = month, y=no3.month.mean, fill=label))+
  geom_bar(position = "dodge", stat = "identity")+
  theme(legend.position = "none")

barpo4 <- ggplot(hf_data_all, aes(x = month, y=po4.month.mean, fill=label))+
  geom_bar(position = "dodge", stat = "identity")+
  theme(legend.position = "none")

barchla <- ggplot(hf_data_all, aes(x = month, y=chla.month.mean, fill=label))+
  geom_bar(position = "dodge", stat = "identity")

ggarrange(
ggarrange(nh4PP, barnh4, nrow = 2, ncol = 1, heights = c(2,3)),

ggarrange(no3PP, barno3, nrow = 2, ncol = 1, heights = c(2,3)),

ggarrange(po4PP, barpo4, nrow = 2, ncol = 1, heights = c(2,3)),
  
ggarrange(chlaPP, barchla, nrow = 2, ncol = 1, heights = c(2,3)),

nrow=4, ncol =1)
