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

getSummary <- function(x){
  summaryByDM <- read_csv(x) %>% 
    mutate(hour = format(datetime_round, "%H")) %>% 
    group_by(hour) %>% 
    na.omit() %>% 
    summarise(nh4.hour.mean = mean(nh4), 
              no3.hour.mean = mean(no3), 
              po4.hour.mean = mean(po4), 
              chla.hour.mean = mean(chla))
  return(summaryByDM)
}



hf_data_cbv_summary <- getSummary("data_NERR/output/cbv_hf_wq_predictions.csv")
  
hf_data_owc_summary <- getSummary("data_NERR/output/owc_hf_wq_predictions.csv")

#CBV location
nh4PPcbv <- ggplot(hf_data_cbv_summary, aes(x=hour, y=nh4.hour.mean))+geom_point()+coord_polar()+ theme_bw()

no3PPcbv<- ggplot(hf_data_cbv_summary, aes(x=hour, y=no3.hour.mean))+geom_point()+coord_polar()+ theme_bw()

po4PPcbv<- ggplot(hf_data_cbv_summary, aes(x=hour, y=po4.hour.mean))+geom_point()+coord_polar()+ theme_bw()

chlaPPcbv<- ggplot(hf_data_cbv_summary, aes(x=hour, y=chla.hour.mean))+geom_point()+coord_polar()+ theme_bw()

#OWC location
nh4PPowc <- ggplot(hf_data_owc_summary, aes(x=hour, y=nh4.hour.mean))+geom_point()+coord_polar()+ theme_bw()

no3PPowc<- ggplot(hf_data_owc_summary, aes(x=hour, y=no3.hour.mean))+geom_point()+coord_polar()+ theme_bw()

po4PPowc<- ggplot(hf_data_owc_summary, aes(x=hour, y=po4.hour.mean))+geom_point()+coord_polar()+ theme_bw()

chlaPPowc<- ggplot(hf_data_owc_summary, aes(x=hour, y=chla.hour.mean))+geom_point()+coord_polar()+ theme_bw()

ggarrange(nh4PPcbv, no3PPcbv, po4PPcbv, chlaPPcbv, nh4PPowc, no3PPowc, po4PPowc, chlaPPowc, nrow = 2, ncol = 4)



getSummaryMonthH <- function(x){
  summaryByDM <- read_csv(x) %>% 
    mutate(month = format(datetime_round, "%h"), hour = format(datetime_round, "%H")) %>% 
    group_split(month)
  
  listSummaryByMonth <- lapply(summaryByDM, function(x) return(x %>% 
                         group_by(hour) %>% 
                         na.omit() %>% 
                         summarise(nh4 = mean(nh4), 
                                   no3 = mean(no3), 
                                   po4 = mean(po4), 
                                   chla = mean(chla))))
  return(listSummaryByMonth)
}

makePolarPlots <- function(dt){
  nh4PPcbv <- ggplot(dt, aes(x=hour, y=nh4))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"))
  
  no3PPcbv <- ggplot(dt, aes(x=hour, y=no3))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"))
  
  po4PPcbv <- ggplot(dt, aes(x=hour, y=po4))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"))
  
  chlaPPcbv <- ggplot(dt, aes(x=hour, y=chla))+geom_point()+coord_polar()+
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          panel.background = element_rect(fill = "white"))
  
  plot_all <- list(nh4PPcbv, no3PPcbv, po4PPcbv, chlaPPcbv)
  
  return(plot_all)
}

hf_data_cbv_summaryPP <- getSummaryMonthH("data_NERR/output/cbv_hf_wq_predictions.csv")

hf_data_owc_summaryPP <- getSummaryMonthH("data_NERR/output/owc_hf_wq_predictions.csv")

allPolarPlots <- lapply(hf_data_cbv_summaryPP, makePolarPlots)

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
  return(summaryByDM)
}

hf_data_cbv_summaryBP <- getSummaryMonth("data_NERR/output/cbv_hf_wq_predictions.csv")

hf_data_owc_summaryBP <- getSummaryMonth("data_NERR/output/owc_hf_wq_predictions.csv")


barnh4 <- ggplot(hf_data_cbv_summaryBP, aes(x = month, y=nh4.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

barno3 <- ggplot(hf_data_cbv_summaryBP, aes(x = month, y=no3.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

barpo4 <- ggplot(hf_data_cbv_summaryBP, aes(x = month, y=po4.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

barchla <- ggplot(hf_data_cbv_summaryBP, aes(x = month, y=chla.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

ggarrange(
ggarrange(nh4PP, barnh4, nrow = 2, ncol = 1, heights = c(2,3)),

ggarrange(no3PP, barno3, nrow = 2, ncol = 1, heights = c(2,3)),

ggarrange(po4PP, barpo4, nrow = 2, ncol = 1, heights = c(2,3)),
  
ggarrange(chlaPP, barchla, nrow = 2, ncol = 1, heights = c(2,3)),

nrow=4, ncol =1)











allPolarPlots <- lapply(hf_data_owc_summaryPP, makePolarPlots)
allPolarPlots[[4]] <- NULL
allPolarPlotsSign <- do.call(c, allPolarPlots)


nh4PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(1, 44, 4))], nrow = 1, ncol=11)

no3PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(2, 44, 4))], nrow = 1, ncol=11)

po4PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(3, 44, 4))], nrow = 1, ncol=11)

chlaPP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(4, 44, 4))], nrow = 1, ncol=11)








barnh4 <- ggplot(hf_data_owc_summaryBP, aes(x = month, y=nh4.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

barno3 <- ggplot(hf_data_owc_summaryBP, aes(x = month, y=no3.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

barpo4 <- ggplot(hf_data_owc_summaryBP, aes(x = month, y=po4.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()

barchla <- ggplot(hf_data_owc_summaryBP, aes(x = month, y=chla.month.mean))+
  geom_bar(stat = "identity")+
  theme_classic()



ggarrange(
  ggarrange(nh4PP, barnh4, nrow = 2, ncol = 1, heights = c(2,3)),
  
  ggarrange(no3PP, barno3, nrow = 2, ncol = 1, heights = c(2,3)),
  
  ggarrange(po4PP, barpo4, nrow = 2, ncol = 1, heights = c(2,3)),
  
  ggarrange(chlaPP, barchla, nrow = 2, ncol = 1, heights = c(2,3)),
  
  nrow=4, ncol =1)


