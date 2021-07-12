#Observe Seiche & Hurricane Events
createPlots <- function(x, day){
  N132003nh4 <- ggplot(x, aes(x=datetime_round, y=nh4, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003no3 <- ggplot(x, aes(x=datetime_round, y=no3, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003po4 <- ggplot(x, aes(x=datetime_round, y=po4, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003chla <- ggplot(x, aes(x=datetime_round, y=chla, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    theme(legend.position = "none")+
    xlab("Date")
  
  N132003spCond <- ggplot(x, aes(x=datetime_round, y=SpCond.mean, color = site))+
    geom_vline(xintercept = day, color = "black")+
    geom_smooth()+
    xlab("Date")+
    ylab("Conductivity")
  
  return(ggarrange(N132003nh4, N132003no3, N132003po4, N132003chla,  N132003spCond, nrow = 1, ncol=5))
}