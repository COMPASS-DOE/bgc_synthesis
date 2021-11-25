#Observe Seiche & Hurricane Events
  createPlots <- function(x, day){
    N132003nh4 <- ggplot(x, aes(x=datetime_round, y=nh4, color = site))+
      geom_vline(xintercept = day, color = "black")+
      geom_smooth()+
      xlab("Date")+
      ylab("Ammonia (mg/L)")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    N132003no3 <- ggplot(x, aes(x=datetime_round, y=no3, color = site))+
      geom_vline(xintercept = day, color = "black")+
      geom_smooth()+
      xlab("Date")+
      ylab("Nitrate (mg/L)")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    N132003po4 <- ggplot(x, aes(x=datetime_round, y=po4, color = site))+
      geom_vline(xintercept = day, color = "black")+
      geom_smooth()+
      xlab("Date")+
      ylab("Phosphate (mg/L)")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    N132003chla <- ggplot(x, aes(x=datetime_round, y=chla, color = site))+
      geom_vline(xintercept = day, color = "black")+
      geom_smooth()+
      xlab("Date")+
      ylab("Chlorophyll a (ug/L)")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    plots <- list(N132003nh4, N132003no3, N132003po4, N132003chla)
    names(plots)<-c("ammonia", "nitrate", "phosphate", "chlorophyll a")
    return(plots)
  }

# Hysteresis Plots --------------------------------------------------------

  hysteresisPlots <- function(dt){
    
    #summarise by hour
    dt <- dt %>% 
      group_split(site) %>%
      lapply(function(x) x %>% 
               mutate(hour = format(datetime_round, "%m-%d-%H")) %>% 
               na.omit() %>% 
               group_by(hour) %>% 
               summarise(nh4.hour.mean = mean(nh4), 
                         no3.hour.mean = mean(no3), 
                         po4.hour.mean = mean(po4), 
                         chla.hour.mean = mean(chla), 
                         discharge.hour.mean = mean(q_cfs.mean.5),
                         temp.hour.mean = mean(Temp.mean), 
                         SpCond.hour.mean = mean(SpCond.mean),
                         pH.hour.mean = mean(pH.mean),
                         DO.hour.mean = mean(DO_mgl.mean),
                         Turb.hour.mean = mean(Turb.mean)) %>% 
               mutate(nh4.hour.mean = normalize(nh4.hour.mean), 
                      no3.hour.mean = normalize(no3.hour.mean), 
                      po4.hour.mean = normalize(po4.hour.mean), 
                      chla.hour.mean = normalize(chla.hour.mean), 
                      discharge.hour.mean = normalize(discharge.hour.mean),
                      temp.hour.mean = normalize(temp.hour.mean), 
                      SpCond.hour.mean = normalize(SpCond.hour.mean),
                      pH.hour.mean = normalize(pH.hour.mean),
                      DO.hour.mean = normalize(DO.hour.mean),
                      Turb.hour.mean = normalize(Turb.hour.mean)))
    
    #Make lists
    TurbDischarge <- list()
    nh4Discharge <- list()
    no3Discharge <- list()
    po4Discharge <- list()
    chlaDischarge <- list()
    
    #Create plots
      for(val in 1:length(dt)){
        #Turbidity versus discharge
        TurbDischarge[[val]] <- ggplot(dt[[val]], aes(y = Turb.hour.mean, x = discharge.hour.mean, label = hour))+
          geom_point()+
          geom_segment(aes(
            yend=c(tail(Turb.hour.mean, n=-1), NA), 
            xend=c(tail(discharge.hour.mean, n=-1), NA)),
            arrow=arrow(length=unit(0.2,"cm"))
          )+
          xlab("Discharge (cubic feet per sec)")+
          ylab("Turbidity (FNU/NTU)")+
          theme(panel.background = element_rect(fill = "white"))
        
        nh4Discharge[[val]] <- ggplot(dt[[val]], aes(y = nh4.hour.mean, x = discharge.hour.mean, label = hour))+
          geom_point()+
          geom_segment(aes(
            yend=c(tail(nh4.hour.mean, n=-1), NA), 
            xend=c(tail(discharge.hour.mean, n=-1), NA)),
            arrow=arrow(length=unit(0.2,"cm"))
          )+
          xlab("Discharge (cubic feet per sec)")+
          ylab("Ammonia (mg/L)")+
          theme(panel.background = element_rect(fill = "white"))
        
        no3Discharge[[val]] <- ggplot(dt[[val]], aes(y = no3.hour.mean, x = discharge.hour.mean, label = hour))+
          geom_point()+
          geom_segment(aes(
            yend=c(tail(no3.hour.mean, n=-1), NA), 
            xend=c(tail(discharge.hour.mean, n=-1), NA)),
            arrow=arrow(length=unit(0.2,"cm"))
          )+
          xlab("Discharge (cubic feet per sec)")+
          ylab("Nitrate (mg/L)")+
          theme(panel.background = element_rect(fill = "white"))
        
        po4Discharge[[val]] <- ggplot(dt[[val]], aes(y = po4.hour.mean, x = discharge.hour.mean, label = hour))+
          geom_point()+
          geom_segment(aes(
            yend=c(tail(po4.hour.mean, n=-1), NA), 
            xend=c(tail(discharge.hour.mean, n=-1), NA)),
            arrow=arrow(length=unit(0.2,"cm"))
          )+
          xlab("Discharge (cubic feet per sec)")+
          ylab("Phosphate (mg/L)")+
          theme(panel.background = element_rect(fill = "white"))
        
        chlaDischarge[[val]] <- ggplot(dt[[val]], aes(y = chla.hour.mean, x = discharge.hour.mean, label = hour))+
          geom_point()+
          geom_segment(aes(
            yend=c(tail(chla.hour.mean, n=-1), NA), 
            xend=c(tail(discharge.hour.mean, n=-1), NA)),
            arrow=arrow(length=unit(0.2,"cm"))
          )+
          xlab("Discharge (cubic feet per sec)")+
          ylab("Chlorophyll a (ug/L)")+
          theme(panel.background = element_rect(fill = "white"))
      }
    
      plots <- list()
      plots <- c(TurbDischarge, nh4Discharge, no3Discharge, po4Discharge, chlaDischarge)
    return(plots)
  }


# Normalization -----------------------------------------------------------

  normalize <- function(x){
    return((x-min(x,na.rm = TRUE))/(max(x, na.rm = TRUE)-min(x, na.rm = TRUE)))
  }


# Meta charts -------------------------------------------------------------
  
  createMetaPlots <- function(dt){
    
    #create plot for discharge
    discharge <- ggplot(dt, aes(x = datetime_round, color = site))+
      geom_bar(aes(y=normalize(q_cfs.mean.5)), stat = "identity")+
      ylab("Normalized Discharge")+
      geom_vline(xintercept =  dt$datetime_round[which.max(dt$q_cfs.mean.5)],
                 color = "black")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      xlab("Date")
    
    #Create plot for conductivity
    conductivity <- ggplot(dt, aes(x = datetime_round, color = site))+
      geom_line(aes(y=normalize(SpCond.mean)))+
      ylab("Normalized Conductivity")+
      geom_vline(xintercept =  dt$datetime_round[which.max(dt$q_cfs.mean.5)],
                 color = "black")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      xlab("Date")
    
    #create plot for depth
    depth <- ggplot(dt, aes(x = datetime_round, color = site))+
      geom_line(aes(y=normalize(Depth.mean)))+
      ylab("Normalized Depth")+
      geom_vline(xintercept =  dt$datetime_round[which.max(dt$q_cfs.mean.5)],
                 color = "black")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white"),
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      xlab("Date")
    
    #combine
    metaplot <- list(discharge, conductivity, depth)
    names(metaplot)<-c("discharge", "conductivity", "depth")
    
    return(metaplot)
  }   


