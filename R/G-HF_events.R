## ---------------------------
##
## Script name: HF_events
##
## Purpose of script: subset HF data to event specified
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-07-12
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------


# 1. Setup ----------------------------------------------------------------

  # Clean workspace
  rm(list = ls())
  
  # Load packages
  require(pacman)
  p_load(tidyverse, 
         corrplot, 
         ggplot2, 
         gridExtra, 
         cowplot, 
         lubridate, 
         tidymodels,
         splitTools, 
         ggthemes, 
         parallel, 
         ggpubr, 
         hydroGOF, 
         pdp, 
         DALEXtra, 
         kableExtra, 
         hysteresis)
  
  #Functions for HF events
  source("Functions/HF_events_functions.R")
  #Load in necessary functions to plot
  source("Functions/eval_HF_functions.R")
  
  #Variables treated as constants
  source("Constants/initial_model_constants.R")
  
  #HF predictions
  hf_data_cbv <- read_csv("data_NERR/output/cbv_hf_wq_predictions.csv")
  hf_data_owc <- read_csv("data_NERR/output/owc_hf_wq_predictions.csv")
  
  

# Add site data to predictions -------------------------------------------

  #1. Read in data
  siteDataCBV <- preProcessData("data_NERR/output/cbv_hf_wq.csv", wq_predictors) %>% 
                select(site)
  hf_data_cbv_site <- cbind(hf_data_cbv, siteDataCBV)
  
  #1. Read in data
  siteDataOWC <- preProcessData("data_NERR/output/owc_hf_wq.csv", wq_predictors) %>% 
    select(site)
  hf_data_owc_site <- cbind(hf_data_owc, siteDataOWC) 
  

# Event prediction -------------------------------------------------
  
  oct272019seiche <- hf_data_owc_site[hf_data_owc$datetime_round > "2019-10-31 14:00" & 
                                        hf_data_owc$datetime_round < "2019-11-1 14:00",]
  
  irene <- hf_data_cbv_site[hf_data_cbv_site$datetime_round > "2011-8-27 12:00" & 
                              hf_data_cbv$datetime_round < "2011-8-30 17:00", ]
  
  

# Create Meta data charts -------------------------------------------------

  seicheMetaplot <- createMetaPlots(oct272019seiche)
  
  ireneMetaplot <- createMetaPlots(irene)

# Time Series -------------------------------------------------------------
  
  #Seiche
  
    #Create concentration time series charts
    seicheConcentrations <- createPlots(oct272019seiche, 
                                          oct272019seiche$datetime_round[which.max(oct272019seiche$q_cfs.mean.5)])
  
    #layout plots
    ggarrange(plotlist = seicheConcentrations, 
              ggarrange(seicheMetaplot$depth, 
                        seicheMetaplot$conductivity, 
                        nrow=2, ncol=1), 
              nrow=1,ncol=5)  
  
  
    
    
  #Hurricane
  
    #Create time series plots
    hurricaneConcentrations <- createPlots(irene, 
                                          irene$datetime_round[which.max(irene$q_cfs.mean.5)])
  
    #just nitrate
    ggarrange(ireneMetaplot$depth, 
              ireneMetaplot$conductivity, 
              hurricaneConcentrations$nitrate,
              nrow=3, ncol=1)
    
  #Combine
    
    #just nitrate
    ggarrange(ireneMetaplot$depth +
                ylab(" ")+
                scale_x_datetime(date_labels = "%b %d %H:%M")+
                scale_color_manual(values=c('#6A8395',
                                            '#0680F9',
                                            '#FE8802',
                                            '#9A8565'))+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()),
              seicheMetaplot$depth+
                scale_color_manual(values=c('#FE8802',
                                            '#9A8565',
                                            '#6A8395',
                                            '#0680F9'))+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())+
                ylab(" "),
              ireneMetaplot$conductivity +
                ylab(" ")+
                scale_x_datetime(date_labels = "%b %d %H:%M")+
                scale_color_manual(values=c('#6A8395',
                                            '#0680F9',
                                            '#FE8802',
                                            '#9A8565'))+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()),
              seicheMetaplot$conductivity+
                scale_color_manual(values=c('#FE8802',
                                            '#9A8565',
                                            '#6A8395',
                                            '#0680F9'))+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())+
                ylab(" "),
              hurricaneConcentrations$nitrate +
                ylab(" ")+
                scale_x_datetime(date_labels = "%b %d %H:%M")+
                scale_color_manual(values=c('#6A8395',
                                            '#0680F9',
                                            '#FE8802',
                                            '#9A8565'))+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank()),
              seicheConcentrations$nitrate+
                scale_color_manual(values=c('#FE8802',
                                            '#9A8565',
                                            '#6A8395',
                                            '#0680F9'))+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())+
                ylab(" "),
              nrow=3, ncol=2)
  
# Winds -------------------------------------------------------------------

  #Seiche
    
    getNOAAData <- function(link, start, end){
      #Get Wind data from Marblehead
      NOAAweather <- read_csv(link) %>% 
        rename(Date = 1, 
               Time = 2,
               WindSpd = 3, 
               WindDir = 4,
               WindGust = 5,
               Bar = 6) %>% 
        mutate(WindSpd = as.numeric(WindSpd),
               WindDir = as.numeric(WindDir),
               WindGust = as.numeric(WindGust),
               Bar = as.numeric(Bar))
      
      
      #Combine and reformate data and time
      NOAAweather$date_time <- as.POSIXct(as.character(paste(NOAAweather$Date, NOAAweather$Time)), format="%Y-%m-%d %H:%M:%S")
      
      #subset data to storm event
      NOAAweather <- NOAAweather[NOAAweather$date_time > start & 
                                   NOAAweather$date_time < end,] 
      
      #Create plot
      WindSpd <- ggplot(NOAAweather) +
        geom_segment(aes(x = date_time,
                         y = 0,
                         xend = date_time + lubridate::dhours(WindSpd * 1 * -cos((90-WindDir) / 360 * 2 * pi)),
                         yend = WindSpd * 1 * -sin((90-WindDir) / 360 * 2 * pi),
                         col = Bar
        ),
        arrow = arrow(length = unit(0.1, "cm")) ) +
        geom_point(aes(date_time, 0), size = 1) +
        coord_fixed(3600) +
        ylab("Wind speed (kn)")+
        xlab("Time")+
        scale_color_gradient(low="blue", high="orange")+
        theme(panel.background = element_rect(fill = "white"))+
        geom_line(stat="identity", aes(x=date_time, y=normalize(WindGust)*10), color = "#ff0900")+
        geom_vline(xintercept = oct272019seiche$datetime_round[which.max(oct272019seiche$q_cfs.mean.5)], 
                   color = "black")
    }
  
  
  
  
  seicheWindPlot <- getNOAAData("data_NERR/NOAA/weatherOWC.csv", 
                                "2019-10-30 20:00", 
                                "2019-11-1 12:00")
  
  
  
  ireneWindPlot <- getNOAAData("data_NERR/NOAA/weatherCBVIrene.csv", 
                               "2011-8-27 12:00", 
                               "2011-8-30 17:00")
  
  

# Seiches -----------------------------------------------------------------

  
  
  sandy <- hf_data_cbv_site[hf_data_cbv_site$datetime_round > "2012-10-31 9:00" & 
                              hf_data_cbv$datetime_round < "2012-11-3 9:00" &
                              hf_data_cbv_site$site != "cb", ]
  
  
  
  ggplot(sandy, aes(x=datetime_round, y=q_cfs.mean.5, color=site))+geom_bar(stat="identity")

# Concentration vs Seiche -------------------------------------------------

  #ggplot(oct272019seiche, aes(y = q_cfs.mean.5, x = datetime_round))+
    #geom_line()
  
  seichePlots <- hysteresisPlots(oct272019seiche)
  ggarrange(
    ggarrange(plotlist = seichePlots[c(1:4)], nrow = 4),
    ggarrange(plotlist = seichePlots[c(5:8)], nrow = 4),
    ggarrange(plotlist = seichePlots[c(9:12)], nrow = 4),
    ggarrange(plotlist = seichePlots[c(13:16)], nrow = 4),
    ggarrange(plotlist = seichePlots[c(17:20)], nrow = 4),
    ncol = 5,nrow = 1)
  
    #Just Nitrate
    ggarrange(plotlist = seichePlots[c(9:12)], nrow = 4)
  
  #Irene
  irenePlots <- hysteresisPlots(irene)
  ggarrange(
  ggarrange(plotlist = irenePlots[c(3,4,1,2)], nrow = 4),
  ggarrange(plotlist = irenePlots[c(7,8,5,6)], nrow = 4),
  ggarrange(plotlist = irenePlots[c(11,12,9,10)], nrow = 4),
  ggarrange(plotlist = irenePlots[c(15,16,13,14)], nrow = 4),
  ggarrange(plotlist = irenePlots[c(19,20,17,18)], nrow = 4),
  ncol = 5,nrow = 1)
  
    #Just Nitrate
    ggarrange(irenePlots[[11]]+
                xlab(" ")+
                ylab(" "),
              seichePlots[[9]]+
                xlab(" ")+
                ylab(" "),
              irenePlots[[12]]+
                xlab(" ")+
                ylab(" "),
              seichePlots[[10]]+
                xlab(" ")+
                ylab(" "),
              irenePlots[[9]]+
                xlab(" ")+
                ylab(" "), 
              seichePlots[[11]]+
                xlab(" ")+
                ylab(" "),
              irenePlots[[10]]+
                xlab(" ")+
                ylab(" "), 
              seichePlots[[12]]+
                xlab(" ")+
                ylab(" ")
              nrow = 4, ncol=2)
  
  #Sandy
  sandyPlots <- hysteresisPlots(sandy)
  ggarrange(
    ggarrange(plotlist = sandyPlots[c(2,3,1)], nrow = 3),
    ggarrange(plotlist = sandyPlots[c(5,6,4)], nrow = 3),
    ggarrange(plotlist = sandyPlots[c(8,9,7)], nrow = 3),
    ggarrange(plotlist = sandyPlots[c(11,12,10)], nrow = 3),
    ggarrange(plotlist = sandyPlots[c(14,15,13)], nrow = 3),
    ncol = 5,nrow = 1)

  

