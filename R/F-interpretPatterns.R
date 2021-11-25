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


# 1. Setup ----------------------------------------------------------------
  rm(list=ls()) # clear workspace
  #load packages
  require(pacman) # easy way to load packages
  pacman::p_load(hms, #extract time from datetime, 
                 lubridate, #work with dates
                 parsedate, #auto-parse dates without being told format
                 tidyverse, #data wrangling
                 tictoc, #timing things
                 furrr, #parallel for purrr
                 purrr, #tools to iterate rowwise through a dataframe (used for pmap)
                 beepr, # yell at me when you're done
                 dataRetrieval,
                 caret, 
                 ggpubr) # USGS's package to pull data from their portal
  source("Functions/interpret_patterns_functions.R")
  
  #read in prediction data
  CBVpred <- read_csv("data_NERR/output/cbv_hf_wq_predictions.csv")
  OWCpred <- read_csv("data_NERR/output/owc_hf_wq_predictions.csv")

# 2. Hourly mean of nutrient data for each year -----------------------------------------
  
  site_summaries <- list()
  
  site_summary_cbv <- getDiurnalByYear(CBVpred, "CBV", 2002:2020)
  site_summary_owc <- getDiurnalByYear(OWCpred, "OWC", 2002:2020)
  
  for(val in 1:(length(site_summary_cbv)-1)){
    
    site_summaries[[val]] <-try( rbind(site_summary_cbv[[val]], site_summary_owc[[val]]) %>%
                              plotNutrients(year = 2001+val))
    
  }
  ggarrange(plotlist = site_summaries, ncol=20)
  
  

# 3. Create coord plots for each month ------------------------------------

  hf_data_cbv_summaryAll <- getSummaryMonthH(CBVpred, "CBV",
                                             OWCpred, "OWC") %>% 
                              as.data.frame() %>% 
                              group_split(month)
  
  #Order by month of year not alphabetically
  hf_data_cbv_summaryAll<-hf_data_cbv_summaryAll[c(5, 4, 8, 1, 9, 7, 6, 2, 12, 11, 10, 3)]
  
  #Combine plots
  allPolarPlots <- lapply(hf_data_cbv_summaryAll, makePolarPlots)
  allPolarPlotsSign <- do.call(c, allPolarPlots)
  
  #Generate plots
  nh4PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(1, 48, 4))], nrow = 1, ncol=12)
  no3PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(2, 48, 4))], nrow = 1, ncol=12)
  po4PP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(3, 48, 4))], nrow = 1, ncol=12)
  chlaPP <- ggarrange(plotlist = allPolarPlotsSign[c(seq(4, 48, 4))], nrow = 1, ncol=12)
  


# 4. Create bar chart for mean nutrient level -----------------------------
  
  hf_data_cbv_summaryBP <- getSummaryMonth(CBVpred) %>% 
                            add_column("CBV") %>% 
                            rename(label = 6)  %>% 
    mutate(nh4.month.mean = scale(nh4.month.mean),
           no3.month.mean = scale(no3.month.mean),
           po4.month.mean = scale(po4.month.mean),
           chla.month.mean =scale(chla.month.mean)
    )
  
  hf_data_owc_summaryBP <- getSummaryMonth(OWCpred) %>% 
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


# 5. Combine coord and bar plots ------------------------------------------

  ggarrange(
  ggarrange(nh4PP, barnh4, nrow = 2, ncol = 1, heights = c(2,3)),
  
  ggarrange(no3PP, barno3, nrow = 2, ncol = 1, heights = c(2,3)),
  
  ggarrange(po4PP, barpo4, nrow = 2, ncol = 1, heights = c(2,3)),
    
  ggarrange(chlaPP, barchla, nrow = 2, ncol = 1, heights = c(2,3)),
  
  nrow=4, ncol =1)
