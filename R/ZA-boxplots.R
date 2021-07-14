## ---------------------------
##
## Script name: boxplots
##
## Purpose of script: Create boxplots for each predictor
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-07-14
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# 1. Setup data -----------------------------------------------------------

  rm(list=ls()) # clear workspace
  
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
         kableExtra)

# 2. Read all data --------------------------------------------------------
  
  #read cbv data
  hf_data_cbv <- read_csv("data_NERR/output/cbv_hf_wq_predictions.csv") %>% 
                  add_column("CBV") %>% 
                  rename(site = ncol(.)) 
  
  #read owc data
  hf_data_owc <- read_csv("data_NERR/output/owc_hf_wq_predictions.csv") %>% 
                  add_column("OWC") %>% 
                  rename(site = ncol(.)) 
  
  #combine all data
  hf_data_all <- rbind(hf_data_cbv, hf_data_owc)%>% 
                  mutate(site = as.factor(site)) %>% 
                  na.omit()

# 3. Create plots ---------------------------------------------------------

  #create boxplot for each predicter
  boxplots <- lapply(colnames(hf_data_all)[-c((ncol(hf_data_all)-5):(ncol(hf_data_all)-1))], 
        function(val) ggplot(hf_data_all, aes(x = site, y = pull(hf_data_all[,val]), color = site)) + 
          geom_boxplot(notch=TRUE)+
          ylab(val)+ 
          stat_summary(fun=mean, geom="point", shape=23, size=4)+
          theme(legend.position = "none"))
  
  ggarrange(plotlist = boxplots[-length(boxplots)], ncol = 3, nrow = 3)

