
## ---------------------------
##
## Script name: map_script
##
## Purpose of script: Create maps of the sites
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-06-24
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------
##
## Notes:
##   
## Create maps of sites
##

# 1. setup ----------------------------------------------------------------

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
         sf, 
         spData, 
         ggmap)
  
  source("Constants/prep_data_constants.R")
  
  #hf data
  hf_cbv <- read.csv(paste0(filepath_out, "/cbv_hf_wq.csv"))
  hf_owc <- read.csv(paste0(filepath_out, "/owc_hf_wq.csv"))
  
  #site locations
  cbv <- read_sf("water_stations/GE_cbv_ST/cbv.kml") %>% 
    mutate(lat = unlist(map(geometry,1)),
           long = unlist(map(geometry,2)))
  owc <- read_sf("water_stations/GE_owc_ST/owc.kml") %>% 
    mutate(lat = unlist(map(geometry,1)),
           long = unlist(map(geometry,2)))
  
  #map of united states
  data("us_states")
  us_states_2163 = st_transform(us_states, crs = 2163)

# 2. Find Conductivity of each site ---------------------------------------

  #cbv location
  hf_cbv_na <- na.omit(hf_cbv)
  mean_spCond_cbv <- tapply(hf_cbv_na$SpCond, na.omit(hf_cbv_na$site) , mean)  
  
  #owc location
  hf_owc_na <- na.omit(hf_owc)
  mean_spCond_owc <- tapply( hf_owc_na$SpCond, na.omit(hf_owc_na$site) , mean)
  
  
# York Town River Map -----------------------------------------------------

  #Repeat conductivity measurements for each site
  ConductivityCBV <- as.factor(c(rep(mean_spCond_cbv[1], 2), 
                                 rep(mean_spCond_cbv[2],2), 
                                 rep(mean_spCond_cbv[3], 4), 
                                 rep(mean_spCond_cbv[5],3)))
  
  #Label sites by location
  siteName <- as.factor(c(rep("Claybank", 2), 
                          rep("Goodwin Islands",2), 
                          rep("Sweethall",4), 
                          rep("Taskinas Creek", 3)))



  #bind mean conductivity and names
  cbv <- cbv[c(-3,-13),]%>% 
    cbind(ConductivityCBV, siteName) %>% 
    mutate(ConductivityCBV = as.numeric(as.character(ConductivityCBV)))

  #Define map area
  uscbv <- c(left = -77, bottom = 37.1, right = -76.25, top = 37.7)
  
  #Create a stamen map
  cbv_map<- get_stamenmap(uscbv, zoom = 14, maptype = "terrain")
  
  ggmap(cbv_map, base_layer=ggplot(data=cbv, aes(x=lat, y=long, color = ConductivityCBV))) +
    geom_point()+
    geom_text(aes(label=siteName), color = "black", size = 3, vjust = 1)+
    theme(axis.text.x = element_text(angle = 90))+
    scale_color_gradient(low="blue", high="red")


# York Town Location Map --------------------------------------------------
  
  #box in area of york town river
  cbv_bb <- st_as_sfc(st_bbox(cbv))
  
  #create map
  ggcbv = ggplot() + 
    geom_sf(data = us_states_2163, fill = "white")+ 
    geom_sf(data = cbv_bb, fill = NA, color = "red", size = 1.2)+
    theme_void()

  ggcbv

# Old Woman Ceek Map ------------------------------------------------------
  #Repeat conductivity measurements for each site
  ConductivityOWC <- as.factor(c(rep(mean_spCond_owc[1], 2), 
                                  rep(mean_spCond_owc[2],2), 
                                  rep(mean_spCond_owc[3], 2), 
                                  rep(mean_spCond_owc[4],2), 
                                  rep(mean_spCond_owc[5],2)))
  
  #Repeat site names for same locations
  siteName <- as.factor(c(rep("Berlin Road", 2), 
                          rep("Darrow Road",2), 
                          rep("Lower Estuary", 2), 
                          rep("State Route 2",2), 
                          rep("State Route 6",2)))

  #bind conductivity and site name information with locations
  owc <- owc[-7,]%>% 
    cbind(ConductivityOWC, siteName) %>% 
    mutate(ConductivityOWC = as.numeric(as.character(ConductivityOWC)))


  #Define map area for owc
  usowc <- c(left = -82.52, bottom = 41.345, right = -82.5, top = 41.39)
  
  #Create a stamen map
  owc_map<-get_stamenmap(usowc, zoom = 14, maptype = "terrain") 
  
  ggmap(owc_map, base_layer = ggplot(data=owc, aes(x=lat, y=long, color = ConductivityOWC))) + geom_point()+
    geom_text(aes(label=siteName), color = "black", size = 3, vjust = 1)+
    theme(axis.text.x = element_text(angle = 90))+
    scale_color_gradient(low="blue", high="red")


# Old Woman Creek Location Map --------------------------------------------
  
  #location of OWC
  owc_bb <- st_as_sfc(st_bbox(owc))
  
  #create map
  ggowc = ggplot() + 
    geom_sf(data = us_states_2163, fill = "white")+ 
    geom_sf(data = owc_bb, fill = NA, color = "red", size = 4)+
    theme_void()
  
  #may not show given size of site
  ggowc
  