## ---------------------------
##
## Script name: prep_model_data.R
##
## Purpose of script: Prepare data from NESS to be used in future scripts
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-05-28
##
## Email: peter.regier@pnnl.gov
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# This script takes all the raw data files downloaded from NERR and combines them
# into two datasets, one for each NERR site, which are used for modeling. 

# General steps
# 1. read in data by site and type
# 2. process those data (clean up based on flags, bin to appropriate intervals)
# 3. merge together with emphasis on maintaining highest sample size

# QC is based on NERR's flagging approach:  https://cdmo.baruch.sc.edu/data/qaqc.cfm

# Data types
## nutrients/chlorophyll = nuts
## water quality = wq
## meteorology / climate = met

## Data are labeled as CBV (Chesapeake Bay site) or OWC (Lake Erie)

# Created 5/27/21 by PR

##################################################
##### SETUP
##################################################

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
               dataRetrieval) # USGS's package to pull data from their portal


#Predefined constant variables in preparation of data
source("./Constants/prep_data_constants.R")

#Predefined functions used in preparation of data
source("./Functions/prep_data_functions.R")



##################################################
##### READ IN DATA
##################################################

tic("run script") #measure how long the script takes

# Read in pre-processed nutrients
cbv_nuts <- read_dir(paste0(cbv_directory, "/nutrients")) %>% 
  process_nuts(bin_rate = bin_rate)

owc_nuts <- read_dir(paste0(owc_directory, "/nutrients")) %>% 
  process_nuts(bin_rate = bin_rate)

toc()
## TODO Add site column (datetime_site is deprecated now)


# Read in water quality, then format
tic("prep wq data")

cbv_wq <- read_dir(paste0(cbv_directory, "/water_quality")) %>% 
  prep_wq() %>% bin_wq()

owc_wq <- read_dir(paste0(owc_directory, "/water_quality")) %>% 
  prep_wq() %>% bin_wq()

toc()

# read in meteorology, then format
tic("prep met data")

cbv_met <- read_dir(paste0(cbv_directory, "/meteorology")) %>% 
  prep_met() %>% bin_met()

owc_met <- read_dir(paste0(owc_directory, "/meteorology")) %>% 
  prep_met() %>% bin_met()

toc()

# read in USGS Q data
tic("prep q data")

cbv_q <- read_q(cbv_site)
owc_q <- read_q(owc_site)

toc()


##################################################
##### COMBINE AND EXPORT
##################################################

# Return combined high frequency data
hf_cbv <- left_join(cbv_wq, cbv_met, by="datetime_round") %>% 
  left_join(., cbv_q, by = c("datetime_round" = "dateTime")) %>% 
  select(-datetime_site)

hf_owc <- left_join(owc_wq, owc_met, by="datetime_round") %>% 
  left_join(., owc_q, by = c("datetime_round" = "dateTime")) %>% 
  select(-datetime_site)

# set up parallel
future::plan(multiprocess)

# use calc_stats. functions to combine high frequency data with nutrients
tic("bin owc data")
cbv_combined <- prep_vectors(cbv_nuts, 1) %>% 
  future_pmap(., calc_stats_cbv) %>% 
  bind_rows(.) %>% 
  full_join(cbv_nuts, ., by=c("datetime_round" = "datetime_og", "site" = "site")) 
toc()
beep()

tic("bin owc data")
owc_combined <- prep_vectors(owc_nuts, 1) %>% 
  future_pmap(., calc_stats_owc) %>% 
  bind_rows(.) %>% 
  full_join(owc_nuts, ., by=c("datetime_round" = "datetime_og", "site" = "site")) 
toc()
beep()

toc() #full script timer (1370s)

# Write out data for models
write_csv(cbv_combined %>% filter(!is.na(datetime_round)), paste0(filepath_out, "/cbv_for_models.csv"))
write_csv(owc_combined %>% filter(!is.na(datetime_round)), paste0(filepath_out, "/owc_for_models.csv"))

# Write out high-frequency data for use later
write_csv(hf_cbv %>% filter(datetime_round >= "2002-01-01"), paste0(filepath_out, "/cbv_hf_wq.csv"))
write_csv(hf_owc %>% filter(datetime_round >= "2002-01-01"), paste0(filepath_out, "/owc_hf_wq.csv"))




