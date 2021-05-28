## ---------------------------
##
## Script name: prep_data_constants.R
##
## Purpose of script: A group of constant variable names
##
## Author: Peter Regier and Matt Duggan
##
## Date Created: 2021-05-28
##
## Copyright (c) PNNL, 2021
## Email: mattduggan2018@gmail.com
##
## ---------------------------
##
## Notes: 
##   
##
## ---------------------------

#clear workspace
rm(list=ls())

# Predefined Global Variables
bin_rate = "15 min"
flag_list <- c(0, 4)
days_to_lag = 1

##  TODO Define scripts for reading in data ##
filepath_in = "./data/nerr/210225_csvs/"
filepath_out = "./data/nerr/"
##
##

# USGS data Reference Information
cbv_site = "02037500" # JAMES RIVER NEAR RICHMOND, VA
owc_site = "04199155" # Old Woman Creek at Berlin Rd near Huron OH
parCd = "00060" # code for discharge (q)
start_Q = "2002-01-01" # start date for data to be retrieved
end_Q = "2021-01-01" # end date for data to be retrieved

# Defined Data Columns
wq_vars = c("Temp", "SpCond", "DO_mgl", "Depth","pH", "Turb")
met_vars = c("ATemp", "RH", "BP", "WSpd", "Wdir", "TotPAR")
all_vars = append(wq_vars, met_vars)

#Lock variables from being edited within future R scripts
lapply(ls(), lockBinding, env = globalenv())

