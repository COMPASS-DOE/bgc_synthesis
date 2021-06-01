## ---------------------------
##
## Script name: prep_data_constants.R
##
## Purpose of script: A group of constants used in prep data
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

#clear workspace
#rm(list=ls())

# Predefined Global Variables
bin_rate = "15 min"
flag_list <- c(0, 4)
days_to_lag = 1

##  TODO Define scripts for reading in data ##

#Set directories to data input location
filepath_in = "data_NERR/"

#Set directories to data output location
filepath_out = "data_NERR/output"

# Set directories for each location
cbv_directory <- paste0(filepath_in, "cbv")
owc_directory <- paste0(filepath_in, "owc")
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
all_vars = c(wq_vars, met_vars)

#Lock variables from being edited within future R scripts
lapply(ls(), lockBinding, env = globalenv())

