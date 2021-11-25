#######################################
#####Constants
#######################################

# Set columns for independent and dependent variables
dependents <- c("chla", "nh4", "no3", "po4")
wq_predictors <- c("Temp.mean", 
                   "SpCond.mean", 
                   "DO_mgl.mean", 
                   "Depth.mean", 
                   "pH.mean", 
                   "Turb.mean", 
                   "sin_doy")
met_predictors <- c("ATemp.mean", 
                    "RH.mean", 
                    "BP.mean", 
                    "WSpd.mean", 
                    "Wdir.mean", 
                    "TotPAR.mean", 
                    "TotPrcp.mean.5", 
                    "sin_doy")
all_predictors <- unique(append(wq_predictors, met_predictors))

predictors <- list(wq_predictors, met_predictors, all_predictors)

#indexes of wq 
wq_ind <- seq(2,11,3)
