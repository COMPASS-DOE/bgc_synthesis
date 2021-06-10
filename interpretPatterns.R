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

getSummary <- function(x){
  summaryByDM <- read_csv() %>% 
    mutate(hour = format(datetime_round, "%h"), month = format(datetime_round, "%m")) %>% 
    group_by(hour) %>% 
    summarise(nh4.hour.mean = mean(nh4), 
              no3.hour.mean = mean(no3), 
              po4.hour.mean = mean(po4), 
              chla.hour.mean = mean(chla))
  return(summaryByDM)
}



hf_data_cbv_summary <- getSummary("data_NERR/output/cbv_hf_wq_predictions.csv")
  
hf_data_owc_summary <- getSummary("data_NERR/output/owc_hf_wq_predictions.csv")














