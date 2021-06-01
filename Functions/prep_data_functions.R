## ---------------------------
##
## Script name: prep_data_functions
##
## Purpose of script: A group of functions used to prep data
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

##################################################
##### FUNCTIONS
##################################################

# OLD: Read in csvs and bind to a single dataframe by common substring
read_data <- function(directory, type) {
  file_list <- list.files(directory, pattern="csv", full.names=T) %>%
    subset(str_detect(., type)==TRUE) %>% as.list()
  df_list <- lapply(file_list, read_csv)
  df <- do.call("rbind", df_list)
  return(df)
}

#NEW: Read in csv by folder
read_dir <- function(directory) {
  file_list <- list.files(directory, pattern="csv", full.names=T) %>% 
    map(read_csv)
  df <- do.call("rbind", file_list)
  return(df)
}



# Prepare a single nutrient parameter (gets called within process_nuts)
single_nutrient_prep <- function(raw_data, dep, flag.dep, bin_rate ="15 min", stationCode = 1) {
  
  #Format data with proper time and removal of NA
  df <- raw_data %>%
    mutate(datetime = (parsedate::parse_date(DateTimeStamp)),
           datetime_round = round_date(datetime, bin_rate),
           site = substr(.[[stationCode]], 4,5),
           datetime_site = paste(datetime_round, site),
           flag = as.numeric(str_match(.[[flag.dep]], "<\\s*(.*?)\\s*>")[,2]),
           flag %in% flag_list,
           dep = as.numeric(.[[dep]])) %>%
    select(datetime_round, datetime_site, site, flag, dep) %>% 
    drop_na() %>% 
    group_by(datetime_site) %>% 
    summarize(datetime_round = first(datetime_round),
              site = first(site),
              flag = mean(flag), 
              dep_mean = mean(dep)) %>% 
    dplyr::rename(!!dep:=dep_mean)
  
  #Print out the information regarding unique values and QAQC
  cat(dep, ":\n", 
      "Ratio of data passing QAQC:",round(nrow(df) / nrow(raw_data), 2), "\n",
      length(unique(df$datetime_site)), "datetime_site values", "of", nrow(df), "are unique\n")
  
  return(df)
}

# Process each nutrient in turn (modular for flexibility to add/remove parameters)
# then join (full_join instead of inner_join to get maximum sample size)
process_nuts <- function(raw_data, bin_rate = "15 min") {
 
  chla <- single_nutrient_prep(raw_data, "CHLA_N", "F_CHLA_N", bin_rate)
  nh4 <- single_nutrient_prep(raw_data, "NH4F", "F_NH4F", bin_rate)
  no3 <- single_nutrient_prep(raw_data, "NO3F", "F_NO3F", bin_rate)
  po4 <- single_nutrient_prep(raw_data, "PO4F", "F_PO4F", bin_rate)
  
  x <- full_join(chla %>% select(-flag), 
                 nh4 %>% select(-datetime_round, -site, -flag), by="datetime_site") %>% 
    full_join(., no3 %>% select(-datetime_round, -site,-flag), by="datetime_site") %>% 
    full_join(., po4 %>% select(-datetime_round, -site,-flag), by="datetime_site")
  
  colnames(x) <- c("datetime_site", "datetime_round", "site","chla", "nh4", "no3", "po4")
  
  return(x)
}

# Process water quality data
prep_wq <- function(df) {
  df %>% 
    mutate(datetime = parsedate::parse_date(DateTimeStamp), 
           datetime_round = round_date(datetime, bin_rate), 
           site = substr(StationCode, 4,5), 
           datetime_site = paste(datetime_round, site), 
           F_Temp = as.numeric(str_match(F_Temp, "<\\s*(.*?)\\s*>")[,2]), 
           F_SpCond = as.numeric(str_match(F_SpCond, "<\\s*(.*?)\\s*>")[,2]), 
           F_DO_mgl = as.numeric(str_match(F_DO_mgl, "<\\s*(.*?)\\s*>")[,2]), 
           F_Depth = as.numeric(str_match(F_Depth, "<\\s*(.*?)\\s*>")[,2]), 
           F_pH = as.numeric(str_match(F_pH, "<\\s*(.*?)\\s*>")[,2]), 
           F_Turb = as.numeric(str_match(F_Turb, "<\\s*(.*?)\\s*>")[,2])) %>% 
    filter(F_Temp %in% flag_list, 
           F_SpCond %in% flag_list, 
           F_DO_mgl %in% flag_list, 
           F_Depth %in% flag_list, 
           F_pH %in% flag_list, 
           F_Turb %in% flag_list) %>%
    mutate(Temp = as.numeric(Temp), 
           SpCond = as.numeric(SpCond), 
           DO_mgl = as.numeric(DO_mgl), 
           Depth = as.numeric(Depth), 
           pH = as.numeric(pH), 
           Turb = as.numeric(Turb), 
           Turb = ifelse(Turb < 0, 0, Turb)) %>%
    select(datetime_round, site, datetime_site, Temp, SpCond, DO_mgl, Depth, pH, Turb)
}

# Bin water quality data to remove duplicate datetime stamps
bin_wq <- function(df) { 
  
  df_dup <- df %>% 
    group_by(datetime_site) %>% 
    filter(n() > 1) %>% 
    summarize(datetime_round = first(datetime_round), 
              site = first(site), 
              Temp = mean(Temp),
              SpCond = mean(SpCond),
              DO_mgl = mean(DO_mgl),
              Depth = mean(Depth),
              pH = mean(pH),
              Turb = mean(Turb))
  
  df_unique <- df %>% 
    group_by(datetime_site) %>% 
    filter(n() == 1)
  
  x <- bind_rows(df_dup, df_unique) 
  
  return(x)
}

# Process met data
prep_met <- function(df) {
  
  n0 <- nrow(df)
  
  x <- df %>% 
    mutate(datetime = (parsedate::parse_date(DatetimeStamp)), 
           datetime_round = round_date(datetime, bin_rate), 
           F_ATemp = as.numeric(str_match(F_ATemp, "<\\s*(.*?)\\s*>")[,2]), 
           F_RH = as.numeric(str_match(F_RH, "<\\s*(.*?)\\s*>")[,2]), 
           F_BP = as.numeric(str_match(F_BP, "<\\s*(.*?)\\s*>")[,2]), 
           F_Wdir = as.numeric(str_match(F_Wdir, "<\\s*(.*?)\\s*>")[,2]), 
           F_WSpd = as.numeric(str_match(F_WSpd, "<\\s*(.*?)\\s*>")[,2]), 
           F_TotPAR = as.numeric(str_match(F_TotPAR, "<\\s*(.*?)\\s*>")[,2]), 
           F_TotPrcp = as.numeric(str_match(F_TotPrcp, "<\\s*(.*?)\\s*>")[,2])) %>% 
    filter(F_ATemp %in% flag_list, 
           F_RH %in% flag_list, 
           F_BP %in% flag_list, 
           F_Wdir %in% flag_list, 
           F_WSpd %in% flag_list, 
           F_TotPAR %in% flag_list, 
           F_TotPrcp %in% flag_list) %>% 
    mutate(ATemp = as.numeric(ATemp), 
           RH = as.numeric(RH), 
           BP = as.numeric(BP), 
           WSpd = as.numeric(WSpd), 
           Wdir = as.numeric(Wdir), 
           TotPAR = as.numeric(TotPAR), 
           TotPrcp = as.numeric(TotPrcp)) %>% 
    select(datetime_round, ATemp, RH, BP, WSpd, Wdir, TotPAR, TotPrcp) 
  
  cat("Ratio of data passing QAQC:",round(nrow(x) / n0, 2), "\n",
      length(unique(x$datetime_round)), "datetime_round values", "of", nrow(x), "are unique\n")
  return(x)
}

# Bin met data to remove duplicate datetime stamps
bin_met <- function(df) { 
  df_dup <- df %>% 
    group_by(datetime_round) %>% 
    filter(n() > 1) %>% 
    summarize(ATemp = mean(ATemp),
              RH = mean(RH), 
              BP = mean(BP),
              WSpd = mean(WSpd),
              Wdir = mean(Wdir),
              TotPAR = mean(TotPAR),
              TotPrcp = mean(TotPrcp))
  
  df_unique <- df %>% 
    group_by(datetime_round) %>% 
    filter(n() == 1)
  
  x <- bind_rows(df_dup, df_unique) 
  
  print(paste(nrow(df) - nrow(x), "duplicate rows removed"))
  return(df)
}

# Read in USGS discharge (q) data
read_q <- function(site) {
  x <- dataRetrieval::readNWISdata(siteNumbers = site, 
                                   parameterCd = parCd, 
                                   startDate = start_Q,
                                   endDate = end_Q,
                                   service = "iv") %>% 
    as_tibble() %>% rename(q_cfs = X_00060_00000) %>% 
    select(dateTime, q_cfs)
}

# Reformat tibbles to create datetime ranges. This seems more complicated than
# needed, but is based on the idea that nutrients at a given point in time are 
# driven by antecedent processes (here represented by the previous 24 hours except q),
# so we can't just match time-stamps (i.e. instantaneous conditions), but need to
# calculate statistics for the high frequency data based on a date range for each
# row in the nutrient dataframe. This function sets up lists to feed into the
# calc_stats_. functions below, since that's the format required by pmap()
prep_vectors <- function(df, days_to_lag) {
  
  x <- c(df$datetime_round)
  y <- c(df$datetime_round - days(days_to_lag))
  z <- c(df$site)
  l <- list(x=x, y=y, z=z)
  
  return(l)
}

# helper stat functions for calc_stats. functions
min_    <- function(...) min(..., na.rm=T)
mean_   <- function(...) mean(..., na.rm=T)
max_    <- function(...) max(..., na.rm=T)
sum_    <- function(...) sum(..., na.rm=T)

# Calculate statistics for high frequency parameters, and bind those statistics
# as new columns of the nutrient dataframes. I really don't want to hardwire 
# specific dataframe names, but haven't found a better method, so there are 
# two functions that are NERR site-specific (e.g., don't want to call "hf_cbv"). 
# These functions have lots of room for improvement!
calc_stats_cbv <- function(x, y, z) {
  
  # Splitting into two outputs to deal with non-stat columns
  output1 <- hf_cbv %>% filter(datetime_round >= y & datetime_round <= x) %>% 
    filter(site == z) %>% # subset that to only relevant site
    mutate(datetime_og = x) %>% 
    summarize(across(c(datetime_og, site), first)) # report the first value of a group
  
  output2 <- hf_cbv %>% filter(datetime_round >= y & datetime_round <= x) %>% 
    filter(site == z) %>% # subset that to only relevant site
    summarise(across(all_of(all_vars), c(min=min_, mean=mean_, max=max_), .names = "{.col}.{.fn}"))
  
  # Here, 5 indicates a lag of 5 days, the idea is to be as flexible as possible here
  output5 <- hf_cbv %>% filter(datetime_round >= 5 & datetime_round <= x) %>% 
    filter(site == z) %>% 
    summarize(across(c(TotPrcp, q_cfs), c(min=min_, mean=mean_, max=max_, sum=sum_), .names = "{.col}.{.fn}.5"))
  
  output <- bind_cols(output1, output2, output5)
  
  return(output)
}

calc_stats_owc <- function(x, y, z) {
  
  # Splitting into two outputs to deal with non-stat columns
  output1 <- hf_owc %>% filter(datetime_round >= y & datetime_round <= x) %>% 
    filter(site == z) %>% # subset that to only relevant site
    mutate(datetime_og = x) %>% 
    summarize(across(c(datetime_og, site), first))
  
  output2 <- hf_owc %>% filter(datetime_round >= y & datetime_round <= x) %>% 
    filter(site == z) %>% # subset that to only relevant site
    summarise(across(all_of(all_vars), c(min=min_, mean=mean_, max=max_), .names = "{.col}.{.fn}"))
  
  output5 <- hf_owc %>% filter(datetime_round >= 5 & datetime_round <= x) %>% 
    filter(site == z) %>% 
    summarize(across(c(TotPrcp, q_cfs), c(min=min_, mean=mean_, max=max_, sum=sum_), .names = "{.col}.{.fn}.5"))
  
  output <- bind_cols(output1, output2, output5)
  
  return(output)
}

#Lock variables from being edited within future R scripts
#lapply(ls(), lockBinding, env = globalenv())