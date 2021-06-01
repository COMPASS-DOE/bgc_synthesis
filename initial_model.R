# ---------------------------

# Purpose: Initial testing of models: what set of parameters works best?

# This script helps us decide what set of parameters is most useful for predicting
# nutrients/chla, and which package to use. This same workflow should be useful for 
# tuning models and other optimization. 

# Next steps: 
## integrate iRF into model selection loop
## select ranger v randomForest v iRF; tune mtry, ntrees, etc
## streamline / bulletproof the model creation workflow
## once those are done, bonus would be starting on 

# Outputs
## figure comparing different predictor inputs (see plot call below)
## figure comparing performance of randomForest, ranger, and iRF::randomForest

# I think we'll want to use iRF at some point regardless of how it compares to
# rF and ranger because it returns interactions. Ideal is formatting output of
# script as Rmd

# ---------------------------

# clean workspace
rm(list = ls())

# load packages
require(pacman)
p_load(tidyverse, ggplot2, cowplot, lubridate, tidymodels)

# Read in data
# Filter as is.na | threshold, otherwise you lose a lot of extra rows
cbv_all <- read_csv("./data/nerr/cbv_for_models.csv") %>% 
  mutate(date = lubridate::date(datetime_round)) %>% 
  mutate(doy = lubridate::yday(date)) %>% 
  mutate(sin_doy = sin(doy / (365.25 * pi))) %>% # additional predictor
  select(-doy, -datetime_round, -date) %>%
  select_if(is.double) %>% 
  filter(is.na(no3) | no3 < 1) %>% #drop outliers
  filter(is.na(po4) | po4 < 0.15) %>%
  filter(is.na(chla) | chla < 200)

owc_all <- read_csv("./data/nerr/owc_for_models.csv") %>% 
  mutate(date = lubridate::date(datetime_round)) %>% 
  mutate(doy = lubridate::yday(date)) %>% 
  mutate(sin_doy = sin(doy / (365.25 * pi))) %>% 
  select(-doy, -datetime_round, -date) %>%
  select_if(is.double) %>% 
  filter(is.na(no3) | no3 < 8) %>% 
  filter(is.na(po4) | po4 < 0.1)

# Set columns for independent and dependent variables
dependents <- c("chla", "nh4", "no3", "po4")
wq_predictors <- c("Temp.mean", "SpCond.mean", "DO_mgl.mean", "Depth.mean", "pH.mean", "Turb.mean", "q_cfs.mean.5", "sin_doy")
met_predictors <- c("ATemp.mean", "RH.mean", "BP.mean", "WSpd.mean", "Wdir.mean", "TotPAR.mean", "TotPrcp.mean.5", "sin_doy")
all_predictors <- unique(append(wq_predictors, met_predictors))

# Create model, and return useful things (data, plots, stats)
choose_inputs <- function(df, dep, predictors) {
  
  source = substr(deparse(substitute(df)), 1, 3)
  type = sub("_predictors", "", deparse(substitute(predictors)))
  
  x <- bind_cols(df %>% select(dep), df %>% select(predictors)) %>% 
    drop_na() %>% 
    rename("actual" = dep)
  
  set.seed(42)
  df_split <- initial_split(x)
  df_train <- training(df_split)
  df_test <- testing(df_split)
  
  f <- as.formula(paste(names(df_train)[1], "~", paste(names(df_train)[-1], collapse=" + ")))
  
  rf_defaults <- rand_forest(mode = "regression", trees = 1000)
  
  set.seed(42)
  m1 <- rf_defaults %>%
    set_engine("ranger", importance = "impurity") %>%
    fit(formula = f, data=df_train)
  
  df_modeled <- df_test %>% 
    mutate(predict(m1, df_test))
  
  plot1 <- ggplot(df_modeled, aes(actual, .pred)) + 
    geom_point() + geom_smooth(method="lm", se=F) + 
    geom_abline(slope = 1, intercept = 0)
  
  r2 <- summary(lm(actual ~ .pred, df_modeled))[[9]]

  output_tibble <- tibble(r2 = r2, 
                          type = type, 
                          dep = dep, 
                          source = source)
  
  l <- list(data = df_modeled, plot = plot1, output = output_tibble)
  
return(l)

}


# This could easily be streamlined with a simple wrapper function
cbv <- bind_rows(choose_inputs(cbv_all, "no3", wq_predictors)$output, 
                 choose_inputs(cbv_all, "no3", met_predictors)$output, 
                 choose_inputs(cbv_all, "no3", all_predictors)$output, 
                 choose_inputs(cbv_all, "po4", wq_predictors)$output, 
                 choose_inputs(cbv_all, "po4", met_predictors)$output, 
                 choose_inputs(cbv_all, "po4", all_predictors)$output, 
                 choose_inputs(cbv_all, "nh4", wq_predictors)$output, 
                 choose_inputs(cbv_all, "nh4", met_predictors)$output, 
                 choose_inputs(cbv_all, "nh4", all_predictors)$output, 
                 choose_inputs(cbv_all, "chla", wq_predictors)$output, 
                 choose_inputs(cbv_all, "chla", met_predictors)$output, 
                 choose_inputs(cbv_all, "chla", all_predictors)$output)

owc <- bind_rows(choose_inputs(owc_all, "no3", wq_predictors)$output, 
                 choose_inputs(owc_all, "no3", met_predictors)$output, 
                 choose_inputs(owc_all, "no3", all_predictors)$output, 
                 choose_inputs(owc_all, "po4", wq_predictors)$output, 
                 choose_inputs(owc_all, "po4", met_predictors)$output, 
                 choose_inputs(owc_all, "po4", all_predictors)$output, 
                 choose_inputs(owc_all, "nh4", wq_predictors)$output, 
                 choose_inputs(owc_all, "nh4", met_predictors)$output, 
                 choose_inputs(owc_all, "nh4", all_predictors)$output, 
                 choose_inputs(owc_all, "chla", wq_predictors)$output, 
                 choose_inputs(owc_all, "chla", met_predictors)$output, 
                 choose_inputs(owc_all, "chla", all_predictors)$output)

# Plot comparing fits across different predictor sets
bind_rows(cbv, owc) %>% ggplot(., aes(dep, r2, fill = type)) + 
  geom_col(position = "dodge") + facet_wrap(vars(source))
ggsave("./graphs/210528_compare_predictor_datasets.png", width=8, height=4)

# summary stats by source and parameter inputs
bind_rows(cbv, owc) %>% group_by(source, type) %>% 
  summarize(r2 = mean(r2))

# Example of model comparison (feel free to improve!). We need to figure out
# how to get iRF to play with tidymodels
compare_models <- function(df, dep, predictors) {
  
  source = substr(deparse(substitute(df)), 1, 3)
  type = sub("_predictors", "", deparse(substitute(predictors)))
  
  x <- bind_cols(df %>% select(dep), df %>% select(predictors)) %>% 
    drop_na() %>% 
    rename("actual" = dep)
  
  set.seed(42)
  df_split <- initial_split(x)
  df_train <- training(df_split)
  df_test <- testing(df_split)
  
  f <- as.formula(paste(names(df_train)[1], "~", paste(names(df_train)[-1], collapse=" + ")))
  
  rf_defaults <- rand_forest(mode = "regression", trees = 1000)
  
  set.seed(42)
  m_ranger <- rf_defaults %>%
    set_engine("ranger", importance = "impurity") %>%
    fit(formula = f, data=df_train)
  
  set.seed(42)
  m_rf <- rf_defaults %>%
    set_engine("randomForest") %>%
    fit(formula = f, data=df_train)
  
  l <- list(ranger = m_ranger, rF = m_rf)
  
  return(l)
  
}




