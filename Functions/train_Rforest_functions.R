#######################################
#####Functions
#######################################


# Read in data
read_station <- function(dt){
  dt_all <- read_csv(dt) %>% 
    mutate(sin_doy = sin(yday(lubridate::date(datetime_round)) / (365.25 * pi))) %>%
    select(-datetime_round) %>% 
    select_if(is.double)
  return(dt_all)
}

splitData <- function(dt_clean, dep, pred, prop){
  set.seed(1234)
  
  dt_split <- initial_split(dt_clean, 
                            prop = prop)
  return(dt_split)
  
}

createRecipe <- function(dt_train, dt_clean){
  
  f <- as.formula(paste(names(dt_train)[1], "~", paste(names(dt_train)[-1], collapse=" + ")))
  # define the recipe
  dt_recipe <- 
    # which consists of the formula (outcome ~ predictors)
    recipe(f, 
           data = dt_clean) %>%
    # and some pre-processing steps
    step_normalize(all_numeric()) %>%
    step_knnimpute(all_predictors())
  
  return(dt_recipe)
}

specifyModel <- function(pkge, t, importance){
  if(t == -1){
    
    #Specify the Model
    rf_model <- 
      # specify that the model is a random forest
      rand_forest() %>%
      # specify that the `mtry` parameter needs to be tuned
      set_args(mtry = tune()) %>%
      # select the engine/package that underlies the model
      set_engine(pkge, importance = importance) %>%
      # choose either the continuous regression or binary classification mode
      set_mode("regression")
    
    } else {
      
    rf_model <- 
      # specify that the model is a random forest
      rand_forest() %>%
      # specify that the `mtry` parameter needs to be tuned
      set_args(mtry = t) %>%
      # select the engine/package that underlies the model
      set_engine(pkge, importance = importance) %>%
      # choose either the continuous regression or binary classification mode
      set_mode("regression")
      }
  
  return(rf_model)
}

createWorkflow <- function(dt_recipe, rf_model){
  
  # set the workflow
  rf_workflow <- workflow() %>%
    # add the recipe
    add_recipe(dt_recipe) %>%
    # add the model
    add_model(rf_model)
  
  return(rf_workflow)
}

tuneMtry <- function(rf_workflow, pred, dt_cv, t){
  if(t == -1){
    
    # specify which values want to try
    rf_grid <- expand.grid(mtry = c(1:(length(pred)-1)))
    # extract results
    rf_tune_results <- rf_workflow %>%
      tune_grid(resamples = dt_cv, #CV object
                grid = rf_grid, # grid of values to try
                metrics = metric_set(yardstick::mae))
    #Show the collected metrics
    mtry_vals <- rf_tune_results %>%
      collect_metrics()
    
    #Extract the best value
    param_final <- rf_tune_results %>%
      select_best(metric = "mae")
    
    rf_workflow <- rf_workflow %>%
      finalize_workflow(param_final)
    
  } else{
    
    rf_workflow <- rf_workflow
    
  }
  
  return(rf_workflow)
}

evaluateModel <- function(rf_fit, test_predictions){
  
  #Collect Test Performance
  all_metrics <- gof(sim=test_predictions$.pred, obs=test_predictions$actual)
  
  return(all_metrics)
}

showEval <- function(test_predictions, label){
  Fig <- test_predictions %>% 
    ggplot(aes(1:nrow(test_predictions), actual))+
    geom_point(color="red")+
    geom_line(aes(1:nrow(test_predictions),.pred))+
    xlab("Time (15 minutes)")+
    theme_tufte()+
    ggtitle(label = label)
  
  return(Fig)
}

# Create model, and return useful things (data, plots, stats)
choose_inputs <- function(all_data, dep, pred, label, importance, prop = 3/4, t = -1, modelType = "ranger") {
  
  #Clean and filter data set
  dt_clean <- all_data %>% 
    select(dep, pred) %>% 
    drop_na() %>% 
    rename(actual = dep)
  
  #Calculate correlations
  correlations <- cor(dt_clean)
  
  dt_split <- splitData(dt_clean, dep, pred, prop)
  
  #Create the train and test data
  dt_train <- training(dt_split)
  dt_test <- testing(dt_split)
  
  # create CV object from training data
  dt_cv <- vfold_cv(dt_train)
  
  #1. Create the Recipe
  dt_recipe <- createRecipe(dt_train, dt_clean)
  
  #2. Specify the Model
  rf_model <- specifyModel(modelType, t, importance)
  
  #3. Create workflow
  rf_workflow <- createWorkflow(dt_recipe, rf_model)
  
  #4. Tune mtry
  rf_workflow <- tuneMtry(rf_workflow, pred, dt_cv, t)
  
  #6. Fit model to train set and validate on test set
  rf_fit <- rf_workflow %>%
    # fit on the training set and evaluate on test set
    last_fit(dt_split)
  
  #7. Evaluate model performance
  test_predictions <- rf_fit %>% collect_predictions()
  all_metrics <- evaluateModel(rf_fit, test_predictions)
  
  #8. Build Figure
  Fig <- showEval(test_predictions, label)
  
  #9. Build Final Model
  final_model <- fit(rf_workflow, dt_clean)
  
  #Return a list of the ggplot figure, the final fit, the best mtry, 
  #and the mtry at each fold
  Fig_result <- list(Fig, final_model, rf_fit, all_metrics, correlations)
  
  return(Fig_result)
}
