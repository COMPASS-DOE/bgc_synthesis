gatherMetrics <- function(rang, rand){
  gatheredMetrics <- list()
  for(i in 1:length(rang)){
    gatheredMetrics[[i]] <- as_tibble(cbind(rang[[i]][[4]], 
                    rand[[i]][[4]], 
                    rownames(rand[[i]][[4]]))) %>% 
      rename("ranger" = 1,
             "randomForest" = 2, 
             "gob" = 3)
  }
  gatheredMetrics <- bind_rows(gatheredMetrics)
  return(gatheredMetrics)
}

summarizeMetrics <- function(gatheredMetrics){
  sumMetrics <- gatheredMetrics %>% 
    group_by(gob) %>% 
    summarize(ranger = mean(as.numeric(ranger)), randomForest = mean(as.numeric(randomForest)))
    
  return(sumMetrics)
}