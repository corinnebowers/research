
cat('\n#### REGRESSION_ANALYSIS ####\n\n')

## packages #######################################################################################
cat('loading packages...\n')
suppressMessages({
  require(foreach)
  require(parallel)
  require(doParallel)
  require(doSNOW)
  require(randomForest)
  require(tidyverse); theme_set(theme_bw())
})


## setup ##########################################################################################
cat('initializing code...\n')

## global options ####
options(show.error.locations = TRUE)

## parallel backend ####
num_cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))*2/3

## helper functions ####
toNumber <- function(x) as.numeric(paste(x))
sum.na <- function(x) sum(is.na(x))         
Mean <- function(x) ifelse(sum(is.na(x)) == length(x), NA, mean(x, na.rm = TRUE))
Sum <- function(x) sum(x, na.rm = TRUE)
Max <- function(x) max(x, na.rm = TRUE) 
Min <- function(x) min(x, na.rm = TRUE)

## load data #### 
load('./_results/models/f.rf.Rdata')
load('./_results/regression_results_1117.Rdata')
# load('C:/Users/cbowers/Downloads/f.rf(1).Rdata')
# load('C:/Users/cbowers/Downloads/regression_results_1116.Rdata')


## calculate partial dependence ###################################################################
cat('calculating partial dependence...\n')

## sort mslasso.vars in order of importance ####
importance <- f.rf$finalModel$importance %>% 
  as.data.frame %>% 
  rownames_to_column(var = 'varnames') %>% 
  arrange(desc(MeanDecreaseAccuracy))
mslasso.vars <- mslasso.vars[match(importance$varnames, mslasso.vars)]
  
## initialize parallel backend ####
# cl <- makeCluster(num_cores)
# registerDoSNOW(cl)

## calculate partial dependence for each variable ####
# f.rf.model <- f.rf$finalModel
# 
# pb <- txtProgressBar(min = 0, max = length(mslasso.vars), style = 3)
# partials <- 
#   foreach(i = 1:length(mslasso.vars), 
#           .packages = c('dplyr', 'randomForest')) %do% {
#     x.name = mslasso.vars[i]
#     partialPlot(f.rf$finalModel, df.mslasso, which.class = 'Yes', 
#                 x.var = x.name, n.pt = 100, plot = FALSE) %>% do.call('cbind', .)
#   }
# stopCluster(cl)

partials <- list()
pb <- txtProgressBar(min = 0, max = length(mslasso.vars), style = 3)
for (i in 1:length(mslasso.vars)) {
  partials[[i]] <- 
    partialPlot(f.rf$finalModel, df.mslasso, which.class = 'Yes', 
                x.var = mslasso.vars[i], n.pt = 100, plot = FALSE) %>% 
    do.call('cbind', .)
  attr(partials[[i]], 'varname') <- mslasso.vars[i]
  setTxtProgressBar(pb, i)
  cat('\n')
}


## save out ######################################################################################
save(partials, file = './_results/partialdependence_1117.Rdata')
cat('done!\n\n')

