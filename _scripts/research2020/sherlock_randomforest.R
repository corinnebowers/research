
## packages #######################################################################################
print('loading packages...')
suppressMessages({
  require(tidyverse); theme_set(theme_bw())
  require(lubridate)
  require(sf)
  require(DMwR)
  require(MALDIquant)
  require(glmnet)
  require(caret)
  require(foreach)
  require(parallel)
  require(doParallel)
  require(grid)
  require(gridExtra)
  require(earth)
  require(randomForest)
  require(gbm)
})


## setup ##########################################################################################
print('initializing code...')

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
# save(df.train, df.smote, df.pca, df.mslasso, mslasso.vars, obs.id, df.test, 
#      file = 'C:/Users/cbowers/Downloads/input_randomforest.Rdata')
load('./_temp/input_randomforest.Rdata')
df.mslasso <- df.pca %>% dplyr::select(all_of(mslasso.vars), y)

## run random forest classification ###############################################################

## initialize parallel backend ####
cl <- makeCluster(num_cores)
registerDoParallel(cl)
ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 5)

print('running random forest...')
timer <- Sys.time()
f.rf <-
  train(y ~ ., data = df.pca, method = 'parRF',
        localImp = TRUE,
        tuneGrid = expand.grid(
          mtry = seq(2, length(mslasso.vars), 2)),
        trControl = ctrl)
save(f.rf, file = './_temp/f.rf_friday.Rdata')
Sys.time() - timer

# print('running random forest...')
# timer <- Sys.time()
# f.rf <- 
#   train(y ~ ., data = df.mslasso, method = 'rf',
#         localImp = TRUE,
#         tuneGrid = expand.grid(
#           mtry = seq(2, length(mslasso.vars), 2)), 
#         trControl = ctrl)
# save(f.rf, file = './_temp/f.rf.Rdata')
# Sys.time() - timer


## close out ######################################################################################
stopCluster(cl)
print('done!')

