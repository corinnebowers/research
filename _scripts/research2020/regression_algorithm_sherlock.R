
cat('\n#### REGRESSION_ALGORITHM ####\n\n')

## packages #######################################################################################
cat('loading packages...\n')
suppressMessages({
  require(lubridate)
  require(sf)
  require(DMwR)
  require(MALDIquant)
  require(glmnet)
  require(caret)
  require(foreach)
  require(parallel)
  require(doParallel)
  require(car)
  require(grid)
  require(gridExtra)
  require(ggforce)
  require(ggrepel)
  require(randomForest)
  require(tidyverse); theme_set(theme_bw())
  require(factoextra)
})

## setup ##########################################################################################
cat('initializing code...\n')

## set random seed ####
set.seed(1)

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
# load('C:/Users/cbowers/Downloads/regression_dataframe_1115.Rdata')
load('./_results/regression_dataframe_1117.Rdata')
df <- california %>% 
  st_drop_geometry %>% 
  dplyr::select(-NAMELSAD, -COUNTYFP, -GEOID) %>% 
  full_join(county_catalog, ., by = c('county' = 'NAME')) %>% 
  mutate(CRS = ifelse(is.na(CRS), round(Min(CRS), -2), CRS),
         PDO = ifelse(is.na(PDO), Mean(PDO), PDO),
         rainyseason.month = month(start_day) + ifelse(month(start_day) %in% 10:12, -10, 3),
         y = factor(ifelse(value_claims>0, 'Yes', 'No'))) %>% 
  mutate(flood_experience = ifelse(flood_experience, 1, 0)) %>% 
  # mutate(flood_experience_1yr = ifelse(flood_experience_1yr, 1, 0),
  #        flood_experience_5yr = ifelse(flood_experience_1yr, 5, 0),
  #        flood_experience_10yr = ifelse(flood_experience_1yr, 10, 0)) %>%
  dplyr::select(-AR, -start_day, -end_day, -wateryear, -num_claims, 
                -value_claims, -disaster_declaration) %>%
  mutate(county = factor(county)) %>% 
  filter(!is.na(precip.total)) %>% 
  filter(!is.na(precip.prevmonth))

## print variables ####
cat(paste0('df variables (', ncol(df)-1, '): \n'))
sort(names(df))
cat('\n')

## split data into train & test ####
train <- sample(1:nrow(df), 0.8*nrow(df))
df.train <- df[train,]
df.test <- df[-train,]


## synthetic minority oversampling (SMOTE) ########################################################
cat('performing SMOTE resampling...\n')

ratio <- min(summary(df.train$y))/nrow(df.train)
df.smote <- SMOTE(y ~ ., data = df.train, 
                  perc.over = 100/ratio,  # minority oversampling percentage
                  perc.under = 200,       # majority undersampling percentage
                  k = ncol(df.train)-1)   # KNN used to generate new examples of minority class
row.names(df.smote) <- 1:nrow(df.smote)
obs.id <- data.frame(id = 1:nrow(df.smote), county = df.smote$county)
df.smote <- df.smote %>% dplyr::select(-county)


## principal component analysis (PCA) #############################################################
cat('performing principal component analysis...\n')

## define functions ####
plot_contribution <- function(data, plot.dim = 0) {
  ## get baseline PCA results 
  pca.results <- princomp(data %>% dplyr::select(-y), cor = TRUE)
  pca.var <- get_pca_var(pca.results)
  pca.eig <- get_eigenvalue(pca.results)
  
  print('Cumulative Variance Explained:')
  print(pca.eig$cumulative.variance.percent)
  
  ## get k-nearest neighbor clustering results 
  knn <- kmeans(pca.var$coord, centers = 5, nstart = 10, iter.max = 1e3)
  clusters <- data.frame(varnames = names(knn$cluster), cluster = unname(knn$cluster))
  
  ## plot raster contribution maps
  if (plot.dim > 0) {
    varnames <- names(data %>% dplyr::select(-y))
    temp <- pca.var$contrib %>%
      as.data.frame %>%
      cbind(varnames = row.names(.), .) %>%
      pivot_longer(cols = -varnames, names_to = 'dim', values_to = 'contrib') %>%
      full_join(data.frame(dim = row.names(pca.eig),
                           weight = pca.eig$variance.percent), by = 'dim') %>%
      mutate(dim = toNumber(gsub('Dim.', '', dim)),
             varnames = factor(varnames, levels = arrange(clusters, cluster)$varnames)) %>%
      filter(dim <= plot.dim)
    g1 <- ggplot(temp) +
      geom_tile(aes(y = varnames, x = dim, fill = contrib*weight)) +
      scale_x_continuous(breaks = 1:plot.dim) +
      scale_fill_viridis_c(name = 'Weighted \nContribution') + 
      theme(axis.title.y = element_blank())
    g2 <- ggplot(temp) +
      geom_tile(aes(y = varnames, x = dim, fill = contrib>10), color = 'grey50') +
      geom_tile(aes(y = varnames, x = dim, fill = contrib>7.5), alpha = 0.5, color = 'grey50') +
      scale_x_continuous(breaks = 1:plot.dim) + 
      scale_fill_manual(name = 'Contribution \n < 10%', values = c('grey25', 'orange')) +
      theme(axis.title.y = element_blank())
    (grid.arrange(g1, g2, ncol = 2))
  }
}
plot_components <- function(data, pca.var.list) {
  ## get PCA results for the variables in question 
  pca.results <- princomp(data[,pca.var.list], cor = TRUE)
  pca.var <- get_pca_var(pca.results)
  pca.eig <- get_eig(pca.results)
  
  print('Cumulative Variance Explained:')
  print(pca.eig$cumulative.variance.percent)
  
  ## plot component vector diagrams
  g <- ggplot(data.frame(pca.var$coord)) + 
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.5), color = 'grey70') + 
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.25), color = 'grey90') + 
    geom_circle(aes(x0 = 0, y0 = 0, r = 0.75), color = 'grey90') + 
    geom_circle(aes(x0 = 0, y0 = 0, r = 1)) + 
    geom_hline(yintercept = 0, color = 'grey70') + 
    geom_vline(xintercept = 0, color = 'grey70') + 
    geom_segment(aes(x = 0, xend = Dim.1, y = 0, yend = Dim.2, 
                     color = pca.var$cos2[,1]), size = 1) +
    geom_point(aes(x = Dim.1, y = Dim.2, color = pca.var$cos2[,1])) +
    # scale_color_distiller(name = '', palette = 'Spectral', direction = 1, limits = c(0,1)) + 
    scale_color_viridis_c(option = 'magma', limits = c(0,1), direction = -1) + 
    geom_text_repel(aes(x = Dim.1, y = Dim.2, label = row.names(pca.var$coord))) + 
    labs(x = 'Dim1', y = 'Dim2') +
    theme(panel.border = element_blank(), panel.grid = element_blank(),
          axis.text = element_blank(), axis.ticks = element_line(color = 'white'),
          axis.title = element_text(face = 'bold', size = 14)) + 
    scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + 
    coord_fixed(ratio = 1)
  print(g)
}
predictive_power <- function(data, pca.var.list) {
  ## get PCA results for the variables in question 
  pca.results <- princomp(data[,pca.var.list], cor = TRUE)
  pca.var <- get_pca_var(pca.results)
  
  var.AIC <- rep(NA, length(pca.var.list))
  for (i in 1:length(pca.var.list)) {
    logit <- glm(factor(y) ~ get(pca.var.list[i]), data = data, family = 'binomial')
    if (summary(logit)$coefficients[2,4] < 0.05) { var.AIC[i] <- AIC(logit) }
  }
  comp <- glm(factor(data$y) ~ pca.results$scores[,1], family = 'binomial')
  return(ifelse(AIC(comp) < Min(var.AIC), 'dim1', pca.var.list[which.min(var.AIC)]))
}

## start PCA iteration ####
df.pca <- df.smote
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))
plot <- TRUE

## remove correlated variable cluster ####
if (plot) plot_contribution(df.pca, plot.dim = 5)
pca.var.list <- c('impervious', 'developed', 'SFH', 'population', 'med_income', 'SVI_household')
if (plot) plot_components(df.pca, pca.var.list)
pca.var.list <- c('SFH', 'population')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-population)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))

## remove correlated variable cluster ####
if (plot) plot_contribution(df.pca, plot.dim = 5)
pca.var.list <- c('impervious', 'developed')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-impervious)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))

## remove correlated variable cluster ####
if (plot) plot_contribution(df.pca, plot.dim = 5)
pca.var.list <- c('precip.max', 'precip.total', 'IVT_max', 'days.max')
if (plot) plot_components(df.pca, pca.var.list)
pca.var.list <- c('precip.max', 'precip.total')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-precip.total)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))

## remove correlated variable cluster ####
if (plot) plot_contribution(df.pca, plot.dim = 5)
pca.var.list <- c('med_income', 'SVI_household', 'SVI_socioecon', 'med_struct_age')
if (plot) plot_components(df.pca, pca.var.list)
pca.var.list <- c('SVI_household', 'SVI_socioecon')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-SVI_socioecon)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))

## remove correlated variable cluster ####
if (plot) plot_contribution(df.pca, plot.dim = 5)
pca.var.list <- c('med_income', 'SVI_household', 'developed', 'med_struct_age')
if (plot) plot_components(df.pca, pca.var.list)
pca.var.list <- c('SVI_household', 'med_income')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-SVI_household)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))

## remove correlated variable cluster ####
if (plot) plot_contribution(df.pca, plot.dim = 5)
pca.var.list <- c('rainyseason', 'rainyseason.month', 'precip.season', 
                  'precip.prevmonth', 'precip.total')
if (plot) plot_components(df.pca, pca.var.list)
pca.var.list <- c('rainyseason', 'rainyseason.month')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-rainyseason)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))
glm(y ~ ., data = df.pca, family = 'binomial') %>% vif %>% sort

## remove correlated variable cluster ####
pca.var.list <- c('duration', 'days.max')
if (plot) predictive_power(df.pca, pca.var.list)
df.pca <- df.pca %>% select(-duration)
print(paste('Max VIF:', max(vif(glm(y ~ ., data = df.pca, family = 'binomial'))) %>% round(2)))

## at this point, all VIFs are under 5 and all collinearities are under 0.75
## print variables ####
cat(paste0('df.pca variables (', ncol(df.pca)-1, '): \n'))
sort(names(df.pca))
cat('\n')


## multi-split lasso algorithm (MS-LASSO) #########################################################
cat('performing MS-LASSO variable selection...\n')

MSLASSO <- function(data, criteria, n, plot = TRUE) {
  ## initialize variables ####
  ## note: y-variable should be the last one in the dataframe
  xvars <- names(data)[-ncol(data)]
  names(data)[ncol(data)] <- 'y'
   
  toNumber <- function(x) as.numeric(paste(x))
  summary.stats <- 
    foreach (i = 1:n,
             .packages = c('dplyr', 'glmnet')) %dopar% {
      ## split data into screening & cleaning ####
      screen <- sample(1:nrow(data), 0.5*nrow(data))
      df.screen <- data[screen,]
      
      ## run lasso on screening dataset ####
      lasso.fit <- cv.glmnet(x = model.matrix(y ~ .+0, df.screen), y = df.screen$y, 
                             alpha = 1, family = "binomial", type.measure = "deviance", 
                             standardize = TRUE, nfolds = 10)
      
      ## get lasso variables & coefficients ####
      temp <- coef(lasso.fit, s = lasso.fit$lambda.1se)
      lasso.coefs <- data.frame(name = temp@Dimnames[[1]][temp@i+1], coefficient = temp@x)
      lasso.vars <- paste(lasso.coefs$name)[-1]
      
      ## run least squares estimate on cleaning dataset with predictors selected by lasso ####
      df.clean <- data[-screen, c(lasso.vars, 'y')]
      mle.fit <- glm(y ~ ., data = df.clean, family = 'binomial')
      
      ## get the summary statistics for the least squares model ####
      summary(mle.fit)$coefficients[,-3] %>% 
        cbind(row.names(.), .) %>% data.frame %>% 
        setNames(c('varnames', 'estimate', 'stderror', 'pvalue')) %>% 
        mutate(VIM = 1/n) %>% 
        mutate(varnames = paste(varnames),
               across(where(is.factor), toNumber)) %>% 
        right_join(data.frame(varnames = xvars), by = 'varnames') %>% 
        mutate(estimate = ifelse(is.na(estimate), 0, estimate),
               stderror = ifelse(is.na(stderror), 0, stderror),
               VIM = ifelse(is.na(VIM), 0, VIM),
               pvalue = ifelse(is.na(pvalue), 1, pvalue),
               p.adjusted = (pvalue * (length(lasso.vars)-1))) %>% 
        mutate(p.adjusted = apply(cbind(p.adjusted, 1), 1, min)) %>% 
        arrange(varnames)
    }
  
  ## find the p-value from the empirical CDF (ask Rodrigo about this)
  p.final <- lapply(summary.stats, function(x) x$p.adjusted) %>% 
    do.call(cbind, .) %>% as.data.frame %>% 
    cbind(varnames = sort(xvars), .)
  df.summary <- data.frame(varnames = p.final$varnames)
  for (i in 1:nrow(p.final)){
    p.sorted <- sort(t(p.final[i,-1]), decreasing = FALSE) 
    quantiles.df <- data.frame(delta = seq(0.05, 1, 0.01)) %>% 
      mutate(q = unname(quantile(p.sorted, probs = delta)), q_over_d = q/delta)
    summ.delta <- min(4*min(quantiles.df$q_over_d), 1)
    df.summary$pValue[i] <- 
      p.sorted[match.closest(summ.delta, p.sorted, tolerance = Inf, nomatch = NA)]
  }
  
  ## summarize results from the multi-split algorithm ####
  temp <- lapply(summary.stats, function(x) x$VIM) %>% do.call(cbind, .) 
  VIM <- data.frame(varnames = sort(xvars), VIM = apply(temp, 1, sum))
  results <- VIM %>% full_join(df.summary, by = 'varnames') %>% mutate(include = NA)
  if (criteria == 'And') {  #more restrictive case
    results <- results %>% 
      mutate(include = ifelse(VIM > 0.75 & pValue < 0.05, TRUE, FALSE))
    }
  if (criteria == 'Or') {   #less restrictive case
    results <- results %>% 
      mutate(include = ifelse(VIM > 0.75 | pValue < 0.05, TRUE, FALSE))
  }
  if (plot) {
    g <<- ggplot(data = results) + 
      geom_point(aes(x = jitter(VIM, factor = 20), 
                     y = jitter(pValue, factor = 20), 
                     color = factor(include)), 
                 size = 5, alpha = 0.5, show.legend = FALSE) + 
      scale_color_manual(values = c('black', 'red')) + 
      geom_hline(yintercept = 0.05, color = 'red', linetype = 'dashed') + 
      geom_vline(xintercept = 0.75, color = 'red', linetype = 'dashed') + 
      # geom_text_repel(aes(x = VIM, y = pValue, label = varnames)) +
      coord_cartesian(xlim = c(0,1), ylim = c(0,1)) +
      ggtitle('Multi-Split Lasso Results') + 
      labs(x = 'Variable Importance Measure (VIM)', y = 'p-Value')
  }
  ## return selected variables ####
  return(results %>% filter(include) %>% .$varnames %>% paste)
}

## run function ####
timer <- Sys.time()
cl <- makeCluster(num_cores)
registerDoParallel(cl)
mslasso.vars <- MSLASSO(df.pca[,sort(names(df.pca))], criteria = 'And', n = 100, plot = TRUE)
stopCluster(cl)
Sys.time() - timer
cat(paste('The MS-LASSO algorithm kept', length(mslasso.vars), 'out of', 
          ncol(df.pca)-1, 'predictors.\n'))

## print plot ####
g; ggsave('./_results/mslasso_plot.jpg', width = 5, height = 5)

## print variables ####
df.mslasso <- df.pca %>% dplyr::select(all_of(mslasso.vars), y)
cat(paste0('df.mslasso variables (', ncol(df.mslasso)-1, '): \n'))
sort(names(df.mslasso))
cat('\n')

## checkpoint save #### 
save(df.train, df.test, df.smote, obs.id, df.pca, df.mslasso, mslasso.vars, 
     file = './_results/regression_results_1117.Rdata')


## machine learning ###############################################################################
cat('running predictive analysis...\n')

## define training information ####
ctrl <- trainControl(method = 'repeatedcv', number = 10, repeats = 5)

## initialize parallel backend ####
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# print('...elastic net...')
# timer <- Sys.time()
# f.net <- 
#   train(y ~ ., data = df.mslasso, method = 'glmnet', 
#         family = 'binomial', 
#         tuneGrid = expand.grid(
#           alpha = seq(0, 1, 0.02), 
#           lambda = 10^seq(-5, -1, length.out = 100)),
#         trControl = ctrl)
# save(f.net, file = './_results/models/f.net.Rdata')
# Sys.time() - timer

# print('...MARS...')
# timer <- Sys.time()
# f.mars <- 
#   train(y ~ ., data = df.mslasso, method = 'earth',
#         tuneGrid = expand.grid(
#           degree = 1:4, 
#           nprune = seq(2, length(mslasso.vars), 2)), 
#         trControl = ctrl) 
# save(f.mars, file = './_results/models/f.mars.Rdata')
# Sys.time() - timer

cat('...fitting random forest model...\n')
timer <- Sys.time()
f.rf <- 
  train(y ~ ., data = df.mslasso, method = 'parRF',
        localImp = TRUE,
        tuneGrid = expand.grid(
          mtry = 1:length(mslasso.vars)), 
        trControl = ctrl)
save(f.rf, file = './_results/models/f.rf.Rdata')
Sys.time() - timer

# print('...boosted trees...')
# timer <- Sys.time()
# f.boost <- 
#   train(y ~ ., data = df.mslasso, method = 'gbm', 
#         verbose = FALSE, 
#         tuneGrid = expand.grid(
#           n.trees = c(100, 500, 1000), 
#           interaction.depth = 1:3,
#           shrinkage = 10^seq(-5, -1, length.out = 50), 
#           n.minobsinnode = 10),
#         trControl = ctrl)
# save(f.boost, file = './_results/models/f.boost.Rdata')
# Sys.time() - timer


## close out ######################################################################################
stopCluster(cl)
cat('done!\n\n')

