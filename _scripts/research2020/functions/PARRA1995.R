
##### load packages ###############################################################################
print('loading packages...')

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(raster)
  library(tigris); options(tigris_use_cache = TRUE)
  library(lubridate)
  library(rnoaa); rnoaa_options(cache_messages = FALSE)
  library(mvtnorm)
  library(evd)
  library(quantreg)
  library(caret)
  library(pracma)
  library(dataRetrieval)
  library(exactextractr)
  library(fitdistrplus)
  library(scales) 
  library(tidyverse); theme_set(theme_bw())
  library(foreach)
  library(doSNOW)
  library(parallel)
})


##### define simulation information ###############################################################

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))

## set working directory
setwd('/home/users/cbowers/PARRA/')

## read input information
args <- commandArgs(trailingOnly = TRUE)

## define number of Monte Carlo runs per model
prob.ar <- as.logical(args[1]); n.ar <- as.numeric(args[2])
prob.prcp <- as.logical(args[3]); n.prcp <- as.numeric(args[4])
prob.rnff <- as.logical(args[5]); n.rnff <- as.numeric(args[6])
prob.hydro <- as.logical(args[7]); n.hydro <- as.numeric(args[8])
prob.inun <- as.logical(args[9]); n.inun <- as.numeric(args[10])
prob.dmg <- as.logical(args[11]); n.dmg <- as.numeric(args[12])
prob.loss <- as.logical(args[13]); n.loss <- as.numeric(args[14])

## set results folder
results <- args[15]


##### load functions ##############################################################################
print('loading useful functions...')

toNumber <- function(x) as.numeric(paste(x))
Mean <- function(x) mean(x, na.rm = TRUE)
Sum <- function(x) sum(x, na.rm = TRUE)
Max <- function(x) max(x, na.rm = TRUE)
Min <- function(x) min(x, na.rm = TRUE)
sum.na <- function(x) sum(is.na(x))

scale_x_origin <- function(...) {
  scale_x_continuous(expand = expansion(mult = c(0, 0.01)), limits = c(0,NA), ...) }
scale_y_origin <- function(...) {
  scale_y_continuous(expand = expansion(mult = c(0, 0.01)), limits = c(0,NA), ...) }
geom_parity <- function() geom_abline(slope = 1, intercept = 0, linetype = 'dashed')

predict.se <- function(model, fitdata, newdata) {
  dof <- nrow(fitdata) - ncol(model.matrix(model, fitdata)) #find degrees of freedom
  MSE <- sqrt(sum(residuals(model)^2)/dof) #find MSE
  V <- solve(t(model.matrix(model, fitdata)) %*% 
               model.matrix(model, fitdata)) * MSE^2 #find var-cov matrix of coefficients
  X <- model.matrix(delete.response(terms(model)), newdata) #create matrix of new data
  yhat <- predict(model, newdata) #predict new response
  var.fit <- rowSums((X %*% V) * X) #find the diagonal of the var-cov matrix for yhat
  # se.conf <- sqrt(var.fit) #find pointwise standard errors of predicted mean (CI)
  se.pred <- sqrt(var.fit + MSE^2) #find standard error of the prediction interval (PI)
  return(se.pred)
}


#### load geographies #############################################################################
print('loading geographies...')

## EPSG codes for setting CRS
NAD <- 4269
albers <- 3310

## import geographic boundaries
# USA <- states(class = 'sf')
# california <- counties(state = 'CA', class= 'sf')
sonoma <- tracts(state = 'CA', county = 'Sonoma', class = 'sf') %>% subset(NAME != 9901)

## import useful features
russian <- ('./_data/nhd_majorrivers/MajorRivers.shp') %>% 
  st_read(quiet = TRUE) %>% 
  st_zm(st_transform(albers)) %>% 
  subset(grepl('Russian', GNIS_Name))

# ## import watersheds
# wbd4 <- st_read('./_data/wbds/WBDHU4.shp', quiet = TRUE)
# wbd6 <- st_read('./_data/wbds/WBDHU6.shp', quiet = TRUE)
# wbd8 <- st_read('./_data/wbds/WBDHU8.shp', quiet = TRUE)
# wbd10 <- st_read('./_data/wbds/WBDHU10.shp', quiet = TRUE)
# wbd12 <- st_read('./_data/wbds/WBDHU12.shp', quiet = TRUE)


##### define input information ####################################################################
print('1. identify area of interest')

## define inlet & outlet watersheds using USGS StreamStats
inlet <- st_read('./_data/inlet/layers/globalwatershed.shp', quiet = TRUE)
inlet.point <- st_read('./_data/inlet/layers/globalwatershedpoint.shp', quiet = TRUE)
outlet <- st_read('./_data/outlet/layers/globalwatershed.shp', quiet = TRUE)
outlet.point <- st_read('./_data/outlet/layers/globalwatershedpoint.shp', quiet = TRUE)

## load dem
## note: most up-to-date version of dem comes from LISFLOOD.Rmd (3/20/21)
dem <- raster('./_data/LISFLOOD/russian.dem.asc')
crs(dem) <- paste(
  '+proj=lcc', 
  '+lat_0=37.6666666666667', '+lon_0=-122', 
  '+lat_1=39.8333333333333', '+lat_2=38.3333333333333', 
  '+x_0=2000000', '+y_0=500000', 
  '+ellps=GRS80', '+units=m', '+no_defs')

## convert dem to sf format to get area of interest (aoi)
aoi <- extent(dem) %>% 
  as('SpatialPolygons') %>% 
  as('sf') %>% 
  st_set_crs(proj4string(dem)) %>% 
  st_transform(st_crs(sonoma))  


##### create historic catalog #####################################################################
print('2. generate historic catalog') 
source('./_scripts/AR_sherlock.R')

## decide which USGS gauges are most representative of flows in the study area
gauge <- c(11463500, 11463682, 11464000)

## create AR catalog
#cl <- parallel::makeCluster(num_cores)
#registerDoSNOW(cl)
#catalog <- generate_AR_catalog(outlet, gauge, ar.threshold = 0.5)
#stopCluster(cl)

## checkpoint
#save(catalog, file = paste0(results, 'catalog.Rdata'))
load('/home/groups/bakerjw/cbowers/PARRA/catalog.Rdata')


##### G(AR): generate AR realizations #############################################################
print('3. generate AR realizations')

## simulate new ARs
#cl <- parallel::makeCluster(num_cores)
#registerDoSNOW(cl)
#AR <- 
#  generate_AR(
#    catalog = catalog, 
#    n.AR = n.ar, 
#    intensity.threshold = 0)
#stopCluster(cl)

## or, use a scenario event
AR <- catalog %>% 
  filter(start_day == '1995-01-06') %>% 
#   filter(start_day == '2005-12-30') %>% 
#   filter(start_day == '2019-02-25') %>% 
  transmute(n.AR = 1, IVT_max, duration)

## checkpoint
save(AR, file = paste0(results, 'AR.Rdata'))


##### G(PRCP): generate precipitation realizations ################################################
print('4. generate precipitation realizations')
source('./_scripts/PRCP_sherlock.R')

## simulate precipitation values
precip <- 
  generate_precip(
    AR = AR, 
    catalog = catalog,
    probabilistic = prob.prcp,
    n.precip = n.prcp)

## checkpoint
save(precip, file = paste0(results, 'PRCP.Rdata'))
precip <- precip %>% filter(precip_mm > 0)


##### G(RNFF): generate runoff realizations #######################################################
print('5. generate runoff realizations')
source('./_scripts/RNFF_sherlock.R')



#load(paste0(results, 'PRCP.Rdata'))
#precip <- precip %>% filter(precip_mm > 0)



## simulate runoff values
runoff <- 
  generate_runoff(
    precip = precip, 
    catalog = catalog, 
    probabilistic = prob.rnff,
    n.runoff = n.rnff)

## convert runoff to a hydrograph (Qp+tp)
hydrograph <- 
  generate_hydrograph(
    precip = precip,
    runoff = runoff, 
    catalog = catalog, 
    probabilistic = prob.hydro,
    n.hydro = n.hydro)

## checkpoint
save(runoff, hydrograph, file = paste0(results, 'Q.Rdata'))
hydrograph <- hydrograph %>% filter(Qp_m3s > 0)


#### G(INUN): generate inundation realizations ####################################################
print('6. generate inundation realizations at building locations')
source('./_scripts/INUN_sherlock.R')



#load(paste0(results, 'Q.Rdata'))
#hydrograph <- hydrograph %>% filter(Qp_m3s > 0)



## load building information (from buildings.Rmd)
load('./_data/buildings.Rdata')
res.buildings <- res.buildings[-11654,] 
## res.buildings was cropped to coarser nonzero.buffer --> one snuck through

## calculate inundation height for each building
cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
inundation <- 
  generate_inundation(
    hydrograph = hydrograph,
    buildings = res.buildings,
    probabilistic = prob.inun,
    n.inun = n.inun
  )
stopCluster(cl)

## checkpoint
save(inundation, file = paste0(results, 'INUN.Rdata'))


#### G(DM): generate damage realizations ##########################################################
print('7. generate damage realizations for each building & event')
source('./_scripts/DM_sherlock.R')

## load foundation information (from buildings.Rmd)
load('./_data/foundations.Rdata')

## calculate damage ratios for each building
cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
damage <- 
  generate_damage(
    inundation = inundation,
    buildings = res.buildings,
    foundations = nsi1.found,
    curve = 'average',
    probabilistic = prob.dmg,
    n.damage = n.dmg
  )
stopCluster(cl)

## checkpoint
save(damage, file = paste0(results, 'DM.Rdata'))


#### G(DV): generate loss realizations ############################################################
print('8. generate loss realizations for each building & event')
source('./_scripts/DV_sherlock.R')



#load(paste0(results, 'DM.Rdata'))



## edit buildings dataframe
res.buildings <- res.buildings %>% 
  transmute(value = V601TotalL, value.sd = CT_sd, group = GEOID)

cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
print('aggregating loss by census tract...')
loss.group <- 
  generate_losses(
    damage = damage,
    buildings = res.buildings,
    aggregate = 'group',
    probabilistic = prob.loss,
    n.loss = n.loss
  )

print('aggregating loss by simulation id...')
loss.sim <- 
  generate_losses(
    damage = damage,
    buildings = res.buildings,
    aggregate = 'sim',
    probabilistic = prob.loss,
    n.loss = n.loss
  )
stopCluster(cl)

## checkpoint
save(loss.group, loss.sim, file = paste0(results, 'DV.Rdata'))


###################################################################################################
print('done!')
