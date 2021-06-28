
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
  library(fitdistrplus)
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
  library(pracma)
})


##### define simulation information ###############################################################

## set random seed
set.seed(1)

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
  return(se.pred*2)
}


#### load geographies #############################################################################
print('loading geographies...')

## EPSG codes for setting CRS
NAD <- 4269
albers <- 3310
zone2 <- 6417

## import useful geometries
sonoma <- tracts(state = 'CA', county = 'Sonoma', class = 'sf') %>% subset(NAME != 9901)
russian <- ('./_data/nhd_majorrivers/MajorRivers.shp') %>% 
  st_read(quiet = TRUE) %>% 
  st_zm(st_transform(albers)) %>% 
  subset(grepl('Russian', GNIS_Name))


##### define input information ####################################################################
print('1. identify area of interest')

## define inlet & outlet watersheds using USGS StreamStats
inlet <- st_read('./_data/inlet/layers/globalwatershed.shp', quiet = TRUE)
inlet.point <- st_read('./_data/inlet/layers/globalwatershedpoint.shp', quiet = TRUE)
outlet <- st_read('./_data/outlet/layers/globalwatershed.shp', quiet = TRUE)
outlet.point <- st_read('./_data/outlet/layers/globalwatershedpoint.shp', quiet = TRUE)

## define area of interest (aoi)
ext <- extent(1895000, 1935000, 579500, 616500)
aoi <- ext %>% as('SpatialPolygons') %>% st_as_sf %>% st_set_crs(6417)

## load dem
## note: most up-to-date version of dem comes from LISFLOOD.Rmd (6/1/21)
dem <- raster('./_data/LISFLOOD/russian.dem.asc')
crs(dem) <- projection(aoi)


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
load('_data/catalog.Rdata')


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
#   filter(start_day == '1995-01-06') %>% 
#   filter(start_day == '2005-12-30') %>% 
  filter(start_day == '2019-02-25') %>% 
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
buildings <- suppressWarnings(
  buildings %>% 
  st_as_sf(coords = c('X', 'Y'), crs = 4269) %>%
  st_transform(6417) %>%  
  st_intersection(aoi))

## calculate inundation height for each building
cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
inundation <- 
  generate_inundation(
    hydrograph = hydrograph,
    buildings = buildings,
    probabilistic = prob.inun,
    n.inun = n.inun
  )
stopCluster(cl)

## checkpoint
save(inundation, random.sim, file = paste0(results, 'INUN.Rdata'))


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
    buildings = buildings,
    foundations = nsi1.found,
    curve = 'beta',
    probabilistic = prob.dmg,
    n.damage = n.dmg
  )
stopCluster(cl)

## checkpoint
print('saving damage checkpoint...')
save(damage, file = paste0(results, 'DM.Rdata'))


#### G(DV): generate loss realizations ############################################################
print('8. generate loss realizations for each building & event')
source('./_scripts/DV_sherlock.R')



#load(paste0(results, 'DM.Rdata'))



## edit buildings dataframe
buildings <- buildings %>% 
  transmute(value = V601TotalL, value.sd = value_sd/5, group = GEOID)

cl <- parallel::makeCluster(num_cores)
registerDoSNOW(cl)
print('aggregating loss by census tract...')
loss.group <- 
  generate_losses(
    damage = damage,
    buildings = buildings,
    aggregate = 'group',
    probabilistic = prob.loss,
    n.loss = n.loss
  )

print('aggregating loss by simulation id...')
loss.sim <- 
  generate_losses(
    damage = damage,
    buildings = buildings,
    aggregate = 'sim',
    probabilistic = prob.loss,
    n.loss = n.loss
  )
stopCluster(cl)

## checkpoint
save(loss.group, loss.sim, file = paste0(results, 'DV.Rdata'))


###################################################################################################
print('done!')
