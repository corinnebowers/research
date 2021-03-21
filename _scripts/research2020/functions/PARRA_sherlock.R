
# things to do:
#   -fix all hard-coded urls
#   -fix dem to be the more accurate one
#   -fix all sourced file urls

##### load packages ###############################################################################
print('loading packages...')

library(sf)
library(raster)
# library(reshape2)
library(tigris); options(tigris_use_cache = TRUE)
library(lubridate)
# library(RColorBrewer)
library(rnoaa); rnoaa_options(cache_messages = FALSE)
library(quantreg)
library(dataRetrieval)
# library(evd)
library(exactextractr)
library(fitdistrplus) ##new!
library(scales) ##new!
library(tidyverse); theme_set(theme_bw())
library(foreach)
library(doSNOW)
library(parallel)


##### define simulation information ###############################################################

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))

## set working directory


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


#### load geographies #############################################################################
print('loading geographies...')

## import geographic boundaries
# USA <- states(class = 'sf')
# california <- counties(state = 'CA', class= 'sf')
sonoma <- tracts(state = 'CA', county = 'Sonoma', class = 'sf') %>% subset(NAME != 9901)

## import useful features
russian <- st_read('./_gis/California/_hydrology/nhd_majorrivers/MajorRivers.shp', quiet = TRUE) %>% 
  st_zm(st_transform(albers)) %>% 
  subset(grepl('Russian', GNIS_Name))

# ## import watersheds
# wbd4 <- st_read('./_gis/California/_floodhazard/WBD_18_HU2_Shape/Shape/WBDHU4.shp', quiet = TRUE)
# wbd6 <- st_read('./_gis/California/_floodhazard/WBD_18_HU2_Shape/Shape/WBDHU6.shp', quiet = TRUE)
# wbd8 <- st_read('./_gis/California/_floodhazard/WBD_18_HU2_Shape/Shape/WBDHU8.shp', quiet = TRUE)
# wbd10 <- st_read('./_gis/California/_floodhazard/WBD_18_HU2_Shape/Shape/WBDHU10.shp', quiet = TRUE)
# wbd12 <- st_read('./_gis/California/_floodhazard/WBD_18_HU2_Shape/Shape/WBDHU12.shp', quiet = TRUE)


##### define input information ####################################################################
print('1. identify area of interest')

## define inlet watershed using USGS StreamStats
inlet <- st_read('C:/Users/cbowers/Downloads/inlet/layers/globalwatershed.shp', quiet = TRUE)
inlet.point <- st_read('C:/Users/cbowers/Downloads/inlet/layers/globalwatershedpoint.shp', quiet = TRUE)

## define outlet watershed using USGS StreamStats
outlet <- st_read('C:/Users/cbowers/Downloads/outlet/layers/globalwatershed.shp', quiet = TRUE)
outlet.point <- st_read('C:/Users/cbowers/Downloads/outlet/layers/globalwatershedpoint.shp', quiet = TRUE)

## define area of interest (aoi)
dem <- raster('C:/Users/cbowers/Downloads/watersheds/Topobathy.tif')
topobathy <- raster('C:/Users/cbowers/Downloads/watersheds/Topobathy_save.tif')
dem <- crop(dem, topobathy)
aoi <- extent(dem) %>% 
  as('SpatialPolygons') %>% 
  as('sf') %>% 
  st_set_crs(proj4string(dem)) %>% 
  st_transform(st_crs(sonoma))  #the area under consideration, in sf format


##### G(AR): generate ARs #########################################################################
print('2. generate ARs')

source('D:/Research/_scripts/research2020/functions/AR_parallel.R')

## decide which USGS gauges are most representative of flows in the study area
gauge <- c(11464000, 11467000)

## create AR catalog
cl <- makeCluster(num_cores)
registerDoSNOW(cl)
catalog <- generate_AR_catalog(outlet, gauge, ar.threshold = 0.5)
stopCluster(cl)

## save catalog 
save(catalog, 'catalog.Rdata')

## simulate new ARs
AR <- generate_AR(catalog, n.AR = 100, intensity.threshold = 0)



