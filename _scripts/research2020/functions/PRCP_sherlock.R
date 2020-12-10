print('loading packages...')

require(ggplot2); theme_set(theme_bw())
require(sf)
require(raster)
require(reshape2)
require(dplyr)
require(tigris); options(tigris_use_cache = TRUE)
require(stringr)
require(lubridate)
# require(velox)
require(RColorBrewer)
require(rnoaa); rnoaa_options(cache_messages = FALSE)
require(quantreg)
require(dataRetrieval)
require(mvtnorm)
require(evd)
require(foreach)
require(parallel)
require(future)
require(doSNOW)

print('defining input information...')

toNumber <- function(x) as.numeric(paste(x))

ggcolor <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

log_breaks <- function(min, max) rep(1:9, (max-min+1))*(10^rep(min:max, each = 9))

Mean <- function(x) mean(x, na.rm = TRUE)
Sum <- function(x) ifelse(nrow(x)>0, sum(x, na.rm = TRUE), NA)
Max <- function(x) max(x, na.rm = TRUE)
Min <- function(x) min(x, na.rm = TRUE)

## meters to feet conversion factor
mft <- 3.28084

USA <- states(class = 'sf')
california <- counties(state = 'CA', class= 'sf')
sonoma <- tracts(state = 'CA', county = 'Sonoma', class = 'sf') %>% subset(NAME != 9901)

print('identifying area of interest...')

# data = matrix(rnorm(100), nrow = 10)
# write.csv(data, './test.csv')

## define inlet watershed using USGS StreamStats
print('reading inlet...')
inlet <- st_read('./PARRA/_data/inlet/layers/globalwatershed.shp', quiet = TRUE)
inlet.point <- st_read('./PARRA/_data/inlet/layers/globalwatershedpoint.shp', quiet = TRUE)

## define outlet watershed using USGS StreamStats
print('reading outlet...')
outlet <- st_read('./PARRA/_data/outlet/layers/globalwatershed.shp', quiet = TRUE)
outlet.point <- st_read('./PARRA/_data/outlet/layers/globalwatershedpoint.shp', quiet = TRUE)

## define area of interest (aoi)
print('reading dem...')
dem <- raster('./PARRA/_data/watersheds/TopoBathy.tif')
print('reading topobathy...')
topobathy <- raster('./PARRA/_data/watersheds/TopoBathy_save.tif')
print('transforming...')
dem <- crop(dem, topobathy)
aoi <- extent(dem) %>%
  as('SpatialPolygons') %>%
  as('sf') %>%
  st_set_crs(proj4string(dem)) %>%
  st_transform(st_crs(sonoma))  #the area under consideration, in sf format

## @param
## catalog (data.frame): catalog of ARs occurring in region of interest
## n.precip (integer): number of precipitation events to generate per AR
## dx (double): interval between quantile calculations (recommended 0.01-0.1)
## precip.threshold (double): only sample precipitation events above this percentile (recommended 0.5)
## plot (logical): print plot to console? 

## @return
## precip (data.frame): list of synthetic precipitation events

generate_precip <- function(AR, catalog, dx = 0.05, n.precip = 1, precip.threshold = 0.5, 
                            quiet = FALSE, plot = TRUE) {
  ## fix inputs 
  precip.threshold <- max(precip.threshold, dx)
  
  ## calculate n.AR
  n.AR <- max(AR$n.AR)
  
  ## fit G(PRCP), probability of precipitation 
  quant.model <- list()
  tau <- seq(dx, 1-dx, dx)
  for (i in 1:length(tau)) {
    quant.model[[i]] <- rq(precip ~ IVT_max + duration, data = catalog, tau = tau[i])
  }
  
  ## generate precipitation realizations
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  pb <- txtProgressBar(min = 0, max = n.AR, style = 3)
  precip <- 
    foreach (ar = 1:n.AR, 
             .combine = 'rbind',
             .packages = c('quantreg', 'dplyr'), 
             .options.snow = opts) %dopar% {
      quant.result <- data.frame(tau = tau, fit = NA, lower = NA, higher = NA)
      for (i in 1:length(tau)) {
        quant.boot <- predict(quant.model[[i]], newdata = AR[ar,], 
                              type = 'percentile', interval = 'confidence', se = 'boot')
        quant.result[i, 2:4] <- quant.boot
        quant.result[i,'sd'] <- (quant.result$higher[i]-quant.result$lower[i])/4
      }
      q <- sample(seq(precip.threshold, 1-dx, dx), size = n.precip, replace = TRUE)
      index <- match(round(q, 2), round(tau, 2))
      
      data.frame(n.AR = ar, n.precip = 1:n.precip, q = q,
                 precip_mm = data.frame(mean = quant.result[index, 'fit'],
                                        sd = quant.result[index, 'sd']) %>% 
                   apply(1, function(x) rnorm(n = 1, mean = x[1], sd = x[2])))
    }

  ## plot cumulative distribution
  if (plot) {
    g <- ggplot() 
    for (i in 1:n.AR) {
      g <- g + geom_line(data = quant.result[[i]], aes(x = tau, y = fit)) + 
        geom_ribbon(data = quant.result[[i]], aes(x = tau, ymin = lower, ymax = higher),
                    fill = 'grey70', alpha = 0.5) 
    }
    g <- g + geom_point(data = precip, aes(x = q, y = precip_mm)) + 
      labs(x = 'Percentile', y = 'Expected Precipitation (mm)') + 
      scale_x_continuous(limits = c(0,1)) + 
      coord_fixed(ratio = 2.5e-3) + 
      theme_classic()
    print(g)
    g.precip <<- g
  }
  
  precip <- precip %>% full_join(AR, by = 'n.AR')
  return(precip)
}

print('3. generate precipitation outcomes')

AR <- read.csv('./PARRA/_results/AR.csv')
catalog <- read.csv('./PARRA/_results/catalog.csv')

## register parallel backend
num_cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))
cl <- makeCluster(num_cores)
registerDoSNOW(cl)

precip <- generate_precip(AR, catalog, n.precip = 100, dx = 0.05, precip.threshold = 0.5, plot = FALSE)
stopCluster(cl)

write.csv(precip, file = './PARRA/_results/precip.csv')
