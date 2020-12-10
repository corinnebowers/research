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
## precip (data.frame): list of synthetic precipitation events
## catalog (data.frame): catalog of ARs occurring in region of interest
## n.runoff (integer): number of runoff events to generate per precipitation event
## plot (logical): whether or not to show a plot

## @return
## runoff (data.frame): list of synthetic runoff events

generate_runoff <- function(precip, catalog, n.runoff = 1, plot = TRUE) {
  
  ## calculate n.AR and n.precip
  n.AR <- max(precip$n.AR)
  n.precip <- max(precip$n.precip)
  
  ## calculate the curve number (CN) by the statistical method
  
  ## find the characteristics of the annual max storm
  catalog$wateryear <- ifelse(month(catalog$start) %in% 10:12, year(catalog$start)+1, year(catalog$start))
  wateryear.df <- data.frame(wateryear = unique(catalog$wateryear), precip = NA, runoff = NA)
  for (wy in 1:nrow(wateryear.df)) {
    index <- catalog %>% 
      subset(wateryear == unique(wateryear)[wy]) %>% 
      select(precip) %>% unlist %>% which.max
    index.value <- catalog %>% 
      subset(wateryear == unique(wateryear)[wy]) %>% 
      subset(1:nrow(.) == index)
    wateryear.df[wy, 'precip'] <- index.value$precip / 25.4
    wateryear.df[wy, 'runoff'] <- index.value$runoff / 25.4
    wateryear.df[wy, 'IVT_max'] <- index.value$IVT_max
    wateryear.df[wy, 'duration'] <- index.value$duration
  }
  
  P <- wateryear.df$precip
  Q <- wateryear.df$runoff
  S.val <- ifelse(P>0 & Q>0, 5*(P + 2*Q - sqrt(5*P*Q + 4*Q^2)), NA)
  S.best <- Mean(log(S.val))
  S.sd <- sd(log(S.val), na.rm = TRUE)

  ## sample a CN value from the normal distribution
  runoff <- expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.runoff = 1:n.runoff) %>% 
    full_join(precip, by = c('n.AR', 'n.precip')) %>% 
    mutate(precip_in = precip_mm/25.4, 
           S = rnorm(n = nrow(.), mean = S.best, sd = S.sd), 
           CN = 1000/(10+exp(S)), 
           runoff_in = ifelse(precip_in < 0.2*exp(S), 0, 
                              (precip_in-0.2*exp(S))^2/(precip_in+0.8*exp(S))), 
           runoff_mm = runoff_in*25.4)
    
  if (plot) {
    dx <- seq(0, 20, 0.1)
    g <- ggplot() + 
      geom_ribbon(aes(x = dx, 
                      ymin = ifelse(dx < 0.2*exp(S.best-2*S.sd), 0, 
                                    (dx - 0.2*exp(S.best-2*S.sd))^2/(dx + 0.8*exp(S.best-2*S.sd))), 
                      ymax = ifelse(dx < 0.2*exp(S.best+2*S.sd), 0, 
                                    (dx - 0.2*exp(S.best+2*S.sd))^2/(dx + 0.8*exp(S.best+2*S.sd)))),
                  fill = 'grey80', alpha = 0.5) +
      geom_line(aes(x = dx, y = ifelse(dx < 0.2*exp(S.best), 0, 
                                       (dx - 0.2*exp(S.best))^2/(dx + 0.8*exp(S.best)))), 
                color = ggcolor(1), size = 1) +
      geom_point(data = runoff, aes(x = precip_in, y = runoff_in)) + 
      labs(x = 'Precipitation Estimate (in)', y = 'Runoff Estimate (in)') + 
      coord_fixed(ratio = 1) + 
      theme_classic()
    print(g)
    g.runoff <<- g
  }
  
  return(runoff)
}

print('4. generate runoff outcomes')

AR <- read.csv('./PARRA/_results/AR.csv')
catalog <- read.csv('./PARRA/_results/catalog.csv')
precip <- read.csv('./PARRA/_results/precip.csv')

runoff <- generate_runoff(precip, catalog, n.runoff = 25, plot = FALSE)

write.csv(runoff, file = './PARRA/_results/runoff.csv')
