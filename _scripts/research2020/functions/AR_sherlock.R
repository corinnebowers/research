
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

print('1. identify area of interest')

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
## aoi (sf): polygon bounding region of interest
## gauge (vector): list of USGS gauge ID(s) that best represent flows within the study area
   ## note: if more than one gauge is listed, the reported flow will be the average
## ar.threshold (double): controls what % of MERRA cells need to register an AR for it to count (recommended 0.3-0.5)

## @return
## catalog (data.frame): catalog of ARs occurring in region of interest

generate_AR_catalog <- function(aoi, gauge, ar.threshold = 0.5) {
  ## data needed:
  load('./PARRA/_data/grid_catalog.Rdata')  # list of ARs by cell (ar_grid) and a spatial tracker (tracker)

  ## find which cells cross the area of interest
  tracker.raster <- tracker[,c(2,1,3,4)]
  tracker.raster <- rasterFromXYZ(tracker.raster, crs = "+proj=longlat +datum=NAD83 +no_defs")
  tracker.id <- rasterize(aoi, tracker.raster, getCover = TRUE) %>%
    as.data.frame(xy = TRUE) %>%
    subset(layer > 0) %>%
    left_join(tracker, by = c('x'='lon', 'y'='lat')) %>%
    select(step) %>%
    unlist %>% unname %>% sort
  tracker.id <- tracker.id[!(tracker.id %in% c(827,1185))]  ## these ones cause issues

  ## identify all AR storms in the region of interest
  storm.df <- data.frame(date = seq(ymd('1980-01-01'), ymd('2019-12-31'), 'days'))
  storm.df[,paste(tracker.id)] <- 0
  hour.df <- storm.df
  for (id in 1:length(tracker.id)) {
    ar_list <- ar_grid[[tracker.id[id]]]
    ar_list$total_hours <- (ymd_h(paste(ar_list$end_date, ar_list$end_hour)) -
                              ymd_h(paste(ar_list$start_date, ar_list$start_hour))) %>%
      as.numeric(units = 'hours')
    for (i in 1:nrow(ar_list)) {
      storm <- seq(ymd(ar_list[i, 'start_date']), ymd(ar_list[i, 'end_date']), 'days')
      storm.df[storm.df$date %in% storm, id+1] <- ar_list[i, 'IVT_max']
      hour.df[storm.df$date == storm[1], id+1] <- ar_list[i, 'total_hours']
    }
  }

  ## subset to rainy season
  storm.df <- storm.df %>% subset(month(date) %in% c(10:12, 1:3))
  hour.df <- hour.df %>% subset(month(date) %in% c(10:12, 1:3))
  storm.df$storm <- apply(storm.df[,2:(length(tracker.id)+1)], 1, function(x) sum(x > 0))

  ## number AR storms
  storm.df$ar <- 0
  ar <- 0
  for (i in 2:nrow(storm.df)) {
    if (storm.df[i, 'storm'] >= length(tracker.id)*ar.threshold) {
      if (storm.df[i-1, 'storm'] <= length(tracker.id)*ar.threshold) {
        ar <- ar + 1
      }
      storm.df[i, 'ar'] <- ar
    }
  }

  ## make a catalog
  catalog <- data.frame(AR = 1:max(storm.df$ar))
  for (ar in 1:max(storm.df$ar)) {
    catalog$start_day[ar] <- paste(min(ymd(storm.df[storm.df$ar == ar, 'date'])))
    catalog$end_day[ar] <- paste(max(ymd(storm.df[storm.df$ar == ar, 'date'])))
    catalog$IVT_max[ar] <- max(storm.df[storm.df$ar == ar, 2:(ncol(storm.df)-2)])
    catalog$duration[ar] <- max(apply(hour.df[storm.df$ar == ar, 2:(ncol(storm.df)-2)], 2, sum))
  }

  ## set up runoff dataframe
  param <- c('00060', '00065'); names(param) <- c('discharge_cfs', 'gageht_ft')
  statcode <- c('00001', '00002', '00003', '00008'); names(statcode) <- c('max', 'min', 'mean', 'median')
  site.runoff <- readNWISdv(gauge, parameterCd = param, statCd = statcode, startDate = '1980-01-01') %>%
    renameNWISColumns() %>%
    full_join(readNWISsite(gauge), by = 'site_no') %>%
    mutate(Runoff_mmday = Flow / drain_area_va / 5280^2 * (60*60*24) * (25.4*12)) %>%
    dplyr::select(Runoff_mmday, Date) %>%
    group_by(Date) %>%
    summarize(Runoff_mmday = Mean(Runoff_mmday))

  pb <- txtProgressBar(min = 0, max = nrow(catalog), style = 3)
  for (ar in 1:nrow(catalog)) {
    datelist <- seq(ymd(catalog$start_day[ar]), ymd(catalog$end_day[ar]), 'days')
    ## get storm-total precip
    precip <- 0
    for (i in 1:length(datelist)) {
      d <- datelist[i]
      cpc_precip <- cpc_prcp(d)
      cpc_precip$lon <- cpc_precip$lon-360
      cpc_precip <- suppressWarnings(rasterFromXYZ(cpc_precip, crs = "+proj=longlat +datum=NAD83 +no_defs"))
      # precip <- precip + velox(cpc_precip)$extract(aoi, small = TRUE) %>% lapply(mean) %>% unlist
      # precip <- precip + exact_extract(cpc_precip, aoi, 'mean', progress = FALSE)
      precip <- precip + raster::extract(cpc_precip, aoi, mean, method = 'bilinear', small = TRUE)
    }
    catalog[ar, 'precip'] <- precip

    ## get storm-total runoff
    catalog[ar, 'runoff'] <- site.runoff %>%
      subset(Date %in% datelist) %>%
      select(Runoff_mmday) %>% Sum
    setTxtProgressBar(pb, ar)
  }
  return(catalog)
}

## @param
## catalog (data.frame): catalog of ARs occurring in region of interest
## n.AR (integer): number of ARs to generate
## intensity.threshold (double): keep only AR events above this percentile (recommended 0.9)

## @return
## AR (data.frame): list of synthetic ARs

## assumptions: generate 10x more points than needed, then pick the highest ones

generate_AR <- function(catalog, n.AR = 1, intensity.threshold = 0.9) {
  ## create the Gaussian copula
  create_AR_copula(catalog)

  ## generate new points from the copula
  z <- rmvnorm(n = round(n.AR/(1-intensity.threshold)), mean = c(0,0), sigma = RHO) %>% as.data.frame
  dz <- 1/(1-intensity.threshold)
  temp <- data.frame(id = 1:n.AR, base = floor(dz), add = 0)
  temp$add[sample(1:n.AR, size = nrow(z)-floor(dz)*n.AR, replace = FALSE)] <- 1

  z <- z %>%
    mutate(id = temp %>% apply(1, function(x) rep(x[1], x[2]+x[3])) %>% unlist %>% c,
           rank1 = rank(V1), rank2 = rank(V2))
  z$rank <- rowMeans(z[,c('rank1', 'rank2')])
  z <- z %>%
    group_by(id) %>%
    summarize(index = which.max(rank),
              v1 = V1[index], v2 = V2[index]) %>%
    dplyr::select(v1, v2) %>%
    as.matrix
  u <- pnorm(z)
  AR <- data.frame(n.AR = 1:n.AR,
                   duration = qlnorm(u[,1],
                                     meanlog = param_duration['meanlog'],
                                     sdlog = param_duration['sdlog']),
                   IVT_max = qgumbel(u[,2],
                                     loc = param_IVT['loc'],
                                     scale = param_IVT['scale']))
  return(AR)
}




## @param
## catalog (data.frame): catalog of ARs occurring in region of interest

## @return
## RHO (matrix): 2x2 correlation matrix
## param_duration (vector): lognormal parameters for duration
## param_IVT (vector): Type I Gumbel parameters for max IVT

## note: this will throw a warning if the chosen distributions are not a good fit, as measured by the K-S test

create_AR_copula <- function(catalog) {
  ## find spearman rank coefficient
  rho_s <- cor.test(catalog$IVT_max, catalog$duration, method = 'spearman')$estimate

  ## convert spearman rank coefficient to linear correlation
  rho <- 2*sin(pi*rho_s/6)
  RHO <<- matrix(c(1, rho, rho, 1), nrow = 2, ncol = 2)

  ## find lognormal parameters for duration
  sdlog <- sqrt(log((sd(catalog$duration)/mean(catalog$duration))^2 + 1))
  meanlog <- log(mean(catalog$duration)) - sdlog^2/2
  param_duration <<- c('meanlog' = meanlog, 'sdlog' = sdlog)

  ## find Gumbel parameters for IVT
  alpha <- pi/(sd(catalog$IVT_max)*sqrt(6))
  u <- mean(catalog$IVT_max) - 0.5772/alpha
  param_IVT <<- c('loc' = u, 'scale' = 1/alpha)

  ## do the fits pass the K-S test?
  fit <- c()
  d_crit <- 1.36/sqrt(nrow(catalog))

  x <- sort(catalog$duration + rnorm(nrow(catalog), sd = 2))
  cdf_x <- (1:length(x))/(length(x)+1)
  cdf_lognormal <- plnorm(x, meanlog = meanlog, sdlog = sdlog)
  fit['duration'] <- max(abs(cdf_lognormal - cdf_x)) / d_crit

  x <- sort(catalog$IVT_max)
  cdf_x <- (1:length(x))/(length(x)+1)
  cdf_gumbel <- pgumbel(x, loc = u, scale = 1/alpha)
  fit['IVT'] <- max(abs(cdf_gumbel - cdf_x)) / d_crit

  if (fit['duration'] > 1) {
    warning('Logarithmic distribution is not a good fit to duration in this study region')}
  if (fit['IVT'] > 1) {
    warning('Gumbel extreme value distribution is not a good fit to maximum IVT in this study region')
  }
}




## @param
## catalog (data.frame): catalog of ARs occurring in region of interest
## add.historic (data.frame): option to add historic ARs to exceedance plot, colored black
## add.synthetic (data.frame): option to add synthetic ARs to exceedance plot, colored red

## @return
## print plot to console

plot_AR_copula <- function(catalog, add.historic = NA, add.synthetic = NA) {
  ## creat exceedance plot
  IVT_seq <- seq(0, max(catalog$IVT_max)+50, 10)
  dur_seq <- seq(0, max(catalog$duration)+5, 1)
  exceed <- matrix(nrow = length(IVT_seq), ncol = length(dur_seq))
  for (i in 1:length(IVT_seq)) {
    for (j in 1:length(dur_seq)) {
      exceed[i,j] <- catalog[catalog$IVT_max >= IVT_seq[i] & catalog$duration >= dur_seq[j],] %>% nrow
    }
  }
  exceed <- data.frame(exceed)
  names(exceed) <- dur_seq
  exceed <- cbind(IVT_max = IVT_seq, exceed)
  exceed <- melt(exceed, id.vars = 'IVT_max', variable.name = 'duration', value.name = 'freq')
  exceed$duration <- toNumber(exceed$duration)

  g <- ggplot() +
    geom_raster(data = exceed, aes(x = IVT_max, y = duration, fill = freq/40)) +
    ggtitle('Storm Exceedance Totals') +
    labs(x = 'Max Storm IVT (kg/m/s)', y = 'Storm Duration (hrs)', fill = 'Storms/year') +
    lims(x = c(250, NA)) +
    coord_fixed(ratio = 8) +
    scale_fill_viridis_c()

  if (!is.na(add.historic)) {
    g <- g + geom_point(data = add.historic,
                        aes(x = IVT_max, y = duration), shape = 21, fill = 'green', color = 'white')
  }
  if(!is.na(add.synthetic)) {
    g <- g + geom_point(data = add.synthetic,
                        aes(x = IVT_max, y = duration), shape = 21, fill = 'red', color = 'white')
  }

  print(g)
}


print('2. generate ARs')

## decide which USGS gauges are most representative of flows in the study area
gauge <- c(11464000, 11467000)
catalog <- generate_AR_catalog(outlet, gauge)
write.csv(x = catalog, file = './PARRA/_results/catalog.csv')
# catalog <- read.csv('./PARRA/_results/catalog.csv')

AR <- generate_AR(catalog, n.AR = 100)
write.csv(x = AR, file = './PARRA/_results/AR.csv')
