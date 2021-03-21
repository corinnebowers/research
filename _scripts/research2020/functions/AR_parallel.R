
## @param
## aoi (sf): polygon bounding region of interest
## gauge (vector): list of USGS gauge ID(s) that best represent flows within the study area
   ## note: if more than one gauge is listed, the reported flow will be the average
## ar.threshold (double): controls what % of MERRA cells need to register an AR for it to count (recommended 0.3-0.5) 

## @return
## catalog (data.frame): catalog of ARs occurring in region of interest

generate_AR_catalog <- function(aoi, gauge, ar.threshold = 0.5) {
  ## data needed:
  load('D:/Research/_data/grid_catalog.Rdata')  # list of ARs by cell (ar_grid) and a spatial tracker (tracker)

  ## find which cells cross the area of interest
  tracker.raster <- tracker[,c(2,1,3,4)]
  tracker.raster <- rasterFromXYZ(tracker.raster, crs = "+proj=longlat +datum=NAD83 +no_defs") 
  tracker.id <- rasterize(aoi, tracker.raster, getCover = TRUE) %>% 
    as.data.frame(xy = TRUE) %>% 
    subset(layer > 0) %>% 
    left_join(tracker, by = c('x'='lon', 'y'='lat')) %>% 
    dplyr::select(step) %>% 
    unlist %>% unname %>% sort
  tracker.id <- tracker.id[!(tracker.id %in% c(827,1185))]  ## these ones cause issues
  
  ## identify all AR storms in the region of interest
  storm.df <- data.frame(date = seq(ymd('1980-01-01'), ymd('2019-12-31'), 'days'))
  storm.df[,paste(tracker.id)] <- 0
  hour.df <- storm.df
  for (id in 1:length(tracker.id)) {
    ar_list <- ar_grid[[tracker.id[id]]]
    ar_list$total_hours <- (ymd_h(paste(ar_list$end_date, ar_list$end_hour)) -
                              ymd_h(paste(ar_list$start_date, ar_list$start_hour))) %>% as.numeric(units = 'hours')
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
  pb <- txtProgressBar(min = 0, max = max(storm.df$ar), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  catalog <- 
    foreach (ar = 1:max(storm.df$ar), 
             .combine = 'rbind',
             .packages = c('dplyr', 'dataRetrieval', 'rnoaa', 'lubridate', 'exactextractr', 'raster', 'sf'),
             .options.snow = opts) %dopar% {
               
      start_day = paste(min(ymd(storm.df[storm.df$ar == ar, 'date'])))
      end_day = paste(max(ymd(storm.df[storm.df$ar == ar, 'date'])))
      
      datelist <- seq(ymd(start_day), ymd(end_day), 'days')
      precip <- 0
      for (i in 1:length(datelist)) {
        d <- datelist[i]
        cpc_precip <- cpc_prcp(d)
        cpc_precip$lon <- cpc_precip$lon-360
        cpc_precip <- suppressWarnings(rasterFromXYZ(cpc_precip, crs = "+proj=longlat +datum=NAD83 +no_defs"))
        precip <- precip + exact_extract(cpc_precip, aoi) %>% lapply(mean) %>% unlist
      }
      
      param <- c('00060', '00065'); names(param) <- c('discharge_cfs', 'gageht_ft')
      statcode <- c('00001', '00002', '00003', '00008'); names(statcode) <- c('max', 'min', 'mean', 'median')
      runoff <- readNWISdv(gauge, parameterCd = param, statCd = statcode, startDate = '1980-01-01') %>% 
        renameNWISColumns() %>% 
        full_join(readNWISsite(gauge), by = 'site_no') %>% 
        mutate(Runoff_mmday = Flow / drain_area_va / 5280^2 * (60*60*24) * (25.4*12)) %>% 
        dplyr::select(Runoff_mmday, Date) %>% 
        group_by(Date) %>% 
        summarize(Runoff_mmday = mean(Runoff_mmday, na.rm = TRUE)) %>% 
        subset(Date %in% datelist) %>% 
        dplyr::select(Runoff_mmday) %>% 
        unlist %>% 
        sum(na.rm = TRUE)
      
      data.frame(AR = ar, 
                 start_day = start_day,
                 end_day = end_day, 
                 IVT_max = max(storm.df[storm.df$ar == ar, 2:(ncol(storm.df)-2)]), 
                 duration = max(apply(hour.df[storm.df$ar == ar, 2:(ncol(storm.df)-2)], 2, sum)),
                 precip = precip,
                 runoff = runoff)
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
  z <- rmvnorm(round(n.AR/(1-intensity.threshold)), mu = c(0,0), sigma = RHO) %>% 
    # t %>% c %>% 
    # array(dim = c(2, n.AR, 10)) %>% 
    # apply(c(1,2), max) %>% t
    as.data.frame %>% 
    subset(V1 > 0 & V2 > 0) %>%
    mutate(eigen = sqrt(V1^2 + V2^2))
  z <- z[order(z$eigen, decreasing = TRUE),][1:n.AR,1:2] %>% as.matrix
  u <- pnorm(z)
  AR <- data.frame(n.AR = 1:n.AR, 
                   duration = qlnorm(u[,1], meanlog = param_duration['meanlog'], sdlog = param_duration['sdlog']), 
                   IVT_max = qgumbel(u[,2], loc = param_IVT['loc'], scale = param_IVT['scale']))
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
