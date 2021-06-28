
## goal: find a conversion from 11464000 to 11463500 

# ## find out when data is available for 11463500
# param <- c('00060', '00065'); names(param) <- c('discharge_cfs', 'gageht_ft')
# statcode <- c('00001', '00002', '00003', '00008'); names(statcode) <- c('max', 'min', 'mean', 'median')
# gauge <- c(11463500, 11463682, 11463980, 11464000)
# whatNWISdata(siteNumber = gauge, service = 'iv', parameterCd = param, statCd = statcode)
# 
# ## download data for 11463500 and 11464000
# num_cores <- 5
# cl <- parallel::makeCluster(num_cores)
# registerDoSNOW(cl)
# pb <- txtProgressBar(min = 0, max = 2020-2013, style = 3)
# data <- 
#   foreach(wy = 2013:2020, 
#     .combine = 'rbind',
#     .packages = c('tidyverse', 'dataRetrieval'),
#     .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
#     readNWISdata(
#       sites = c(11463500, 11464000), 
#       parameterCd = param, 
#       startDate = paste(wy-1, 10, 1, sep = '-'), 
#       endDate = paste(wy, 4, 1, sep = '-'), 
#       service = 'iv', tz = 'America/Los_Angeles') %>% 
#       renameNWISColumns %>% 
#       pivot_wider(id_cols = dateTime, names_from = site_no, 
#                   values_from = Flow_Inst, values_fn = mean)
#   }
# stopCluster(cl)
# data <- data %>% 
#   arrange(dateTime) %>% 
#   mutate(wy = year(dateTime) + ifelse(month(dateTime) %in% 10:12, 1, 0))
# 
# ## find the relationship between the two gauges
# crosscorr <- matrix(nrow = 201, ncol = 8)
# for (x in 2013:2020) {
#   temp <- data %>% filter(wy == x) 
#   crosscorr[,x-2012] <- 
#     ccf(temp$`11464000`, temp$`11463500`, na.action = na.pass, lag.max = 100)$acf
# }
# lag <- crosscorr %>% 
#   as.data.frame %>% 
#   setNames(paste0('wy', 2013:2020)) %>% 
#   mutate(lag = -100:100) %>% 
#   pivot_longer(cols = -lag) %>% 
#   # ggplot() + geom_line(aes(x = lag, y = value, group = name, color = name))
#   mutate(wy = toNumber(gsub('wy', '', name))) %>% 
#   filter(wy > 2015) %>% 
#   # ggplot() + geom_line(aes(x = lag, y = value, group = wy, color = wy))
#   group_by(lag) %>% 
#   summarize(acf = mean(value)) %>%
#   # ggplot() + geom_line(aes(x = lag, y = acf))
#   filter(acf == max(acf)) %>% 
#   pull(lag)
# data.lag <- data %>% 
#   group_by(wy) %>% 
#   mutate(n = length(wy), index = 1:n) %>%
#   mutate(gage.lag = case_when(index <= (n-lag) ~ `11464000`[index+lag])) 
# # ggplot(data.lag) + geom_point(aes(x = gage.lag, y = `11463500`))
# summary(lm(`11463500` ~ gage.lag+0, data = data.lag))

## conclusion: gauge 11463500 is ~66% of gauge 11464000, with a time lag of 4.5 hours


###################################################################################################

## @param
## aoi (sf): polygon bounding region of interest
## gauge (vector): list of USGS gauge ID(s) that best represent flows within the study area
   ## note: if more than one gauge is listed, the reported flow will be the average
## ar.threshold (double): controls what % of MERRA cells need to register an AR for it to count (recommended 0.3-0.5)

## @return
## catalog (data.frame): catalog of ARs occurring in region of interest

generate_AR_catalog <- function(aoi, gauge, ar.threshold = 0.5) {
  print('getting IVT & duration information...')

  ## data needed:
  load('./_data/grid_catalog.Rdata')  
  # contains list of ARs by cell (ar_grid) and a spatial tracker (tracker)

  ## find which cells cross the area of interest
  tracker.raster <- tracker[,c(2,1,3,4)]
  tracker.raster <- rasterFromXYZ(tracker.raster, crs = "+proj=longlat +datum=NAD83 +no_defs")
  tracker.id <- raster::rasterize(aoi %>% st_transform(crs(tracker.raster)), 
                                  tracker.raster, getCover = TRUE) %>%
    as.data.frame(xy = TRUE) %>%
    subset(layer > 0) %>%
    left_join(tracker, by = c('x'='lon', 'y'='lat')) %>%
    dplyr::select(step) %>%
    unlist %>% unname %>% sort
  tracker.id <- tracker.id[!(tracker.id %in% c(827,1185))]  ## these ones cause issues
  message('Ignore the errors above, they are artifacts of the terra package')
  message('(see https://github.com/rspatial/terra/issues/30)')

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
  
  print('getting precipitation information...')

  ## get precipitation information
  pb <- txtProgressBar(min = 0, max = nrow(catalog), style = 3)
  catalog$precip <- 
    foreach (
      ar = 1:nrow(catalog), 
      .combine = 'c',
      .packages = c('rnoaa', 'exactextractr', 'lubridate', 'foreach', 'raster', 'dplyr'),
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        datelist <- seq(ymd(catalog$start_day[ar]), ymd(catalog$end_day[ar]), 'days')
        foreach (i = 1:length(datelist), .combine = '+') %do% {
          datelist[i] %>%
            cpc_prcp %>%
            mutate(lon = lon-360) %>%
            rasterFromXYZ(crs = "+proj=longlat +datum=NAD83 +no_defs") %>%
            exact_extract(aoi, 'mean', progress = FALSE) %>%
            unlist
        }
      }
  cat('\n')

  print('getting runoff & streamflow information...')
  
  ## get runoff information
  param <- c('00060', '00065'); names(param) <- c('discharge_cfs', 'gageht_ft')
  statcode <- c('00001', '00002', '00003', '00008'); names(statcode) <- c('max', 'min', 'mean', 'median')
  sites <- readNWISsite(gauge)
  ar.start <- which(year(catalog$start_day) >= 1987)[1]  #when daily records start for gage 11464000
  pb <- txtProgressBar(min = 0, max = nrow(catalog)-ar.start, style = 3)
  flow.catalog <- 
    foreach(
      ar = ar.start:nrow(catalog), 
      .packages = c('lubridate', 'dataRetrieval', 'foreach', 'raster', 'dplyr', 'tidyr'), 
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        site.runoff <- 
          readNWISdata(
            sites = gauge, parameterCd = param,
            startDate = ymd(catalog$start_day[ar]), 
            endDate = ymd(catalog$end_day[ar])+days(1),
            service = 'iv', tz = 'America/Los_Angeles') %>% 
          renameNWISColumns
        if (nrow(site.runoff) == 0 | !('Flow_Inst' %in% names(site.runoff))) {
          NA
        } else {
          temp <- site.runoff %>% 
            pivot_wider(id_cols = dateTime, 
                        names_from = site_no, names_prefix = 'x', 
                        values_from = Flow_Inst, values_fn = mean) %>% 
            left_join(data.frame(
              dateTime = seq(min(ymd_hms(site.runoff$dateTime)), 
                             max(ymd_hms(site.runoff$dateTime)), by = '15 mins')), ., 
              by = 'dateTime') %>% 
            mutate(index = 1:nrow(.)) %>% 
            mutate(gage_lag = case_when(index <= (nrow(.)-18) ~ x11464000[index+18])) 
          if (!('x11463500' %in% names(temp))) {
            temp <- temp %>% mutate(x11463500 = 0.6646*gage_lag)
          }
          temp %>%
            filter(complete.cases(.)) %>%
            transmute(dateTime, Flow_Inst = x11463500)
        }
      }
  cat('\n')
  
  ## merge runoff information to catalog
  bad <- data.frame(
    bad1 = lapply(flow.catalog, function(x) is.null(nrow(x))) %>% unlist, 
    bad2 = NA) 
  bad[!bad1, 'bad2'] <- lapply(flow.catalog, function(x) nrow(x)==0) %>% unlist
  bad <- bad %>% apply(1, any)
    
  catalog <- 
    flow.catalog[!bad] %>% 
    lapply(function(x) x %>% 
             summarize(Qp = max(Flow_Inst), 
                       Qp.date = date(dateTime[which.max(Flow_Inst)]),
                       sum_flow = sum(Flow_Inst)*60*15) %>% 
             mutate(site_no = '11463500') %>% 
             left_join(sites %>% dplyr::select(site_no, drain_area_va), by = 'site_no') %>% 
             mutate(runoff_ft = sum_flow / (drain_area_va*5280^2),
                    runoff = runoff_ft*25.4*12)) %>% 
    do.call(rbind, .) %>% 
    cbind(AR = (ar.start:nrow(catalog))[!bad]) %>% 
    select(AR, runoff, Qp, Qp.date) %>% 
    as.data.frame %>% 
    left_join(as.data.frame(catalog), ., by = 'AR')
  
  print('getting time to peak information...')
  
  #### insert here ####
  
  print('getting soil moisture information...')
  
  ## open soil moisture file
  soil_nc <- nc_open('D:/Research/_data/soilmoisture/soilw.mon.mean.v2.nc')
  soil_lat <- ncvar_get(soil_nc, 'lat')
  soil_lon <- ncvar_get(soil_nc, 'lon') - 180
  soil_time <- ymd('1800-01-01') + days(ncvar_get(soil_nc, 'time'))
  soil_time <- data.frame(month = month(soil_time), year = year(soil_time))
  soil <- ncvar_get(soil_nc, 'soilw')
  nc_close(soil_nc)
  
  ## attach soil moisture information to catalog events
  catalog <- catalog %>% mutate(sm = NA)
  pb <- txtProgressBar(min = 0, max = nrow(catalog), style = 3)
  for (ar in 1:nrow(catalog)) {
    start <- catalog$start_day[ar]
    end <- catalog$end_day[ar]
    
    SM_stack <- raster::stack()
    monthrecord <- ifelse(month(start) <= month(end),
                          length(month(start):month(end)),
                          length((month(start)-12):month(end))) - 1
    startmonth <- ymd(paste(year(start), month(start), '1', sep = '-'))
    for (i in 0:monthrecord) {
      mo <- month(startmonth + months(i))
      yr <- year(startmonth + months(i))
      index <- (1:nrow(soil_time))[soil_time$month == mo & soil_time$year == yr]
      SM_raster <- raster(t(soil[,,index]), xmn = min(soil_lon), xmx = max(soil_lon), 
                          ymn = min(soil_lat), ymx = max(soil_lat))
      SM_stack <- raster::stack(SM_stack, SM_raster)
    }
    SM_avg <- mean(SM_stack)
    crs(SM_avg) <- projection(california)
    
    catalog$sm[ar] <- exact_extract(SM_avg, aoi %>% st_transform(crs(SM_avg)), fun = 'mean')
    setTxtProgressBar(pb, ar)
  }
  
  ## keep only good records in the catalog
  # catalog.save <<- catalog
  catalog <- catalog %>% 
    filter(complete.cases(.)) %>% 
    filter(duration > 0) %>% 
    mutate(precip = case_when(precip < 0 ~ 0, TRUE ~ precip))
  
  # ## match to tp information from file
  # catalog <- catalog %>% 
  #   left_join(hydrographs %>% 
  #               mutate(Qp.date = ymd(Qp.date)) %>% 
  #               select(Qp.date, tp.hydrograph = tp3), by = 'Qp.date') %>% 
  #   left_join(catalog_tp %>% 
  #               mutate(Qp.date = mdy(peak_date)) %>% 
  #               group_by(Qp.date) %>% 
  #               summarize(tp.catalog = mean(tp)), by = 'Qp.date') %>% 
  #   mutate(tp = cbind(tp.hydrograph, tp.catalog) %>% 
  #            apply(1, function(x) Mean(x) %>% ifelse(is.nan(.), NA, .))) %>% 
  #   select(-tp.hydrograph, -tp.catalog)
  
  return(catalog)
}


###################################################################################################

## @param
## catalog (data.frame): catalog of ARs occurring in region of interest
## n.AR (integer): number of ARs to generate
## intensity.threshold (double): keep only AR events above this percentile (recommended 0.9)
##  (assumptions: generate 10x more points than needed, then pick the highest ones)

## @return
## AR (data.frame): list of synthetic ARs

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
           rank1 = rank(V1), rank2 = rank(V2)) %>% 
    mutate(rank = rowMeans(select(., rank1, rank2))) %>% 
    group_by(id) %>%
    summarize(index = which.max(rank), v1 = V1[index], v2 = V2[index]) %>%
    dplyr::select(v1, v2) %>% as.matrix
  u <- pnorm(z)
  AR <- data.frame(
    n.AR = 1:n.AR,
    duration = qlnorm(u[,1], meanlog = param_duration['meanlog'], sdlog = param_duration['sdlog']),
    IVT_max = qgumbel(u[,2], loc = param_IVT['alpha'], scale = param_IVT['scale'])) %>% 
    mutate(sm = NA)
  return(AR)
}


###################################################################################################

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
  rate <- pi/(sd(catalog$IVT_max)*sqrt(6))
  alpha <- mean(catalog$IVT_max) - 0.5772/rate
  param_IVT <<- c('alpha' = alpha, 'scale' = 1/rate)

  ## do the fits pass the K-S test?
  fit <- c()
  d_crit <- 1.36/sqrt(nrow(catalog))

  x <- sort(catalog$duration + rnorm(nrow(catalog), sd = 2))
  cdf_x <- (1:length(x))/(length(x)+1)
  cdf_lognormal <- plnorm(x, meanlog = meanlog, sdlog = sdlog)
  fit['duration'] <- max(abs(cdf_lognormal - cdf_x)) / d_crit

  x <- sort(catalog$IVT_max)
  cdf_x <- (1:length(x))/(length(x)+1)
  cdf_gumbel <- pgumbel(x, loc = alpha, scale = 1/rate)
  fit['IVT'] <- max(abs(cdf_gumbel - cdf_x)) / d_crit

  if (fit['duration'] > 1) {
    warning('Logarithmic distribution is not a good fit to duration in this study region')}
  if (fit['IVT'] > 1) {
    warning('Gumbel extreme value distribution is not a good fit to maximum IVT in this study region')
  }
}


###################################################################################################

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
    g <- g + 
      geom_point(data = add.historic,
                 aes(x = IVT_max, y = duration), shape = 21, fill = 'green', color = 'white')
  }
  if(!is.na(add.synthetic)) {
    g <- g + 
      geom_point(data = add.synthetic,
                 aes(x = IVT_max, y = duration), shape = 21, fill = 'red', color = 'white')
  }
  print(g)
}
