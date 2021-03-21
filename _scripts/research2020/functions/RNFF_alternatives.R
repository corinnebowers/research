
# ## open soil moisture file
# library(ncdf4)
# soil_nc <- nc_open('D:/Research/_data/soilmoisture/soilw.mon.mean.v2.nc')
# soil_lat <- ncvar_get(soil_nc, 'lat')
# soil_lon <- ncvar_get(soil_nc, 'lon') - 180
# soil_time <- ymd('1800-01-01') + days(ncvar_get(soil_nc, 'time'))
# soil_time <- data.frame(month = month(soil_time), year = year(soil_time))
# soil <- ncvar_get(soil_nc, 'soilw')
# nc_close(soil_nc)
# 
# catalog$soilmoisture <- NA
# pb <- txtProgressBar(min = 0, max = nrow(catalog), style = 3)
# for (ar in 1:nrow(catalog)) {
#   start <- ymd(catalog$start_day[ar]) - days(1)
#   end <- ymd(catalog$end_day[ar]) + days(1)
#   wy <- catalog$wateryear[ar]
#   
#   ## soil moisture
#   SM_stack <- raster::stack()
#   monthrecord <- ifelse(month(start) <= month(end),
#                         length(month(start):month(end)),
#                         length((month(start)-12):month(end))) - 1
#   startmonth <- ymd(paste(year(start), month(start), '1', sep = '-'))
#   for (i in 0:monthrecord) {
#     mo <- month(startmonth + months(i))
#     yr <- year(startmonth + months(i))
#     index <- (1:nrow(soil_time))[soil_time$month == mo & soil_time$year == yr]
#     SM_raster <- raster(t(soil[,,index]), xmn = min(soil_lon), xmx = max(soil_lon), 
#                         ymn = min(soil_lat), ymx = max(soil_lat))
#     SM_stack <- raster::stack(SM_stack, SM_raster)
#   }
#   SM_avg <- mean(SM_stack); crs(SM_avg) <- projection(california)
#   catalog$soilmoisture[ar] <- exact_extract(SM_avg, inlet, 'mean', progress = FALSE)
#   
#   setTxtProgressBar(pb, ar)
# }


## @param
## precip (data.frame): list of synthetic precipitation events
## catalog (data.frame): catalog of ARs occurring in region of interest
## n.runoff (integer): number of runoff events to generate per precipitation event
## boot (integer): number of samples to bootstrap confidence intervals for CN parameters
## plot (logical): whether or not to show a plot

## @return
## runoff (data.frame): list of synthetic runoff events


generate_runoff1 <- function(precip, catalog, n.runoff = 1, boot = 1e3, plot = TRUE) {

  ## calculate n.AR and n.precip
  n.AR <- max(precip$n.AR)
  n.precip <- max(precip$n.precip)
  
  ## calculate seasondays, the number of days since 10/1/XX
  catalog <- catalog %>% 
    mutate(wateryear = year(ymd(start_day)) + 
             ifelse(month(start_day) %in% c(10:12), 1, 0),
           seasondays = (ymd(start_day) - ymd(paste(wateryear-1, '10', '1', sep = '-'))) %>% 
             toNumber)
  
  ## calculate the curve number (CN) by the statistical method
  
  ## find the characteristics of the annual max storm
  wateryear.df <- data.frame(wateryear = unique(catalog$wateryear), precip = NA, runoff = NA)
  for (wy in 1:nrow(wateryear.df)) {
    index <- catalog %>% 
      subset(wateryear == unique(wateryear)[wy]) %>% 
      dplyr::select(precip) %>% unlist %>% which.max
    index.value <- catalog %>% 
      subset(wateryear == unique(wateryear)[wy]) %>% 
      subset(1:nrow(.) == index)
    wateryear.df[wy, 'precip'] <- index.value$precip / 25.4
    wateryear.df[wy, 'runoff'] <- index.value$runoff / 25.4
    wateryear.df[wy, 'IVT_max'] <- index.value$IVT_max
    wateryear.df[wy, 'duration'] <- index.value$duration
  }

  ## bootstrap confidence intervals for CN parameters
  pb <- txtProgressBar(min = 0, max = boot, style = 3)
  S.best <- rep(NA, boot)
  S.sd <- rep(NA, boot)
  for (b in 1:boot) {
    index <- sample(1:nrow(wateryear.df), size = nrow(wateryear.df), replace = TRUE)
    P <- wateryear.df$precip[index]
    Q <- wateryear.df$runoff[index]
    S.val <- ifelse(P>0 & Q>0, 5*(P + 2*Q - sqrt(5*P*Q + 4*Q^2)), NA)
    S.best[b] <- mean(log(S.val), na.rm = TRUE)
    S.sd[b] <- sd(log(S.val), na.rm = TRUE)
    setTxtProgressBar(pb, b)
  }

  ## sample a CN value from the normal distribution
  runoff <- 
    expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.runoff = 1:n.runoff) %>% 
    full_join(precip, by = c('n.AR', 'n.precip')) %>% 
    mutate(precip_in = precip_mm/25.4, 
           p = (1-(seasondays)/max(seasondays)) %>% qunif(p, min = 0.1, max = 0.9),
           S.mean = rnorm(nrow(.), mean = mean(S.best), sd = sd(S.best)),
           S.sd = rnorm(nrow(.), mean = mean(S.sd), sd = sd(S.sd)),
           S = qnorm(p = p, mean = S.mean, sd = S.sd),
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

