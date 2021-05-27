
###################################################################################################

## @param
## precip (data.frame): list of synthetic precipitation events
## catalog (data.frame): catalog of ARs occurring in region of interest
## probabilistic (logical): choice to incorporate uncertainty
## n.runoff (integer): number of runoff events to generate per precipitation event
## boot (integer): number of bootstrap samples to draw 

## @return
## runoff (data.frame): list of synthetic runoff events

generate_runoff <- function(precip, catalog, probabilistic = FALSE, n.runoff = 1, 
                            ic.threshold = 0.5, boot = 1e3) {
  
  ## fix input parameters
  if (!probabilistic) n.runoff <- 1

  ## calculate n.AR and n.precip
  n.AR <- max(precip$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)

  print('converting precipitation to runoff...')
  
  ## calculate seasondays, the number of days since 10/1/XX
  catalog <- catalog %>% 
    mutate(wateryear = year(ymd(start_day)) + 
             ifelse(month(start_day) %in% c(10:12), 1, 0),
           seasondays = (ymd(start_day) - ymd(paste(wateryear-1, '10', '1', sep = '-'))) %>% 
             toNumber)
  runoff <- expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.runoff = 1:n.runoff) %>% 
    right_join(precip, by = c('n.AR', 'n.precip')) %>% 
    mutate(precip_in = precip_mm/25.4) %>% 
    mutate(p = runif(nrow(.), min = 0, max = 0.1) + 
             runif(nrow(.), min = ic.threshold, max = 0.9)) %>% 
    mutate(p = 1-p)
  
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
  
  if (probabilistic) {
    ## bootstrap confidence intervals for CN parameters
    S.best <- rep(NA, boot)
    S.sd <- rep(NA, boot)
    for (b in 1:boot) {
      index <- sample(1:nrow(wateryear.df), size = nrow(wateryear.df), replace = TRUE)
      P <- wateryear.df$precip[index]
      Q <- wateryear.df$runoff[index]
      S.val <- ifelse(P>0 & Q>0, 5*(P + 2*Q - sqrt(5*P*Q + 4*Q^2)), NA)
      S.best[b] <- mean(log(S.val), na.rm = TRUE)
      S.sd[b] <- sd(log(S.val), na.rm = TRUE)
    }
    ## sample a CN value from the normal distribution
    runoff <- runoff %>% 
      mutate(S.mean = rnorm(nrow(.), mean = mean(S.best), sd = sd(S.best)),
             S.sd = rnorm(nrow(.), mean = mean(S.sd), sd = sd(S.sd)),
             S = qnorm(p = p, mean = S.mean, sd = S.sd),
             CN = 1000/(10+exp(S)))
  } else {
    ## calculate CN deterministically
    P <- wateryear.df$precip
    Q <- wateryear.df$runoff
    S.val <- ifelse(P>0 & Q>0, 5*(P + 2*Q - sqrt(5*P*Q + 4*Q^2)), NA)
    S.mean <- mean(log(S.val), na.rm = TRUE)
    runoff <- runoff %>% mutate(CN = 1000/(10+exp(S.mean)), n.runoff = NA)
  }
  
  ## calculate expected runoff
  runoff <- runoff %>%
    mutate(runoff_in = ifelse(precip_in < 0.2*exp(S.mean), 0, 
                              (precip_in-0.2*exp(S.mean))^2/(precip_in+0.8*exp(S.mean))),
           runoff_mm = runoff_in*25.4) %>% 
    dplyr::select(n.AR, n.precip, n.runoff, IVT_max, duration, precip_mm, runoff_mm)
  if (prcp.memory) runoff$n.precip <- NA
  return(runoff)
}


###################################################################################################

## @param
## runoff (data.frame): list of synthetic runoff events
## catalog (data.frame): catalog of ARs occurring in region of interest
## probabilistic (logical): choice to incorporate uncertainty
## n.hydro (integer): number of hydrographs to generate per runoff event

## @return
## hydrograph (data.frame): list of synthetic hydrographs

generate_hydrograph <- function(precip, runoff, catalog, probabilistic = FALSE, n.hydro = 1) {

  ## fix input parameters
  if (!probabilistic) n.hydro <- 1

  ## calculate n.AR, n.precip, n.runoff
  n.AR <- max(runoff$n.AR)
  if (any(is.na(precip$n.precip))) {
    prcp.memory <- TRUE
    precip$n.precip <- 1
    runoff$n.precip <- rep(1, nrow(runoff))
  } else prcp.memory <- FALSE
  n.precip <- max(precip$n.precip)
  if (any(is.na(runoff$n.runoff))) {
    rnff.memory <- TRUE
    runoff$n.runoff <- 1
  } else rnff.memory <- FALSE
  n.runoff <- max(runoff$n.runoff)

  # print('converting runoff to streamflow...')
  
  catalog <- catalog %>%
    mutate(Qp_m3s = Qp/(3.28084^3)) %>% #convert to metric
    mutate(wateryear = year(start_day) + ifelse(month(start_day) %in% 10:12, 1, 0),
           seasondays = toNumber(ymd(start_day) - ymd(paste(wateryear-1, 10, 1, sep = '-'))))
  fun <- Qp_m3s ~ I(sqrt(runoff)) + runoff*duration + precip:runoff + IVT_max:precip

  ## fit linear regression to predict Qp
  model <- lm(fun, data = catalog)

  # ## fit quantile regression to predict Qp
  # temp <-
  #   foreach (t = seq(0, 1, 0.01), .combine = 'rbind') %do% {
  #     model <- rq(fun, data = catalog, tau = t)
  #     prediction <- predict(model)
  #     prediction <- ifelse(prediction < 0, 0, prediction)
  #     c(tau = t, RMSE = RMSE(sort(prediction), sort(catalog$Qp_m3s)))
  #   }
  # tau.best <- temp %>% as.data.frame %>% .[which.min(.$RMSE), 'tau']
  # model <- rq(fun, data = catalog, tau = tau.best)

  ## find prediction uncertainty
  predictions <- 
    predict(model, runoff %>% rename(runoff = runoff_mm, precip = precip_mm)) %>% 
    as.data.frame %>% setNames('fit') %>% 
    mutate(se.predict = 
             predict.se(model, fitdata = catalog, 
                        newdata = runoff %>% rename(runoff = runoff_mm, precip = precip_mm))) %>% 
    cbind(runoff %>% dplyr::select(n.AR, n.precip, n.runoff))
  
  ## generate realizations of Qp
  if (probabilistic) {
    hydrograph <- 
      expand.grid(n.AR = 1:n.AR, 
                  n.precip = 1:n.precip,
                  n.runoff = 1:n.runoff, 
                  n.hydro = 1:n.hydro) %>% 
      right_join(runoff %>% dplyr::select(n.AR, n.precip, n.runoff), 
                 by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
      left_join(predictions, by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
      mutate(Qp = rnorm(nrow(.), mean = fit, sd = se.predict))
  } else {
    hydrograph <- predictions %>% mutate(n.hydro = NA) %>% rename(Qp = fit)
  }
  hydrograph <- hydrograph %>% mutate(Qp = ifelse(Qp < 0, 0, Qp))
  
  ## generate realizations of tp
  ## (note: for more information on how this was calculated, see hydrographs.Rmd)
  tp.fit <- c('meanlog' = 3.0480540, 'sdlog' = 0.3886569)
  hydrograph <- hydrograph %>% 
    mutate(tp = rlnorm(nrow(.), meanlog = tp.fit['meanlog'], sdlog = tp.fit['sdlog']))
  
  ## return hydrograph
  hydrograph <- hydrograph %>% 
    right_join(runoff, by = c('n.AR', 'n.precip', 'n.runoff')) %>% 
    dplyr::select(n.AR, n.precip, n.runoff, n.hydro, IVT_max, duration, 
                  precip_mm, runoff_mm, Qp_m3s = Qp, tp_hrs = tp)
  if (prcp.memory) {
    runoff$n.precip <- rep(NA, nrow(runoff))
    hydrograph$n.precip <- rep(NA, nrow(hydrograph))
  }
  if (rnff.memory) hydrograph$n.runoff <- rep(NA, nrow(hydrograph))
  return(hydrograph)
}
