
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
             .packages = c('quantreg', 'triangle', 'dplyr'), 
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
