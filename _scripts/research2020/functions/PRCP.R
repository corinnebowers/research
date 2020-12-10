
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
  
  quant.result <- list()
  pb <- txtProgressBar(min = 0, max = n.AR, style = 3)
  for (ar in 1:n.AR) {
    quant.result[[ar]] <- data.frame(tau = tau, fit = NA, lower = NA, higher = NA)
    for (i in 1:length(tau)) {
      quant.boot <- predict(quant.model[[i]], newdata = AR[ar,], 
                            type = 'percentile', interval = 'confidence', se = 'boot')
      quant.result[[ar]][i, 2:4] <- quant.boot
    }
    setTxtProgressBar(pb, ar)
  }
  
  ## use a triangle distribution around the precipitation confidence bounds
  precip <- expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip)
  precip$q <- sample(seq(precip.threshold, 1-dx, dx), size = n.AR * n.precip, replace = TRUE)
  if (!quiet) { pb <- txtProgressBar(min = 0, max = n.precip, style = 3) }
  for (i in 1:nrow(precip)) {
    index <- which(round(quant.result[[precip$n.AR[i]]]$tau, 2) == round(precip$q[i], 2))
    precip$precip_mm[i] <- rltriangle(n = 1, a = quant.result[[precip$n.AR[i]]][index, 'lower'], 
                                      b = quant.result[[precip$n.AR[i]]][index, 'higher'], 
                                      c = quant.result[[precip$n.AR[i]]][index, 'fit'])
    if (!quiet) { setTxtProgressBar(pb, i) }
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
