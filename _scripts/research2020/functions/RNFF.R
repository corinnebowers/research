
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
      dplyr::select(precip) %>% unlist %>% which.max
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

