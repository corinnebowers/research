
#### setup information ############################################################################

## this code generates .bdy and .bci files for LISFLOOD event analysis.

## load packages
require(lhs)
require(dplyr)

## input parameters
n <- 1                  #number of LHS samples
Qp <- 112000/mft^3      #peak flow (m3s)
baseflow <- 3           #baseflow (m3s)
# fileloc <- './bci_bdy/' #location to save files

simlength <- 30*24*3600 #length of hydrograph timeseries (sec)
spinup <- 15*24*3600    #length of baseflow before hydrograph (sec)

tp <- 40*3600#98.89811*3600     #time to peak flow (sec)	 
m <- 4#3.659283           #hydrograph shape parameter

edgewidth <- mean(edge.in$layer)      #channel width at inlet (m)

## constants
mft <- 3.28084


#### generate samples #############################################################################
setwd('C:/Users/cbowers/Desktop/LISFLOOD/new_rasters/')
# load('fitobjects.Rdata')
# load('edges.Rdata')
# samples <- improvedLHS(n = n, k = 4) 
# samples <- samples %>% 
#   as.data.frame %>%
#   setNames(paste0('x', 1:4)) %>%
#   mutate(SGCn = cut(x1, breaks = 5, labels = seq(0.035, 0.075, 0.01)),
#          SGCr = cut(x2, breaks = 3, labels = c(0.01, 0.04, 0.12)),
#          tp = qlnorm(x3, meanlog = tp.fit$estimate[1], sdlog = tp.fit$estimate[2]),
#          m = qlnorm(x4, meanlog = m.fit$estimate[1], sdlog = m.fit$estimate[2])) %>% 
#   select(SGCn, SGCr, tp, m)
# write.table(samples, file = 'samples.txt',
#             row.names = FALSE, quote = FALSE, sep = '\t')


#### generate bci and bdy files ###################################################################
for (i in 1:n) {
  ## define LHS parameters
  # tp <- samples[i, 'tp'] * 3600  #seconds
  # Qp <- samples[i, 'r'] / 1e3 * drainarea / tp  #m3/s
  
  ## calculate storm hydrograph
  t <- seq(0, simlength, 60)
  q <- ((t/tp)^m * exp(m*(1-(t/tp)))) * Qp
  q <- cbind(q, baseflow) %>% apply(1, max)
  
  ## add spinup time
  t <- c(seq(0, spinup-1, 3600), spinup+t)
  q <- c(rep(baseflow, length(seq(0, spinup-1, 3600))), q)
  
  ## convert to m2/s
  q <- q / edgewidth
  
  ## write out files
  bdy <- matrix(c('LISFLOOD', NA, 'rp100', NA, length(t), 'seconds'),
                byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
  write.table(bdy, file = 'C:/Users/cbowers/Desktop/rp100.bdy',
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  bci <- data.frame(matrix(
    c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', 'rp100',
      'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
    nrow = 2, byrow = TRUE))
  write.table(bci, file = './rp100.bci',
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
}
