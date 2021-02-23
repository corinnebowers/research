
#### setup information ############################################################################

## this code generates .bdy and .bci files for LISFLOOD event analysis.

## load packages
require(lhs)
require(dplyr)

## input parameters
n <- 1000  #number of LHS samples
Qp <- 113000  #max flow (m3s)
baseflow <- 4  #baseflow (m3s)
fileloc <- './bci_bdy/' #location to save files


## constants
mft <- 3.28084


#### generate samples #############################################################################
setwd('C:/Users/cbowers/Desktop/LISFLOOD/new_rasters/')
load('fitobjects.Rdata')
load('edges.Rdata')
samples <- improvedLHS(n = n, k = 4) 
samples <- samples %>% 
  as.data.frame %>%
  setNames(paste0('x', 1:4)) %>%
  mutate(SGCn = cut(x1, breaks = 5, labels = seq(0.035, 0.075, 0.01)),
         SGCr = cut(x2, breaks = 3, labels = c(0.01, 0.04, 0.12)),
         tp = qlnorm(x3, meanlog = tp.fit$estimate[1], sdlog = tp.fit$estimate[2]),
         m = qlnorm(x4, meanlog = m.fit$estimate[1], sdlog = m.fit$estimate[2])) %>% 
  select(SGCn, SGCr, tp, m)
write.table(samples, file = 'samples.txt',
            row.names = FALSE, quote = FALSE, sep = '\t')


#### generate bci and bdy files ###################################################################
for (i in 1:n) {
  ## define LHS parameters
  tp <- samples[i, 'tp'] * 3600
  m <- samples[i, 'm']
  
  ## if initial conditons are wet: 
  simlength <- 10*24*3600
  t <- seq(0, simlength, 60)
  q <- ((t/tp)^m * exp(m*(1-(t/tp)))) * Qp
  q <- cbind(q, baseflow) %>% apply(1, max)
  q <- q / mean(edge.in$layer) #convert to m2/s
  # g1 <- ggplot(data.frame(t=t, q=q)) + geom_line(aes(x = t, y = q))
  
  bdy <- matrix(c('LISFLOOD', NA, paste0('flow', i, '_wet'), NA, length(t), 'seconds'), 
                byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
  write.table(bdy, file = paste0('bci_bdy/flow', i, '_wet.bdy'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  bci <- data.frame(matrix(
    c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', paste0('flow', i, '_wet'),
      'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
    nrow = 2, byrow = TRUE))
  write.table(bci, file = paste0('bci_bdy/flow', i, '_wet.bci'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  
  # if initial conditions are dry:
  t <- seq(0, simlength*1.5, 60)
  q <- c(rep(baseflow, length(seq(0, 0.5*simlength-60, 60))), q)
  # q <- q / mean(edge.in$layer) #convert to m2/s  ## this does it twice!! 
  # g2 <- ggplot(data.frame(t=t, q=q)) + geom_line(aes(x = t, y = q))
  
  bdy <- matrix(c('LISFLOOD', NA, paste0('flow', i, '_dry'), NA, length(t), 'seconds'), 
                byrow = TRUE, ncol = 2) %>% rbind(cbind(q, t))
  write.table(bdy, file = paste0('bci_bdy/flow', i, '_dry.bdy'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
  bci <- data.frame(matrix(
    c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QVAR', paste0('flow', i, '_dry'),
      'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
    nrow = 2, byrow = TRUE))
  write.table(bci, file = paste0('bci_bdy/flow', i, '_dry.bci'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
}
