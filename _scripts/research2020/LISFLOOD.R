

## run a LISFLOOD model with the baseflow as a constant input to generate a startfile

## pick a startfile
floods <- raster::stack()
for (i in 1:12) {
  floods <- raster(paste0('C:/Users/cbowers/Desktop/LISFLOOD/', 
                             'sonoma_sherlock/21-01-06 baseflow/results/baseflow', i, '.max')) %>% 
    stack(floods, .)
}

temp <- floods %>% Mean %>% as.data.frame(xy = TRUE) %>% 
  setNames(c('x', 'y', 'value')) %>% filter(value > 0) %>% 
  rasterFromXYZ(crs = crs(dem))
mapview(hydropoly_rr %>% st_transform(crs(dem))) + mapview(temp)

flooded_cells <- c()
for (i in 1:12) {
  flooded_cells[i] <- sum(floods[[i]][] > 0)
}
temp <- read.csv('C:/Users/cbowers/Desktop/LISFLOOD/sonoma_sherlock/21-01-06 baseflow/parfiles/lisflood_log.txt',
                 sep = '\t') %>% mutate(flooded_cells = zeros)
ggplot(temp) + 
  geom_tile(aes(x = factor(SGCn), y = factor(baseflow_cfs), fill = flooded_cells)) + 
  scale_fill_viridis_c()

floods_bycfs <- floods %>% stackApply(indices = rep(1:3, 4), fun = mean) 
temp <- floods_bycfs[[1]] %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c('x', 'y', 'value')) %>% 
  filter(value > 0) %>% 
  rasterFromXYZ(crs = crs(dem))
mapview(hydropoly_rr %>% st_transform(crs(dem))) + mapview(temp)
temp <- floods_bycfs[[2]] %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c('x', 'y', 'value')) %>% 
  filter(value > 0) %>% 
  rasterFromXYZ(crs = crs(dem))
mapview(hydropoly_rr %>% st_transform(crs(dem))) + mapview(temp)
temp <- floods_bycfs[[3]] %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c('x', 'y', 'value')) %>% 
  filter(value > 0) %>% 
  rasterFromXYZ(crs = crs(dem))
mapview(hydropoly_rr %>% st_transform(crs(dem))) + mapview(temp)


temp <- floods %>% stackApply(indices = rep(1,12), fun = sd) %>% 
  as.data.frame(xy = TRUE) %>% 
  setNames(c('x', 'y', 'value')) %>% 
  filter(value > 0) %>% 
  rasterFromXYZ(crs = crs(dem))
mapview(hydropoly_rr %>% st_transform(crs(dem))) + mapview(temp)



## step 5) find the 100-year RP flow

Qp <- runoff[i,'discharge_m3s'] / 100  #m2/s
q <- apply(cbind(exp(m*(1-t/tp)) * (t/tp)^m * Qp, rep(baseflow, length(t))), 1, max)


bci <- data.frame(matrix(
  c('N', round(min(edge.in$x)), round(max(edge.in$x)), 'QFIX', baseflow/mean(edge.in$layer),
    'W', round(min(edge.out$y)), round(max(edge.out$y)), 'FREE', NA),
  nrow = 2, byrow = TRUE))
write.table(bci, file = 'C:/Users/cbowers/Desktop/LISFLOOD/new_rasters/baseflow5.bci', 
            row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t')


install.packages('lhs')
require(lhs)

