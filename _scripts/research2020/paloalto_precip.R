
pa <- st_as_sf(data.frame(name = 'paloalto', lon = -122.170491, lat = 37.427611), 
               coords = c('lon', 'lat'), crs = NAD)

datelist <- seq(ymd('1981-01-01'), ymd('2018-12-31'), 'days')
precip <- rep(NA, length(datelist))

setwd('D:/Research/')

pb <- txtProgressBar(min = 0, max = length(datelist), style = 3)
for (i in 1:length(datelist)) {
  filename <- paste0('./_data/PRISM/', year(datelist[i]), '/PRISM_ppt_stable_4kmD2_', 
                     gsub('-', '', datelist[i]), '_bil.bil')
  precip[i] <- raster::extract(raster(filename), pa)
  setTxtProgressBar(pb, i)
}

ggplot() + 
  geom_raster(data = as.data.frame(raster(filename), xy = TRUE), 
              aes(x=x, y=y, fill = PRISM_ppt_stable_4kmD2_19820818_bil)) + 
  geom_sf(data = pa)

df <- data.frame(data = datelist, 
                 precip = precip,
                 year = year(datelist), 
                 month = month(datelist))
df <- df %>%
  group_by(year, month) %>%
  summarize(precip = Sum(precip))

df %>%
  group_by(month) %>%
  summarize(precip = Mean(precip)/(365/12)/(2018-1981+1))

ggplot(data = df) + 
  annotate('rect', xmin = 1, xmax = 3, ymin = 0, ymax = 12, fill = '#33A8FF', alpha = 0.3) + 
  annotate('rect', xmin = 3, xmax = 4, ymin = 0, ymax = 12, fill = '#33A8FF', alpha = 0.2) + 
  annotate('rect', xmin = 10, xmax = 12, ymin = 0, ymax = 12, fill = '#33A8FF', alpha = 0.3) + 
  geom_line(aes(x = factor(month), y = precip/365*12, group = year), color = 'gray60') + 
  geom_line(data = df %>% group_by(month) %>% summarize(precip = Mean(precip)/365*12),
    aes(x = month, y = precip), size = 1, color = 'black') +
  geom_point(data = df %>% group_by(month) %>% summarize(precip = Mean(precip)/365*12),
            aes(x = month, y = precip), size = 1.5, color = 'black') +
  ggtitle('Average Daily Precipitation in Stanford, CA, 1981-2018') + 
  scale_x_discrete(labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')) + 
  labs(x = '', y = 'Precipitation (in)') + 
  scale_y_continuous(breaks = seq(0,12,2)) + 
  theme_classic()

