## generate US claims map in R instead of ArcGIS

require(sf)
require(ggplot2); theme_set(theme_bw())
require(lubridate)

## user input information
setwd('C:/Users/cbowers/Desktop/Research')
claims <- read.csv('./_data/NFIP/openFEMA_claims20190331.csv')
counties <- st_read('./_gis/USA/_geography/tl_2017_us_county/tl_2017_us_county.shp')
pop <- read.csv('./_scripts/_working/county-pop-est2018-alldata.csv')


## claims values by county
df_claims <- aggregate(amountpaidonbuildingclaim ~ countycode, data = claims, length)  #number of claims
temp <- aggregate(ymd(dateofloss) ~ countycode, data = claims, 
                  function(x) length(unique(x)))                                       #number of loss days
df_claims <- merge(df_claims, temp, by = 'countycode', all = TRUE)
temp <- aggregate(amountpaidonbuildingclaim ~ countycode, data = claims, sum)          #building claim value
df_claims <- merge(df_claims, temp, by = 'countycode', all = TRUE)
temp <- aggregate(amountpaidoncontentsclaim ~ countycode, data = claims, sum)          #contents claim value
df_claims <- merge(df_claims, temp, by = 'countycode', all = TRUE)

names(df_claims) <- c('county', 'num_claims', 'num_lossdays', 'value_buildingclaim', 'value_contentsclaim')
df_claims$value_totalclaim <- df_claims$value_buildingclaim + df_claims$value_contentsclaim

df_claims <- merge(df_claims, data.frame(GEOID = counties$GEOID), 
                   by.x = 'county', by.y = 'GEOID', all.y = TRUE)
df_claims[is.na(df_claims)] <- 0


## load in population data
pop <- pop[,c(4:8, 10:18)]
pop$FIPS <- pop$STATE*1e3 + pop$COUNTY
pop2018 <- pop[,c('FIPS','STNAME','POPESTIMATE2018')]

df_claims <- merge(df_claims, pop2018, by.x = 'county', by.y = 'FIPS', all.x = TRUE)
df_claims$numclaims_percapita <- df_claims$num_claims / df_claims$POPESTIMATE2018
df_claims$lossdays_percapita <- df_claims$num_lossdays / df_claims$POPESTIMATE2018
df_claims$valueclaims_percapita <- df_claims$value_totalclaim / df_claims$POPESTIMATE2018
df_claims[!complete.cases(df_claims), 8:11] <- 0


## load in county shapefile
counties <- counties[,c('GEOID', 'NAME', 'STATEFP', 'geometry')]
counties$GEOID <- as.numeric(paste(counties$GEOID))
counties <- merge(counties, df_claims, by.x = 'GEOID', by.y = 'county', all.x = TRUE)


## plot
names(counties)
val <- counties$lossdays_percapita    #values to plot
n <- 5   #number of quantiles to plot

counties$quant <- cut(val, breaks = quantile(val, seq(0, 1, 1/n), na.rm = TRUE), 
                      include.lowest = TRUE)
ggplot() + 
  geom_sf(data = counties, aes(fill = quant)) +
  scale_fill_viridis_d(direction = -1) + 
  lims(x = c(-125,-68), y = c(25,50)) + 
  ggtitle('Loss Events per Capita, 1970-2018') + 
  labs(fill = 'Quantiles')

