## goal: get everything in one dataframe, and maybe even get a correlation matrix

## packages
require(foreign)
require(ggplot2)
require(GGally)
require(lubridate)

## import data from ArcGIS/Python
root <- 'C:/Users/cbowers/Desktop/Research'
setwd(root)
start <- date('1995-01-07')
end <- date('1995-01-12')
raindata <- read.csv('./_scripts/_WY2017_storm4/raindata.csv')

NFHLdata <- read.csv('./_data/ArcGIS/NFHL_byfishnet.csv')
claimsdata <- read.dbf('./_gis/_working/claims_fishnet_join2.dbf')
sonoma <- read.csv('./_data/_working/fishnetID_sonoma.csv')

####  create new dataframe for analysis ####

## select fishnet grid cells of interest (Sonoma County)
df <- data.frame(sonoma$FID); names(df) <- 'ID'

## add rainfall data
df <- merge(df, raindata, by.x = 'ID', by.y = 'FID_', all = FALSE)

## add floodplain data
df_NFHL <- NFHLdata[NFHLdata$FLOODPLAIN == 'YES', c(2, 5)]; names(df_NFHL) <- c('ID', 'pct_floodplain')
df <- merge(df, df_NFHL, by = 'ID', all.x = TRUE)
df$pct_floodplain[is.na(df$pct_floodplain)] <- 0

## add elevation data
elevation1 <- read.dbf('./_data/ArcGIS/elevation1_fishnet.dbf')
elevation2 <- read.dbf('./_data/ArcGIS/elevation2_fishnet.dbf')
elevation <- rbind(elevation1, elevation2)
elevation <- aggregate(MEAN ~ FID_, data = elevation, mean)
elevation$MEAN <- elevation$MEAN * 3.28084  #convert from meters to feet
names(elevation) <- c('ID', 'ft_elevation')
df <- merge(df, elevation, by = 'ID', all.x = TRUE)

## add claims data
toNumber <- function(x) { as.numeric(paste(x)) }
df_claims <- claimsdata[,c('dateofloss', 'amountpaid', 'amountpa_1', 'FID_2')]
names(df_claims) <- c('dateofloss', 'buildingclaim', 'contentsclaim', 'ID')
df_claims[,1] <- date(df_claims[,1])
df_claims[,2:4] <- data.frame(apply(df_claims[,2:4], 2, toNumber))
df_claims[is.na(df_claims)] <- 0
df_claims$value_claim <- df_claims$buildingclaim + df_claims$contentsclaim
df_claims <- df_claims[df_claims$dateofloss >= start & df_claims$dateofloss <= end,]

df_claims <- aggregate(value_claim ~ ID, data = df_claims, sum)
df <- merge(df, df_claims[,c('ID', 'value_claim')], by = 'ID', all.x = TRUE)
df$value_claim[is.na(df$value_claim)] <- 0

df$logvalue_claim = log10(df$value_claim)
df <- df[df$logvalue_claim > 0,]


#### correlation matrix ####
names(df)
keep = c('logvalue_claim', 'rain_sofar', 'rain_prevmonth', 'rain_stormtotal', 'rain_maxday', 'pct_floodplain', 'ft_elevation')

ggpairs(data = df, columns = keep)
