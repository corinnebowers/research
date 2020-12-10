## packages
require(foreign)
require(ggplot2)
require(GGally)
require(lubridate)

## functions
toNumber <- function(x) { as.numeric(paste(x)) }

## import data from ArcGIS/Python
root <- 'C:/Users/cbowers/Desktop/Research/'
start <- date('2005-12-24')
end <- date('2006-01-03')
raindata <- read.csv(paste(root, '_scripts/_WY2006/raindata_CT.csv', sep = ""))

NFHLdata <- read.csv(paste(root, '_data/ArcGIS/NFHL_bypolygon.csv', sep = ""))
sonoma <- read.dbf(paste(root, '_gis/_working/sonomatracts.dbf', sep = ""))
claimsdata <-  read.csv('C:/Users/cbowers/Desktop/Research/_data/NFIP/openFEMA_claims20190331.csv')
claimsdata$censustract <- toNumber(claimsdata$censustract)
claimsdata <- claimsdata[claimsdata$censustract %in% toNumber(sonoma$GEOID),]

####  create new dataframe for analysis ####

## select fishnet grid cells of interest (Sonoma County)
df <- data.frame(toNumber(sonoma$GEOID)); names(df) <- 'ID'

## add rainfall data
df <- merge(df, raindata[,c('GEOID', 'rain_sofar', 'rain_prevmonth', 'rain_stormtotal', 'days_max', 'rain_maxday')], 
            by.x = 'ID', by.y = 'GEOID', all = FALSE)

## add floodplain data
df_NFHL <- NFHLdata[NFHLdata$FLOODPLAIN == 'YES', c(2, 5)]; names(df_NFHL) <- c('ID', 'pct_floodplain')
df_NFHL$ID <- toNumber(df_NFHL$ID)
df <- merge(df, df_NFHL, by = 'ID', all.x = TRUE)
df$pct_floodplain[is.na(df$pct_floodplain)] <- 0

## add elevation data
elevation1 <- read.dbf(paste(root, '_data/ArcGIS/elevation1.dbf', sep = ""))
elevation2 <- read.dbf(paste(root, '_data/ArcGIS/elevation2.dbf', sep = ""))
elevation <- rbind(elevation1, elevation2)
elevation <- aggregate(MEAN ~ GEOID, data = elevation, mean)
elevation$MEAN <- elevation$MEAN * 3.28084  #convert from meters to feet
names(elevation) <- c('ID', 'ft_elevation')
elevation <- apply(elevation, 2, toNumber)
df <- merge(df, elevation, by = 'ID', all.x = TRUE)

## add claims data
df_claims <- claimsdata[,c('dateofloss', 'amountpaidonbuildingclaim', 'amountpaidoncontentsclaim', 
                           'totalbuildinginsurancecoverage', 'totalcontentsinsurancecoverage', 'censustract')]
names(df_claims) <- c('dateofloss', 'buildingclaim', 'contentsclaim', 'buildingpolicy', 'contentspolicy', 'ID')
df_claims[,1] <- date(df_claims[,1])
df_claims[,2:6] <- data.frame(apply(df_claims[,2:6], 2, toNumber))
df_claims[is.na(df_claims)] <- 0
df_claims$value_claim <- df_claims$buildingclaim + df_claims$contentsclaim
df_claims$value_policy <- df_claims$buildingpolicy + df_claims$contentspolicy
df_claims$num_claim <- 1
df_claims <- df_claims[df_claims$dateofloss >= start & df_claims$dateofloss <= end,]

df_claims <- aggregate(cbind(value_claim, value_policy, num_claim) ~ ID, data = df_claims, sum)
df <- merge(df, df_claims, by = 'ID', all.x = TRUE)
df$percentpayout <- df$value_claim / df$value_policy
df$percentpayout[is.na(df$percentpayout)] <- 0

## add population data
population <- read.dbf(paste(root, '_gis/California/_demographics/populationinfo/SimplyAnalytics_targetarea.dbf', sep = ""))
population <- population[,c('VALUE6', 'FIPS')]; names(population) <- c('POP', 'ID')
population <- apply(population, 2, toNumber)
df <- merge(df, population, by = 'ID', all.x = TRUE)
df$claimdensity <- df$num_claim / df$POP
df$claimdensity[is.na(df$claimdensity)] <- 0


#### correlation matrix ####
names(df)
keep = c('claimdensity', 'percentpayout', 'rain_sofar', 'rain_prevmonth', 'rain_stormtotal', 'rain_maxday', 'pct_floodplain', 'ft_elevation')

ggpairs(data = df, columns = keep)
