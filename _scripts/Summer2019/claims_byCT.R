
require(foreign)
require(lubridate)


root <- 'C:/Users/cbowers/Desktop/Research/'

sonoma <- read.dbf('./_gis/_working/sonomatracts.dbf')
claimsdata <-  read.csv('./_data/NFIP/openFEMA_claims20190331.csv')
claimsdata$censustract <- toNumber(claimsdata$censustract)
claimsdata <- claimsdata[claimsdata$censustract %in% toNumber(sonoma$GEOID),]

start <- date('2005-12-24')
end <- date('2006-01-03')

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


## save this as a CSV and work with it in ArcGIS ##
write.csv(df_claims, paste(root, '_data/_working/2006stormclaims.csv', sep = ''))
