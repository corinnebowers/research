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

NFHLdata <- read.csv(paste(root, '_data/ArcGIS/NFHL_bypolygon.csv', sep = ""))
sonoma <- read.dbf(paste(root, '_gis/_working/sonomatracts.dbf', sep = ""))
claimsdata <-  read.csv('C:/Users/cbowers/Desktop/Research/_data/NFIP/openFEMA_claims20190331.csv')
claimsdata$censustract <- toNumber(claimsdata$censustract)
claimsdata <- claimsdata[claimsdata$censustract %in% toNumber(sonoma$GEOID),]

## run 9/18 code to get river_info


####  create new dataframe for analysis ####

## select fishnet grid cells of interest (Sonoma County)
df <- data.frame(toNumber(sonoma$GEOID)); names(df) <- 'ID'

## add river gauge data
df <- merge(df, river_info, by.x = 'ID', by.y = 'GEOID', all.x = TRUE)

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

df <- merge(df, df_claims, by = 'ID', all = FALSE)
df$percentpayout <- df$value_claim / df$value_policy
df$percentpayout[is.na(df$percentpayout)] <- 0

## add population data
population <- read.dbf(paste(root, '_gis/California/_demographics/populationinfo/SimplyAnalytics_targetarea.dbf', sep = ""))
population <- population[,c('VALUE6', 'FIPS')]; names(population) <- c('POP', 'ID')
population <- apply(population, 2, toNumber)
df <- merge(df, population, by = 'ID', all.x = TRUE)
df$claimdensity <- df$num_claim / df$POP
df$claimdensity[is.na(df$claimdensity)] <- 0

## normalize claims 
df$log_claimvalue <- log(df$value_claim + 1)
df$log_claimdensity <- log(df$claimdensity * 1000 + 1)
df$log_pctpayout <- log(df$percentpayout * 1000 + 1)

#### correlation matrix ####
names(df)
keep = c('DIST_FACTOR', 'pct_maxday_adj', 'days_max_adj', 'log_claimvalue', 'log_claimdensity', 'log_pctpayout')

ggpairs(data = df, columns = keep)

