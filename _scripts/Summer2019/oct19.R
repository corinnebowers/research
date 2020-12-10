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

## import river gauge data
riverdist <- read.dbf(paste(root, '_scripts/_working/disttorussianriver.dbf', sep = ""))
riverdist <- riverdist[,c('GEOID', 'MEAN')]; names(riverdist) <- c('GEOID', 'DIST_FACTOR')
riverdist <- data.frame(apply(riverdist, 2, toNumber))
nearest_gauge <- read.dbf(paste(root, '_scripts/_working/nearest_gauge.dbf', sep = ""))
nearest_gauge <- nearest_gauge[,c('GEOID', 'GAUGE')]
nearest_gauge$GEOID <- toNumber(nearest_gauge$GEOID)

river_info = merge(riverdist, nearest_gauge, by = 'GEOID', all.y = TRUE)

ggplot() + geom_density(data = river_info, aes(x = DIST_FACTOR), fill = 'gray', alpha = 0.6) + xlim(c(0,1)) + theme_bw()

river_info$DIST_FACTOR[is.na(river_info$DIST_FACTOR)] <- 0
gaugeid <- data.frame(str_split(river_info$GAUGE, '_', n = 2, simplify = TRUE))
names(gaugeid) <- c('GAUGE_ID', 'GAUGE_NAME')     
river_info <- cbind(river_info, gaugeid)

## get river gauge info for the 2006 storm
#(see august 5th R script)

siteno = unique(river_info$GAUGE_ID)
stormyear = 2006
start <- date('2005-12-24')
end <- date('2006-01-03')


gauges <- data.frame('GAUGE_ID' = siteno)
gauges$days_max = 0
gauges$pct_maxday = 0

for (i in 1:length(siteno)) {
  dailyrecord <- read.csv(paste(root, '_data/rivergauges/WY', stormyear, '/USGS_', siteno[i], '_DailyData.csv', sep = ''),
                          comment.char = '#', sep = '\t')
  dailyrecord <- dailyrecord[2:nrow(dailyrecord), 3:4]
  names(dailyrecord) <- c('Date', 'RecordedCFS')
  
  avgrecord <- read.csv(paste(root, '/_data/rivergauges/WY', stormyear, '/USGS_', siteno[i], '_DailyStats.csv', sep = ''),
                        comment.char = '#', sep = '\t')
  avgrecord <- data.frame(apply(avgrecord, 2, toNumber))
  year_start <- which((avgrecord$month_nu == 1) & (avgrecord$day_nu == 1))
  year_end <- which((avgrecord$month_nu == 12) & (avgrecord$day_nu == 31))
  if (length(year_start) > 1 | length(year_end) > 1) {
    if (length(year_start) == length(year_end)) {
      keep <- which(year_end - year_start == 365)
      year_start <- year_start[keep]; year_end <- year_end[keep]
      cut <- which.max(avgrecord[year_end, 'end_yr'] - avgrecord[year_end, 'begin_yr'])
      avgrecord <- avgrecord[year_start[cut]:year_end[cut],]
    }
  } else {
    avgrecord <- avgrecord[year_start:year_end,]
  }
  avgrecord$date <- ymd(paste('2016', avgrecord$month_nu, avgrecord$day_nu, sep = '-'))  #2016 chosen as default year because of leap day
  avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] <- avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] - years(1)
  
  
  ## set flooding threshold: 200% of mean
  avgrecord$flood <- avgrecord$mean_va * 2
  
  days_sofar = 0
  days_max = 0
  pct_worstday = 0
  duration = length(integer(end - start))
  
  for (delta in 0:duration) {
    ## find the number of days flow exceeds the threshold
    if (toNumber(dailyrecord[date(dailyrecord$Date) == date(start + days(delta)),'RecordedCFS']) > 
        avgrecord[day(avgrecord$date) == day(start + days(delta)) & month(avgrecord$date) == month(start + days(delta)),'flood']) {
      days_sofar <- days_sofar + 1
    } else {
      days_sofar <- 0
    }
    days_max = max(days_sofar, days_max)
    
    ## find the flow (% of average mean) on the worst day
    flow_today = toNumber(dailyrecord[date(dailyrecord$Date) == start + days(delta),'RecordedCFS'])
    flow_avg = avgrecord[day(avgrecord$date) == day(start + days(delta)) & month(avgrecord$date) == month(start + days(delta)),'mean_va']
    # flow_worstday = max(flow_worstday, flow_sofar)
    # if (flow_worstday == flow_sofar) {
    #   pct_worstday = flow_worstday/flow_avg * 100
    # }  
    pct_worstday = max(pct_worstday, flow_today/flow_avg)
  }
  gauges[i, 'days_max'] <- days_max
  gauges[i, 'pct_maxday'] <- pct_worstday * 100
}


## attach river flow values back to census tracts
river_info <- merge(river_info, gauges, by = 'GAUGE_ID', all.x = TRUE)

river_info$pct_maxday_adj <- river_info$pct_maxday * river_info$DIST_FACTOR
river_info$days_max_adj <- river_info$days_max * river_info$DIST_FACTOR

names(river_info)
drop <- c('GAUGE', 'GAUGE_NAME')
river_info <- river_info[,!names(river_info) %in% drop]

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