## goal: collect all data for PCA 

setwd('C:/Users/cbowers/Desktop/Research/')
require(foreign)
require(lubridate)
require(reshape2)
require(dplyr)
require(ggplot2)

#### rainfall ####

## input data
raingauges <- read.dbf('./_gis/_working/raingauges.dbf')
names(raingauges) <- c('StationID', 'Lat', 'Long')
# raingauges_sonoma <- c('US1CASN0068', 'USC00041603', 'USC00041838', 'USC00043191', 'USC00043578', 'USC00043875', 'USC00046370', 
#                        'USC00046826', 'USC00047965', 'USC00048351', 'USR0000CHAW', 'USR0000CSRS', 'USW00023213')
# raingauges <- raingauges[raingauges$StationID %in% raingauges_sonoma,]

## 11/05 note: can't find this precipitation data -> this code is pretty useless
precip_2005 <- read.csv('C:/Users/Corinne/Downloads/1834108.csv')
precip_2005 <- precip_2005[,1:24]
precip_2006 <- read.csv('C:/Users/Corinne/Downloads/1834123.csv')
precip <- rbind(precip_2005, precip_2006)
precip <- precip[precip$STATION %in% raingauges$StationID,]
precip[is.na(precip$PRCP), 'PRCP'] <- 0

monthnames <- c(1:3, 10:12)
names(monthnames) <- c('jan', 'feb', 'mar', 'oct', 'nov', 'dec')


## find monthly totals by station (in)
temp <- aggregate(PRCP ~ STATION + month(DATE), data = precip, sum)
names(temp) <- c('STATION', 'MONTH', 'PRCP')
precip_bymonth <- dcast(temp, STATION ~ MONTH)
names(precip_bymonth) = c('StationID', paste('monthlytotal', names(monthnames), sep = '_'))
precip_bymonth[is.na(precip_bymonth)] <- 0


## find number of days over threshold (days)
daysover <- data.frame(raingauges$StationID)
names(daysover) <- 'StationID'
daysover$daysover_threshold <- 1
daysover[, paste('daysover', names(monthnames), sep = '_')] <- 0

for (i in 1:nrow(daysover)) {
  precip_daysover <- precip[precip$STATION %in% daysover[i,'StationID'] & precip$PRCP > daysover[i,'daysover_threshold'],]
  if (nrow(precip_daysover) > 0) {
    temp <- aggregate(PRCP ~ month(DATE), data = precip_daysover, length)
    names(temp) <- c('MONTH', 'PRCP')
    daysover[i, 3:8][monthnames %in% temp$MONTH] <- t(temp$PRCP)
  }
}


## find number of consecutive days over threshold (days)
consecdays <- data.frame(raingauges$StationID)
names(consecdays) <- 'StationID'
consecdays$consecdays_threshold <- 1
consecdays[, paste('consecdays', names(monthnames), sep = '_')] <- 0

for (i in 1:nrow(consecdays)) {
  precip_daysover <- precip[precip$STATION %in% consecdays[i,'StationID'] & precip$PRCP > consecdays[i,'consecdays_threshold'],]
  for (mo in 1:length(monthnames)) {
    precip_consecdays <- precip_daysover[month(precip_daysover$DATE) == monthnames[mo],]
    if (nrow(precip_consecdays) == 1) {
      consecdays[i, 2+mo] <- 1
    } else if (nrow(precip_consecdays) > 1) {
      sumdays = 1
      maxdays = 1
      for (x in 2:nrow(precip_consecdays)) {
        if (ymd(precip_consecdays[x, 'DATE']) - 1 == ymd(precip_consecdays[x-1, 'DATE'])) {
          sumdays <- sumdays + 1
          maxdays <- max(sumdays, maxdays)
        } else {
          sumdays <- 1
        }
      }
      consecdays[i, 2+mo] <- maxdays
    }
  }
}


## find length of longest storm (days)

## find max daily rain value (in)
temp <- aggregate(PRCP ~ STATION + month(DATE), data = precip, max)
names(temp) <- c('STATION', 'MONTH', 'PRCP')
max_bymonth <- dcast(temp, STATION ~ MONTH)
names(max_bymonth) = c('StationID', paste('monthlymax', names(monthnames), sep = '_'))
max_bymonth[is.na(max_bymonth)] <- 0


## put it all together
raingauges_save <- raingauges  # raingauges <- raingauges_save
raingauges <- merge(raingauges, precip_bymonth, by = 'StationID', all.x = TRUE)
raingauges <- merge(raingauges, daysover, by = 'StationID', all.x = TRUE)
raingauges <- merge(raingauges, consecdays, by = 'StationID', all.x = TRUE)
raingauges <- merge(raingauges, max_bymonth, by = 'StationID', all.x = TRUE)

## keep only october
raingauges_oct <- raingauges[,c(1:3, (1:ncol(raingauges))[grepl('oct', names(raingauges))])]




#### stream gauges ####

## subset counties
streamgauges <- read.csv('./streamgauges.csv', encoding = 'UTF-8')
names(streamgauges) <- c('StationID', 'StationName', 'County')
counties <- read.dbf('./ArcGIS/_working/countyselection.dbf')
streamgauges <- streamgauges[streamgauges$County %in% counties$NAME,]

write.csv(streamgauges, 'C:/Users/Corinne/OneDrive/research/rivergauges/streamgauges.csv', 
          col.names = TRUE, fileEncoding = 'UTF-8')

## scrape data with Python
streamgauge_latlong <- read.csv('C:/Users/Corinne/OneDrive/research/rivergauges/_SummaryFile.csv')
streamgauge_latlong$StationID <- sapply(strsplit(paste(streamgauge_latlong$Name), "_"), `[`, 1)
streamgauge_latlong <- streamgauge_latlong[,c(4, 2:3)]
streamgauges <- merge(streamgauges, streamgauge_latlong, all.x = TRUE)
streamgauges <- streamgauges[,c(1, 4:5)]

## find which stream gauges have real data
keep <- c()
for (i in 1:nrow(streamgauges)) {
  dailyrecord <- read.csv(paste('C:/Users/Corinne/OneDrive/research/rivergauges/USGS', streamgauges[i,'StationID'], 'DailyData.csv', sep = '_'),
                          comment.char = '#', sep = '\t')
  if (nrow(dailyrecord) > 1) { keep <- c(keep, i) }
}
streamgauges <- streamgauges[keep,]


## initalize dataframes for PCA
daysover <- data.frame(streamgauges$StationID)
names(daysover) <- 'StationID'
daysover[, paste('daysover', names(monthnames), sep = '_')] <- 0

consecdays <- data.frame(streamgauges$StationID)
names(consecdays) <- 'StationID'
consecdays[, paste('consecdays', names(monthnames), sep = '_')] <- 0

maxvalue <- data.frame(streamgauges$StationID)
names(maxvalue) <- 'StationID'
maxvalue[, paste('max', names(monthnames), sep = '_')] <- 0


toNumber <- function(x) as.numeric(paste(x))
for (i in 1:nrow(streamgauges)) {
  # ## debug
  # streamgauges[i, 'StationID']
  
  ## input data
  dailyrecord <- read.csv(paste('C:/Users/Corinne/OneDrive/research/rivergauges/USGS', streamgauges[i,'StationID'], 'DailyData.csv', sep = '_'),
                          comment.char = '#', sep = '\t')
  dailyrecord <- dailyrecord[2:nrow(dailyrecord), 3:4]
  names(dailyrecord) <- c('Date', 'RecordedCFS')
  dailyrecord$Date <- ymd(dailyrecord$Date)
  dailyrecord$RecordedCFS <- toNumber(dailyrecord$RecordedCFS)
  
  avgrecord <- read.csv(paste('C:/Users/Corinne/OneDrive/research/rivergauges/USGS', streamgauges[i,'StationID'], 'DailyStats.csv', sep = '_'),
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
    } else {
      i
    }
  } else {
    avgrecord <- avgrecord[year_start:year_end,]
  }
  
  avgrecord$date <- ymd(paste('2016', avgrecord$month_nu, avgrecord$day_nu, sep = '-'))
  avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] <- avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] - years(1)
  
  
  ## find number of days over threshold (days)
  daily <- data.frame(ymd(avgrecord$date - years(10)))
  names(daily) <- 'Date'
  daily$MEAN <- avgrecord$mean_va
  daily$THRESHOLD <- 2*daily$MEAN
  daily <- merge(daily, dailyrecord, by = 'Date', all.y = TRUE)
  daily$PASS <- (daily$THRESHOLD < daily$RecordedCFS)
  
  temp <- aggregate(PASS ~ month(Date), data = daily, sum)
  names(temp) <- c('MONTH', 'GAUGE')
  daysover[i, 2:7][monthnames %in% temp$MONTH] <- t(temp$GAUGE)
  
  
  ## find number of consecutive days over threshold (days)
  for (mo in 1:length(monthnames)) {
    daily_consecdays <- daily[month(daily$Date) == monthnames[mo],]
    daily_consecdays <- daily_consecdays[!is.na(daily_consecdays$PASS),]
    sumdays = 0
    maxdays = 0
    if (nrow(daily_consecdays) > 0) {
      for (x in 1:nrow(daily_consecdays)) {
        if (daily_consecdays[x, 'PASS']) {
          sumdays <- sumdays + 1
          maxdays <- max(sumdays, maxdays)
        } else {
          sumdays <- 0
        }
      }
    }
    consecdays[i, 1+mo] <- maxdays
  }

  
  ## find max daily stream gauge value (% of daily mean)
  daily$MAX = daily$RecordedCFS / daily$MEAN * 100
  temp <- aggregate(MAX ~ month(Date), data = daily, max)
  names(temp) <- c('MONTH', 'GAUGE')
  maxvalue[i, 2:7][monthnames %in% temp$MONTH] <- t(temp$GAUGE)
  
}


## put it all together
streamgauges_save <- streamgauges  # streamgauges <- streamgauges_save
streamgauges <- merge(streamgauges, daysover, by = 'StationID', all.x = TRUE)
streamgauges <- merge(streamgauges, consecdays, by = 'StationID', all.x = TRUE)
streamgauges <- merge(streamgauges, maxvalue, by = 'StationID', all.x = TRUE)

## keep only october
streamgauges_oct <- streamgauges[,c(1:3, (1:ncol(streamgauges))[grepl('oct', names(streamgauges))])]



#### write both raingauges_oct and streamgauges_oct out for use in ArcGIS ####

write.csv(raingauges_oct, 'C:/Users/Corinne/OneDrive/research/raingauges_oct.csv')
write.csv(streamgauges_oct, 'C:/Users/Corinne/OneDrive/research/streamgauges_oct.csv')


#### get gridded information back from ArcGIS
require(rgdal)
require(ggplot2)
require(ggmap)
require(rgeos)
require(maptools)
input <- readOGR('./ArcGIS/_working/floodplain_bygrid5.shp')
floodplain <- input@data

## I have no idea which one is actually the total rainfall (mean by grid cell) 
keep <- c(1, 3, 8, 34, 35)

floodplain <- floodplain[,keep]
names(floodplain) <- c('FID', 'TotalArea', 'TotalMonthlyRainfall', 'Floodplain', 'PercentArea')

floodplain$Ratio <- floodplain$PercentArea / floodplain$TotalArea
floodplain[floodplain$Floodplain == "YES",]

temp <- aggregate(Ratio ~ FID, data = floodplain[floodplain$Floodplain == "YES",], sum)
