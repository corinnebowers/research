## packages
require(foreign)
require(ggplot2)
require(GGally)
require(lubridate)

## functions
toNumber <- function(x) { as.numeric(paste(x)) }

## import data from Python
root <- 'C:/Users/cbowers/Desktop/Research/'

riverdist <- read.dbf(paste(root, '_scripts/_working/disttorussianriver.dbf', sep = ""))
riverdist <- riverdist[,c('GEOID', 'MEAN')]; names(riverdist) <- c('GEOID', 'DIST_FACTOR')
riverdist <- data.frame(apply(riverdist, 2, toNumber))
nearest_gauge <- read.dbf(paste(root, '_scripts/_working/nearest_gauge.dbf', sep = ""))
nearest_gauge <- nearest_gauge[,c('GEOID', 'GAUGE')]
nearest_gauge$GEOID <- toNumber(nearest_gauge$GEOID)

river_info = merge(riverdist, nearest_gauge, by = 'GEOID', all.y = TRUE)
river_info$DIST_FACTOR[is.na(river_info$DIST_FACTOR)] <- 0
gaugeid <- data.frame(str_split(river_info$GAUGE, '_', n = 2, simplify = TRUE))
names(gaugeid) <- c('GAUGE_ID', 'GAUGE_NAME')     
river_info <- cbind(river_info, gaugeid)


# ## get flood stages (this is done manually, find a better way)
# gauges = data.frame('GAUGE_NAME' = unique(river_info$GAUGE_NAME))
# actionstage <- c(0, 31, 29, 20, 20.8, 38, 0, 0)
# floodstage <- c(0, 0, 32, 23, 25.4, 39.7, 0, 0)
# gauges$FLOOD_FT <- actionstage
# river_info <- merge(river_info, gauges, by = 'GAUGE_NAME')
## for now, just use 200% of the mean


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
