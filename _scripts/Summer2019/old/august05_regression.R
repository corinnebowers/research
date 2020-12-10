
#### goal: get as many regression variables as possible ####

require(lubridate)
require(ggplot2)

## 1. population
## 2. households & housing units


## 3. primary LULC


## 4. highest stream gauge

#### plot records from the river gauge ####
sitelist_sonoma = c(11458433, 11458500, 11458600, 11463170, 11463200, 11463500, 11463682, 11463900, 
                    11463980, 11464000, 11465200, 11465240, 11465350, 11465390, 11465660, 11465680, 
                    11465690, 11465700, 11465750, 11466170, 11466200, 11466320, 11466800, 11467000, 
                    11467002, 11467200, 11467270, 11467510)

siteno = c(2, 4, 10, 16, 18:24, 26)
# something's funky with record #11

toNumber <- function(x) as.numeric(paste(x))

for (i in 1:length(siteno)) {
  dailyrecord <- read.csv(paste('C:/Users/cbowers/Desktop/Research/_data/rivergauges/WY2006/USGS', sitelist_sonoma[siteno[i]], 'DailyData.csv', sep = '_'),
                          comment.char = '#', sep = '\t')
  dailyrecord <- dailyrecord[2:nrow(dailyrecord), 3:4]
  names(dailyrecord) <- c('Date', 'RecordedCFS')
  
  avgrecord <- read.csv(paste('C:/Users/cbowers/Desktop/Research/_data/rivergauges/WY2006/USGS', sitelist_sonoma[siteno[i]], 'DailyStats.csv', sep = '_'),
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

  avgrecord$date <- ymd(paste('2016', avgrecord$month_nu, avgrecord$day_nu, sep = '-'))
  avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] <- avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] - years(1)
  
  g <- ggplot(data = avgrecord[avgrecord$date - years(10) <= max(ymd(dailyrecord$Date)) & 
                            avgrecord$date - years(10) >= min(ymd(dailyrecord$Date)),]) + 
    geom_line(aes(x = date - years(10), y = mean_va, color = 'dailymean', linetype = 'dailymean')) + 
    geom_line(aes(x = date - years(10), y = max_va, color = 'dailymax', linetype = 'dailymax')) + 
    # geom_line(aes(x = date - years(10), y = p90_va, color = 'daily90th', linetype = 'daily90th')) + 
    # geom_line(aes(x = date - years(10), y = p95_va, color = 'daily95th', linetype = 'daily95th')) + 
    geom_line(data = dailyrecord, aes(x = ymd(Date), y = as.numeric(paste(RecordedCFS)), color = 'recorded', linetype = 'recorded')) +
    # scale_linetype_manual(values = c(2, 3, 1, 1, 1)) + 
    # scale_color_manual(values = c('black', 'black', 'gray', 'black', 'red')) + 
    scale_linetype_manual(values = c(1, 1, 1)) + 
    scale_color_manual(values = c('gray', 'black', 'red')) + 
    guides(color = guide_legend("Flows"), linetype = guide_legend("Flows")) + 
    ggtitle(paste('USGS Gauge #', sitelist_sonoma[siteno[i]], sep = ""))
  
  print(g)
  Sys.sleep(1)
}





