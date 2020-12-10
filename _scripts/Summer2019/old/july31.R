#### goal for today: target 2006 Sonoma floods ####

require(lubridate)
require(ggplot2)

## load policies & claims
setwd('C:/Users/Corinne/Documents/_research')
claims <- read.csv('./NFIP/openFEMA_claims20190331.csv')
claims_save <- claims  # claims <- claims_save
claims$counter <- 1
keep_names <- c(2:3, 5, 7:10, 14:15, 17, 19, 22, 25:32, 34:40)
claims <- claims[,keep_names]
claim_names <- names(claims)

policies <- read.csv('./NFIP/CA_policies_0709.csv')
policies_save <- policies  # policies <- policies_save
policies$counter <- 1
keep_names <- c(4:5, 7:12, 15:17, 19, 21, 25, 27:40, 43, 44:45)
policies <- policies[,keep_names]
policy_names <- names(policies)

## subset policies & claims
event = 2006
keep_counties <- c(6097, 6041, 6055, 6045, 6033) #Sonoma, Napa, Lake, Mendocino, & Marin
claims <- claims[claims$countycode %in% keep_counties,]
policies <- policies[policies$countycode %in% keep_counties,]

## create water year variable
claims$wateryear <- claims$yearofloss
claims[month(claims$dateofloss) %in% c(10, 11, 12), 'wateryear'] <- 
  claims[month(claims$dateofloss) %in% c(10, 11, 12), 'wateryear'] + 1
claims <- claims[claims$wateryear == event,]

policies$wateryear <- year(policies$policyeffectivedate)
policies[month(policies$policyeffectivedate) %in% c(10, 11, 12), 'wateryear'] <-
  policies[month(policies$policyeffectivedate) %in% c(10, 11, 12), 'wateryear'] + 1
# policies <- policies[policies$wateryear == event,]  #can only do this after 2009

## determine the length of the event
claimdays <- data.frame(table(claims$dateofloss))
claimdays <- claimdays[claimdays$Freq > 0,]
startdate <- ymd('2005-12-30')
enddate <- ymd('2006-01-03')
claims_event <- claims[ymd(claims$dateofloss) >= startdate & ymd(claims$dateofloss) <= enddate,]


#### plot records from the river gauge ####

dailyrecord <- read.csv('./ArcGIS/California/2006storms/rivergauges/USGS 11467000 RUSSIAN R A HACIENDA BRIDGE NR GUERNEVILLE CA/dailystormlevels.csv',
                        comment.char = '#', sep = '\t')
dailyrecord <- dailyrecord[2:nrow(dailyrecord), 3:4]
names(dailyrecord) <- c('Date', 'RecordedCFS')

avgrecord <- read.csv('./ArcGIS/California/2006storms/rivergauges/USGS 11467000 RUSSIAN R A HACIENDA BRIDGE NR GUERNEVILLE CA/averagelevels.csv',
                      comment.char = '#', sep = '\t')
avgrecord <- avgrecord[2:nrow(avgrecord), c(5:6, 10:ncol(avgrecord))]
toNumber <- function(x) as.numeric(paste(x))
avgrecord <- data.frame(apply(avgrecord, 2, toNumber))
avgrecord$date <- ymd(paste('2016', avgrecord$month_nu, avgrecord$day_nu, sep = '-'))
avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] <- avgrecord[avgrecord$month_nu %in% c(10, 11, 12), 'date'] - years(1)

# ggplot(data = avgrecord) + 
#   geom_line(aes(x = date, y = mean_va, color = 'dailymean')) + 
#   geom_line(aes(x = date, y = max_va, color = 'dailymax')) +
#   geom_line(aes(x = date, y = p90_va, color = 'daily90th')) +
#   geom_line(aes(x = date, y = p50_va, color = 'median'))

ggplot(data = avgrecord[avgrecord$date - years(10) <= max(ymd(dailyrecord$Date)) & 
                          avgrecord$date - years(10) >= min(ymd(dailyrecord$Date)),]) + 
  geom_line(aes(x = date - years(10), y = mean_va, color = 'dailymean')) + 
  geom_line(aes(x = date - years(10), y = max_va, color = 'dailymax')) + 
  geom_line(aes(x = date - years(10), y = p90_va, color = 'daily90th')) + 
  geom_line(aes(x = date - years(10), y = p95_va, color = 'daily95th')) + 
  geom_line(data = dailyrecord, aes(x = ymd(Date), y = as.numeric(paste(RecordedCFS)), color = 'recorded'))



