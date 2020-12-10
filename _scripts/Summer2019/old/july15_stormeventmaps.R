
#### goal: create "storm event maps" ####
## we're gonna figure out what this means as we go. let's hit it

## load claims data
require(data.table)
setwd('C:/Users/Corinne/Documents/_research')
claims <- fread('./NFIP/openFEMA_claims20190331.csv')
claims$counter <- 1
claim_names <- names(claims)

## subset claims to CA
claims_CA <- claims[claims$state == 'CA',]
# write.csv(claims_CA, './NFIP/CA_claims_0716.csv')

## cluster "loss events" spread out over multiple dates
require(lubridate)
claimdates <- aggregate(cbind(counter, amountpaidonbuildingclaim, amountpaidoncontentsclaim) ~ dateofloss, 
                        data = claims_CA, sum)
claimdates <- claimdates[order(claimdates$dateofloss),]
names(claimdates) <- c('dateofloss', 'numclaims', 'buildingloss', 'contentsloss')

# set threshold (number of days to be lumped into one "event")
threshold <- 1

# initialize variables
claimevents <- data.frame(numdays = integer())
counter <- 1

# fill in first row
claimevents[counter, 'numdays'] <- 1
claimevents[counter, 'numclaims'] <- claimdates[1, 'numclaims']
claimevents[counter, 'buildingloss'] <- claimdates[1, 'buildingloss']
claimevents[counter, 'contentsloss'] <- claimdates[1, 'contentsloss']
claimevents[counter, 'event_startdate'] <- claimdates[1, 'dateofloss']

# loop over the rest of the matrix
for (i in 2:nrow(claimdates)) {
  if (ymd(claimdates[i-1, 'dateofloss']) + threshold >= ymd(claimdates[i, 'dateofloss'])) {
    claimevents[counter, 'numdays'] <- claimevents[counter, 'numdays'] + 1
    claimevents[counter, 'numclaims'] <- claimevents[counter, 'numclaims'] + claimdates[i, 'numclaims']
    claimevents[counter, 'buildingloss'] <- claimevents[counter, 'buildingloss'] + claimdates[i, 'buildingloss']
    claimevents[counter, 'contentsloss'] <- claimevents[counter, 'contentsloss'] + claimdates[i, 'contentsloss']
    claimevents[counter, 'event_enddate'] <- claimdates[i, 'dateofloss']
  } else {
    counter <- counter + 1
    claimevents[counter, 'numdays'] <- 1
    claimevents[counter, 'numclaims'] <- claimdates[i, 'numclaims']
    claimevents[counter, 'buildingloss'] <- claimdates[i, 'buildingloss']
    claimevents[counter, 'contentsloss'] <- claimdates[i, 'contentsloss']
    claimevents[counter, 'event_startdate'] <- claimdates[i, 'dateofloss']
  }
}

#### first test: January-March 2017

## figure out what data I actually need from the claims dataset
require(lubridate)
claims_test <- claims_CA
claims_test <- claims_test[ymd(claims_test$dateofloss) <= ymd('2017-01-31'),]
claims_test <- claims_test[ymd(claims_test$dateofloss) >= ymd('2017-01-01'),]

require(reshape2)
require(dplyr)
temp <- aggregate(counter ~ dateofloss + countycode, data = claims_test, sum)
lossdates_bycounty <- dcast(temp, countycode ~ dateofloss, value.var = "counter")
lossdates_bycounty[is.na(lossdates_bycounty)] <- 0
names(lossdates_bycounty) <- make.names(names(lossdates_bycounty))

## read in a shapefile of CA counties
require(rgdal)
require(ggplot2)
require(ggmap)
require(rgeos)
require(maptools)
counties <- readOGR('./ArcGIS/California/CA_Counties_0/CA_Counties/CA_Counties_TIGER2016.shp')
counties_fortify <- fortify(counties, region = 'FIPS')
counties <- merge(counties_fortify, counties, by.x = 'id', by.y = 'FIPS', all.x = TRUE)
counties <- counties[order(counties$order),]
counties <- merge(counties, lossdates_bycounty, by.x = 'id', by.y = 'countycode', all.x = TRUE)
counties <- counties[order(counties$order),]

countylines <- geom_path(data = counties, aes(x=long, y=lat, group=group), color = 'black')

eventdate <- 'X2017.01.07'

for (i in 2:length(names(lossdates_bycounty))) {
  eventdate = names(lossdates_bycounty)[i]
  eventcounties <- counties[counties[,eventdate] > 0,]
  map <- ggplot() + geom_polygon(aes(x = eventcounties$long, y = eventcounties$lat, group = eventcounties$group, 
                              fill = eventcounties[,eventdate])) + 
    scale_fill_gradient(low = '#4292c6', high = '#08306b') + countylines + 
    guides(fill = guide_legend(title = eventdate))
  print(map)
  rm(map)
  Sys.sleep(0.5)
}

