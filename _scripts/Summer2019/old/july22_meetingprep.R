
require(data.table)
require(lubridate)
require(ggplot2)

## load all NFIP data

# policy0 <- fread('C:/Users/Corinne/Downloads/FIMA_NFIP_Redacted_Policies_Data_Set_Part_1/openFEMA_policies20190430_01.csv')


setwd('C:/Users/Corinne/Documents/_research/NFIP')
claims <- fread('./openFEMA_claims20190331.csv')
claims$counter <- 1
claim_names <- names(claims)


#### find census tract with highest claims per capita ####

# ## load population by county
# pop <- read.csv('C:/Users/Corinne/Documents/_research/county-pop-est2018-alldata.csv')
# pop <- pop[pop$COUNTY != 0,]
# pop$CountyCode <- pop$STATE*1000 + pop$COUNTY
# 
# statecodes <- read.csv('C:/Users/Corinne/Documents/_research/statecodes.csv', fileEncoding="UTF-8-BOM")
# pop <- merge(pop, statecodes, by.x = 'STNAME', by.y = 'StateName', all.x = TRUE)
# 
# pop_bycounty = pop[,c('StateCode', 'CountyCode', 'CTYNAME', 'POPESTIMATE2018')]
# names(pop_bycounty) = c('statecode', 'countycode', 'countyname', 'pop')

# ## find number & dollar value of claims by county
# number_claims <- data.frame(table(claims$countycode)); names(number_claims) <- c('countycode', 'numclaims')
# value_claims <- aggregate(cbind(amountpaidonbuildingclaim, amountpaidoncontentsclaim) ~ countycode, data = claims, sum)
# claims_bycounty = merge(number_claims, value_claims, by = 'countycode', all = TRUE)

# ## find the counties with the most loss events
# claims$counter <- 1
# date_claims <- aggregate(counter ~ (countycode + dateofloss), data = claims, sum)
# date_claims$counter <- 1
# date_claims <- aggregate(counter ~ countycode, data = date_claims, sum)
# names(date_claims) <- c('countycode', 'numevents')

# ## combine into one file for joining in ArcGIS
# countyinfo <- merge(pop_bycounty, claims_bycounty, by = 'countycode', all = TRUE)
# countyinfo <- merge(pop_bycounty, claims_bycounty, by = 'countycode', all = TRUE)
# countyinfo <- merge(countyinfo, date_claims, by = 'countycode', all = TRUE)
# countyinfo[is.na(countyinfo$pop), 'pop'] <- 0
# countyinfo[is.na(countyinfo$numclaims), 'numclaims'] <- 0
# countyinfo[is.na(countyinfo$amountpaidonbuildingclaim), 'amountpaidonbuildingclaim'] <- 0
# countyinfo[is.na(countyinfo$amountpaidoncontentsclaim), 'amountpaidoncontentsclaim'] <- 0
# countyinfo[is.na(countyinfo$numevents), 'numevents'] <- 0
# 
# countyinfo <- countyinfo[!is.na(countyinfo$statecode),]
# 
# check <- function(x) sum(is.na(x))
# apply(countyinfo,2,check)
# 
# ## export
# write.csv(countyinfo, './countyinfo_0702.csv', row.names = FALSE)


# #### focus: counties with largest number of loss events ####
# 
# require(ggplot2)
# ggplot(countyinfo, aes(x = numevents)) + geom_histogram(bins = round(sqrt(nrow(countyinfo))))
# ggplot(countyinfo[countyinfo$numevents > 0,], aes(x = numevents)) + geom_histogram(bins = round(sqrt(nrow(countyinfo)))) +
#   scale_x_log10(minor_breaks = c(seq(1, 1e1, by = 1), seq(1e1, 1e2, by = 1e1), seq(1e2, 1e3, by = 1e2), seq(1e3, 1e4, by = 1e3)))
# 
# bigloss_counties <- countyinfo[countyinfo$numevents > 1000,]
# nrow(bigloss_counties)/nrow(countyinfo)*100  # 0.8% of counties
# sum(bigloss_counties$pop)/sum(countyinfo$pop)  # 12% of population 
# 
# bigloss_claims <- claims[claims$countycode %in% bigloss_counties$countycode,]
# nrow(bigloss_claims)/nrow(claims)*100  # 40% of claims
# sum(!is.na(bigloss_claims$amountpaidonbuildingclaim)) / sum(!is.na(claims$amountpaidonbuildingclaim))*100  # 40% of total building loss
# 
# # View(bigloss_claims)
# # View(aggregate(reportedcity ~ countycode, bigloss_claims, unique)) #city names are a hot mess


# #### focus: counties with largest number of loss events per capita ####
# 
# countyinfo$eventspercapita <- countyinfo$numevents / countyinfo$pop
# 
# ggplot(countyinfo, aes(x = eventspercapita)) + geom_histogram(bins = round(sqrt(nrow(countyinfo)))) + 
#   ggtitle('Distribution of Loss Events per Capita, by County') + xlab('Events per Capita') + ylab('Number of US Counties')
# ggplot(countyinfo[countyinfo$eventspercapita > 0,], aes(x = eventspercapita)) + geom_histogram(bins = round(sqrt(nrow(countyinfo)))) + 
#   scale_x_log10(minor_breaks = c(seq(1e-5, 1e-4, by = 1e-5), seq(1e-4, 1e-3, by = 1e-4), seq(1e-2, 1e-1, by = 1e-2), seq(1e-1, 1, by = 1e-1))) + 
#   ggtitle('Distribution of Loss Events per Capita, by County') + xlab('Events per Capita') + ylab('Number of US Counties')



## trim claims dataset
claims_CA <- claims[claims$state == 'CA',]
claims_CA <- claims_CA[year(claims_CA$dateofloss) >= 1975,]
claims_CA <- claims_CA[year(claims_CA$dateofloss) <= 2018,]
claims_CA$counter <- 1;

## cluster "loss events" spread out over multiple dates

date_claims_CA <- aggregate(cbind(counter, amountpaidonbuildingclaim, amountpaidoncontentsclaim) ~ dateofloss, 
                            data = claims_CA, sum)
date_claims_CA <- date_claims_CA[order(date_claims_CA$dateofloss),]
names(date_claims_CA) <- c('dateofloss', 'numclaims', 'buildingloss', 'contentsloss')

# set threshold (number of days to be lumped into one "event")
threshold <- 2

# initialize variables
counter <- 1
realevents <- data.frame(event_count = integer())

# fill in first row
realevents[counter, 'event_count'] <- 1
realevents[counter, 'claim_count'] <- date_claims_CA[1, 'numclaims']
realevents[counter, 'buildingloss'] <- date_claims_CA[1, 'buildingloss']
realevents[counter, 'contentsloss'] <- date_claims_CA[1, 'contentsloss']
realevents[counter, 'event_startdate'] <- date_claims_CA[1, 'dateofloss']

# loop over the rest of the matrix
for (i in 2:nrow(date_claims_CA)) {
  if (ymd(date_claims_CA[i-1, 'dateofloss']) + threshold >= ymd(date_claims_CA[i, 'dateofloss'])) {
    realevents[counter, 'event_count'] <- realevents[counter, 'event_count'] + 1
    realevents[counter, 'claim_count'] <- realevents[counter, 'claim_count'] + date_claims_CA[i, 'numclaims']
    realevents[counter, 'buildingloss'] <- realevents[counter, 'buildingloss'] + date_claims_CA[i, 'buildingloss']
    realevents[counter, 'contentsloss'] <- realevents[counter, 'contentsloss'] + date_claims_CA[i, 'contentsloss']
    realevents[counter, 'event_enddate'] <- date_claims_CA[i, 'dateofloss']
  } else {
    counter <- counter + 1
    realevents[counter, 'event_count'] <- 1
    realevents[counter, 'claim_count'] <- date_claims_CA[i, 'numclaims']
    realevents[counter, 'buildingloss'] <- date_claims_CA[i, 'buildingloss']
    realevents[counter, 'contentsloss'] <- date_claims_CA[i, 'contentsloss']
    realevents[counter, 'event_startdate'] <- date_claims_CA[i, 'dateofloss']
  }
}


#### look at the distribution of claims by month ####
claim_months <- data.frame(table(month(claims_CA$dateofloss)))
names(claim_months) <- c('Month','NumClaims')

event_months <- data.frame(table(month(date_claims_CA$dateofloss)))
names(event_months) <- c('Month', 'NumEvents')

ggplot(claims_CA, aes(x = month(dateofloss))) + geom_bar() + 
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL) +
  ggtitle('Northern CA Claims by Month, 1975-2018') + xlab('Month') + ylab('Number of Claims')
  
ggplot(date_claims_CA, aes(x = month(dateofloss))) + geom_bar() + 
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL) + 
  ggtitle('Northern CA Loss Days by Month, 1975-2018') + xlab('Month') + ylab('Number of Loss Events')


#### look at CRS stuff

