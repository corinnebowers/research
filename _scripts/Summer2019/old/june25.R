
## today's goal: look through the NFIP data, also relearn how to use R

## load all NFIP data
setwd('C:/Users/Corinne/Documents/_research/NFIP')

require(data.table)

# policy0 <- fread('./openfema_policies20190331_00.csv', header = TRUE)
# policy_names <- names(policy0)
# policy1 <- fread('./openfema_policies20190331_01.csv', header = FALSE, col.names = policy_names)
# policy2 <- read.csv('./openfema_policies20190331_02.csv')
# policy3 <- read.csv('./openfema_policies20190331_03.csv')
# policy4 <- read.csv('./openfema_policies20190331_04.csv')
# policy5 <- read.csv('./openfema_policies20190331_05.csv')
# policy6 <- read.csv('./openfema_policies20190331_06.csv')
# policy7 <- read.csv('./openfema_policies20190331_07.csv')
# policy8 <- read.csv('./openfema_policies20190331_08.csv')
# policy9 <- read.csv('./openfema_policies20190331_09.csv')

claims <- fread('./openFEMA_claims20190331.csv')
claim_names <- names(claims)



#### find census tract with highest claims per person ####

## load population by county
pop <- read.csv('C:/Users/Corinne/Documents/_research/county-pop-est2018-alldata.csv')
pop <- pop[pop$COUNTY != 0,]
pop$CountyCode <- pop$STATE*1000 + pop$COUNTY

statecodes <- read.csv('C:/Users/Corinne/Documents/_research/statecodes.csv', fileEncoding="UTF-8-BOM")
pop <- merge(pop, statecodes, by.x = 'STNAME', by.y = 'StateName', all.x = TRUE)

pop_bycounty = pop[,c('StateCode', 'CountyCode', 'CTYNAME', 'POPESTIMATE2018')]; names(pop_bycounty) = c('statecode', 'countycode', 'countyname', 'pop')

## find number & dollar value of claims by county
number_claims <- data.frame(table(claims$countycode)); names(number_claims) <- c('countycode', 'numclaims')
value_claims <- aggregate(cbind(amountpaidonbuildingclaim, amountpaidoncontentsclaim) ~ countycode, data = claims, sum)
claims_bycounty = merge(number_claims, value_claims, by = 'countycode', all = TRUE)

## combine into one file for joining in ArcGIS
countyinfo <- merge(pop_bycounty, claims_bycounty, by = 'countycode', all = TRUE)
countyinfo[is.na(countyinfo$pop), 'pop'] <- 0
countyinfo[is.na(countyinfo$numclaims), 'numclaims'] <- 0
countyinfo[is.na(countyinfo$amountpaidonbuildingclaim), 'amountpaidonbuildingclaim'] <- 0
countyinfo[is.na(countyinfo$amountpaidoncontentsclaim), 'amountpaidoncontentsclaim'] <- 0

## export
write.csv(countyinfo, './countyinfo_0628.csv')



# #### find out which events are drivers and exclude them #### 
# 
# require(lubridate)
# 
# datetable <- data.frame(table(ymd(claims$dateofloss))); names(datetable) = c('date', 'lossval')
# datetable_save <- datetable;  # datetable <- datetable_save
# datetable <- datetable[order(datetable$lossval, decreasing = TRUE),]
# datetable$count <- 1:nrow(datetable)
# 
# plot((datetable$lossval), (datetable$count), log = 'xy', type = 'l'); grid()
# 
# cutoff <- 10000  #can change this later
# 
# exclude_events <- datetable[datetable$lossval > cutoff,'date']
# claims_filtered <- claims[!(claims$dateofloss %in% exclude_events),]
# nrow(claims_filtered)/nrow(claims)
# 
# number_claims_filtered <- data.frame(table(claims_filtered$countycode)); names(number_claims_filtered) <- c('countycode', 'numclaims')
# value_claims_filtered <- aggregate(cbind(amountpaidonbuildingclaim, amountpaidoncontentsclaim) ~ countycode, data = claims_filtered, sum)
# claims_bycounty_filtered = merge(number_claims_filtered, value_claims_filtered, by = 'countycode', all = TRUE)
# 
# ## combine into one file for joining in ArcGIS
# countyinfo_filtered <- merge(pop_bycounty, claims_bycounty_filtered, by = 'countycode', all = TRUE)
# countyinfo_filtered[is.na(countyinfo_filtered$pop), 'pop'] <- 0
# countyinfo_filtered[is.na(countyinfo_filtered$numclaims), 'numclaims'] <- 0
# countyinfo_filtered[is.na(countyinfo_filtered$amountpaidonbuildingclaim), 'amountpaidonbuildingclaim'] <- 0
# countyinfo_filtered[is.na(countyinfo_filtered$amountpaidoncontentsclaim), 'amountpaidoncontentsclaim'] <- 0
# 
# ## export
# write.csv(countyinfo_filtered, './countyinfo_filtered_0628.csv')



#### find the counties with the most loss events ####
claims$counter <- 1
date_claims <- aggregate(counter ~ (countycode + dateofloss), data = claims, sum)
date_claims$counter <- 1
date_claims <- aggregate(counter ~ countycode, data = date_claims, sum)
names(date_claims) <- c('countycode', 'numevents')

countyinfo <- merge(pop_bycounty, claims_bycounty, by = 'countycode', all = TRUE)
countyinfo <- merge(countyinfo, date_claims, by = 'countycode', all = TRUE)
countyinfo[is.na(countyinfo$pop), 'pop'] <- 0
countyinfo[is.na(countyinfo$numclaims), 'numclaims'] <- 0
countyinfo[is.na(countyinfo$amountpaidonbuildingclaim), 'amountpaidonbuildingclaim'] <- 0
countyinfo[is.na(countyinfo$amountpaidoncontentsclaim), 'amountpaidoncontentsclaim'] <- 0
countyinfo[is.na(countyinfo$numevents), 'numevents'] <- 0

check <- function(x) sum(is.na(x))
apply(countyinfo,2,check)

## export
write.csv(countyinfo, './countyinfo_0628_events.csv')


#### play around with the high-octane events ####

hist(countyinfo$numevents, breaks = round(sqrt(nrow(countyinfo))))

require(ggplot2)
ggplot(countyinfo, aes(x = numevents)) + geom_histogram(bins = round(sqrt(nrow(countyinfo)))) #+ scale_x_log10()

bigloss_counties <- countyinfo[countyinfo$numevents > 1000,]
nrow(bigloss_counties)/nrow(countyinfo)*100 # 0.8% of counties
sum(bigloss_counties$pop)/sum(countyinfo$pop) # 10% of population 
#why is the population of the US only 26 million? 

bigloss_claims <- claims[claims$countycode %in% bigloss_counties,]
nrow(bigloss_claims)/nrow(claims)*100 # 40% of claims
sum(!is.na(bigloss_claims$amountpaidonbuildingclaim)) / sum(!is.na(claims$amountpaidonbuildingclaim))*100 # 40% of total building loss

View(bigloss_claims)

View(aggregate(reportedcity ~ countycode, bigloss_claims, unique))



#### play around with the high-octane counties ####
