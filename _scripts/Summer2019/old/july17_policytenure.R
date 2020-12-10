
## today's goal: how to determine policy tenure

setwd('C:/Users/Corinne/Documents/_research')
require(data.table)
require(lubridate)
require(tictoc)
CA_policies <- fread('./NFIP/CA_policies_0709.csv')
CA_policies$counter <- 1

# CA_policies_toy = CA_policies[round(runif(5000)*nrow(CA_policies)),]
# 
# 
# temp <- aggregate(counter ~ countycode + originalconstructiondate +
#                     latitude + longitude + numberoffloorsininsuredbuilding + occupancytype + 
#                     primaryresidenceindicator + originalnbdate, 
#                   data = CA_policies_toy, sum)
# temp$uniqueid <- 1:nrow(temp)

CA_policies$renewaldate <- month(CA_policies$policyeffectivedate)*100 + day(CA_policies$policyeffectivedate)

policytenure <- aggregate(counter ~ countycode + originalconstructiondate +
                    latitude + longitude + numberoffloorsininsuredbuilding + occupancytype + 
                    primaryresidenceindicator + originalnbdate + renewaldate, 
                  data = CA_policies, sum)
policytenure_save <- policytenure  # policytenure <- policytenure_save
policytenure <- policytenure[order(policytenure$originalnbdate),]
policytenure$uniqueid <- 1:nrow(policytenure)

identifiers <- c('countycode', 'originalnbdate', 'originalconstructiondate', 'latitude', 'longitude',
                 'numberoffloorsininsuredbuilding', 'occupancytype', 'primaryresidenceindicator', 'renewaldate')
CA_policies_merge <- merge(CA_policies, policytenure, by = identifiers)

tenure_summary <- data.frame(uniqueid = integer(), num_properties = integer(), num_years = integer())
row <- 0
for (id in 1:nrow(policytenure)) {
  tic()
  CA_policies_subset <- subset(CA_policies_merge, CA_policies_merge$uniqueid == id)
  dates <- data.frame(table(CA_policies_subset$policyeffectivedate))
  originaldate <- month(CA_policies_subset[1,'originalnbdate'])*100 + day(CA_policies_subset[1,'originalnbdate'])
  
  while (sum(dates$Freq) > 0) {
    row <- row + 1
    tenure_summary[row, 'uniqueid'] <- id
    
    properties <- min(dates[dates$Freq > 0, 'Freq'])
    # tenure_summary[row, 'num_properties'] <- properties
    multiplier <- CA_policies_subset[1, 'counter.y']
    tenure_summary[row, 'num_properties'] <- properties * multiplier
    
    yearval <- max(year(dates[dates$Freq == properties, 'Var1']))
    missed_years <- setdiff(2009:yearval, year(dates[dates$Freq > 0, 'Var1']))
  
    if (length(missed_years) == 0) {
      # the policy has not lapsed within the dataset timeframe
      if (CA_policies_subset[1,'renewaldate'] == originaldate) {
        # assume the policy has never lapsed
        numyears <- yearval - year(CA_policies_subset[1,'originalnbdate']) + 1
      } else {
        # assume the policy lapsed sometime before the dataset timeframe
        # (note: how long ago is unknown)
        numyears <- yearval - min(year(dates[dates$Freq > 0, 'Var1'])) + 1
      }
      dates$Freq <- apply(cbind(0, dates$Freq - properties), 1, max)
    } else {
      # the policy has lapsed within the dataset timeframe
      missed_years <- c(missed_years, 2021)
      yearval <- min(year(dates[dates$Freq == properties, 'Var1']))
      termdate <- ymd(CA_policies_subset[year(CA_policies_subset$policyeffectivedate) == yearval, 
                                         'policyteminationdate'][1])
      term_expected <- ymd(dates[dates$Freq == properties, 'Var1'])[1] + years(1)
      if (is.na(term_expected)) {
        term_expected <- ymd(dates[dates$Freq == properties, 'Var1'])[1] - days(1) + years(1)
      }
      if (termdate == term_expected) {
        year_range <- yearval:min(c(missed_years[missed_years > yearval] - 1, 
                                    max(year(dates[dates$Freq > 0, 'Var1']))))
        numyears <- length(year_range)
        dates[year(dates$Var1) %in% year_range, 'Freq'] <- 
          apply(cbind(0, dates[year(dates$Var1) %in% year_range, 'Freq'] - properties), 1, max)
      } else {
        numyears <- 1
        dates[dates$Freq == properties, 'Freq'][1] <- 0
      }
    }
    tenure_summary[row, 'num_years'] <- numyears
  }
  print(id)
  toc()
}

# KNOWN BUG:
  # if a 11-1-2014 policy gets canceled on 11-30-2015 (early termination occurs on the second year)


#### code for displaying tables showing why this failed ####

# id = 272405
# id = 291696
# id = 294103
id = 503107

id = 924
CA_policies_subset <- subset(CA_policies_merge, CA_policies_merge$uniqueid == id)
dates <- data.frame(table(CA_policies_subset$policyeffectivedate))
temp <- (dates[dates$Freq > 0,])
names(temp) <- c('policyeffectivedate', 'numpolicies')


View(CA_policies_merge[CA_policies_merge$uniqueid == 4,
                       c('originalnbdate', 'policyeffectivedate', 'policyteminationdate', 'uniqueid')])

View(CA_policies[(month(CA_policies_merge$originalnbdate) != month(CA_policies_merge$policyeffectivedate)) | 
                   (day(CA_policies_merge$originalnbdate) != day(CA_policies_merge$policyeffectivedate)),])

View(CA_policies_merge[CA_policies_merge$uniqueid == 924,])
