
#### today's goal: how to determine policy tenure

# #### import information ####
# require(data.table)
# require(lubridate)
# require(tictoc)
# require(parallel)
# require(foreach)
# require(doParallel)
# 
# setwd('C:/Users/Corinne/Documents/_research')
# CA_policies <- read.csv('./NFIP/CA_policies_0709.csv')
# CA_policies$counter <- 1
# CA_policies$renewaldate <- month(CA_policies$policyeffectivedate)*100 + day(CA_policies$policyeffectivedate)
# 
# 
# #### define policy clusters and CA_policies_merge ####
# 
# ## create clusters of similar policies (try to get the clusters as small as possible) 
# policycluster <- aggregate(counter ~ countycode + originalconstructiondate +
#                             latitude + longitude + numberoffloorsininsuredbuilding + occupancytype + 
#                             primaryresidenceindicator + originalnbdate + renewaldate, 
#                           data = CA_policies, sum)
# policycluster <- policycluster[order(policycluster$originalnbdate),]
# policycluster$clusterid <- 1:nrow(policycluster)
# 
# ## add cluster ids to CA_policies
# identifiers <- c('countycode', 'originalnbdate', 'originalconstructiondate', 'latitude', 'longitude',
#                  'numberoffloorsininsuredbuilding', 'occupancytype', 'primaryresidenceindicator', 'renewaldate')
# CA_policies_merge <- merge(CA_policies, policycluster, by = identifiers)
# 
# 
#### set up parallel processing ####

# num_cores <- detectCores() - 1
# clust <- makeCluster(num_cores)
# registerDoParallel(clust)
# 
# # uniqueid <- vector()
# # num_properties <- vector()
# # num_years <- vector()
# # row <- 0
# 
# # foreach(id = 1:nrow(policycluster)) %dopar% CalculatePolicyCluster(CA_policies_merge, policycluster)
# 
# tic()
# test <- foreach(id = 1:100, .combine = 'rbind') %dopar% doEverything()
# stopCluster(clust)
# toc()

#### define functions ####

# doEverything <- function() {
#   dates <- makeDateTable(CA_policies_merge)
#   
# }
# 
# 
# mylist <- list()
# for (id in 1:nrow(policycluster)) {
  # tic()
  dates <- makeDateTable(CA_policies_merge)
  # mylist[[id]] <- dates
  temp <- calculatePolicyCluster(CA_policies_subset, dates)
  # print(id)
  # toc()
# }




makeDateTable <- function(CA_policies_merge) {
  CA_policies_subset <<- subset(CA_policies_merge, CA_policies_merge$clusterid == id)
  dates <- data.frame(table(CA_policies_subset$policyeffectivedate))
  return(dates[dates$Freq > 0,])
  
}

calculatePolicyCluster <- function(CA_policies_subset, dates) {
  row <- 0
  originaldate <- month(CA_policies_subset[1,'originalnbdate'])*100 + day(CA_policies_subset[1,'originalnbdate'])
  
  while (sum(dates$Freq) > 0) {
    row <- row + 1
    uniqueid[row] <- id
    
    properties <- min(dates$Freq)
    multiplier <- CA_policies_subset[1, 'counter.y']
    num_properties[row] <- properties * multiplier
    
    yearval <- max(year(dates[dates$Freq == properties, 'Var1']))
    missed_years <- setdiff(2009:yearval, year(dates$Var1))
    
    if (length(missed_years) == 0) {
      # the policy has not lapsed within the dataset timeframe
      
      if (CA_policies_subset[1,'renewaldate'] == originaldate) {
        # assume the policy has never lapsed
        numyears <- yearval - year(CA_policies_subset[1,'originalnbdate']) + 1
        
      } else {
        # assume the policy lapsed sometime before the dataset timeframe
        # (note: how long ago is unknown)
        numyears <- yearval - min(year(dates$Var1)) + 1
      }
      dates$Freq <- apply(cbind(0, dates$Freq - properties), 1, max)
      
    } else {
      # the policy has lapsed within the dataset timeframe
      # missed_years <- c(missed_years, 2021)  ##is this line necessary?
      yearval <- min(year(dates[dates$Freq == properties, 'Var1']))
      
      termdate <- ymd(CA_policies_subset[year(CA_policies_subset$policyeffectivedate) == yearval, 
                                         'policyteminationdate'][1])
      term_expected <- ymd(dates[dates$Freq == properties, 'Var1'])[1] + years(1)
      if (is.na(term_expected)) {
        term_expected <- ymd(dates[dates$Freq == properties, 'Var1'])[1] - days(1) + years(1)
      }
      if (termdate == term_expected) {
        year_range <- yearval:min(c(missed_years[missed_years > yearval] - 1, max(year(dates$Var1))))
        numyears <- length(year_range)
        dates[year(dates$Var1) %in% year_range, 'Freq'] <- 
          apply(cbind(0, dates[year(dates$Var1) %in% year_range, 'Freq'] - properties), 1, max)
        
      } else {
        numyears <- 1
        dates[dates$Freq == properties, 'Freq'][1] <- 0
      }
    }
    num_years[row] <- numyears
  }
  
  return(cbind(uniqueid, num_properties, num_years))
}



# KNOWN BUG:
# if a 11-1-2014 policy gets canceled on 11-30-2015 (early termination occurs on the second year)


