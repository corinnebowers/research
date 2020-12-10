
#### goal for 07/26: define a water year ####

# require(data.table)
require(foreign)
require(lubridate)
require(ggplot2)

## load in claims
setwd('C:/Users/Corinne/Documents/_research')
claims <- read.csv('./NFIP/openFEMA_claims20190331.csv')
claims_save <- claims  # claims <- claims_save
claims$counter <- 1
keep_names <- c(2:3, 5, 7:10, 14:15, 17, 19, 22, 25:32, 34:40)
claims <- claims[,keep_names]
claim_names <- names(claims)

## filter claims to target region
regionid <- read.dbf('./ArcGIS/_working/CA_targetcounties.dbf')
claims_region <- claims[claims$countycode %in% regionid$FIPS,]

## create water year
claims_region$wateryear <- claims_region$yearofloss
claims_region[month(claims_region$dateofloss) %in% c(10, 11, 12), 'wateryear'] <- 
  claims_region[month(claims_region$dateofloss) %in% c(10, 11, 12), 'wateryear'] + 1

claimsbyyear <- data.frame(table(claims_region$wateryear))

elnino_weak <- c(1970, 1977, 1978, 1980, 2005, 2007, 2015, 2019)
elnino_mod <- c(1987, 1995, 2003, 2010)
elnino_strong <- c(1973, 1988, 1992)
elnino_vstrong <- c(1983, 1998, 2016)
elnino <- c(elnino_weak, elnino_mod, elnino_strong, elnino_vstrong)

lanina_weak <- c(1972, 1975, 1984, 1985, 2001, 2006, 2009, 2017, 2018)
lanina_mod <- c(1971, 1996, 2012)
lanina_strong <- c(1974, 1976, 1989, 1999, 2000, 2008, 2011)
lanina <- c(lanina_weak, lanina_mod, lanina_strong)

neutral <- setdiff(claims_region$wateryear, c(elnino, lanina))

claims_region$southernosc <- ""
claims_region[claims_region$wateryear %in% elnino_weak, 'southernosc'] <- 'El Nino - Weak'
claims_region[claims_region$wateryear %in% elnino_mod, 'southernosc'] <- 'El Nino - Moderate'
claims_region[claims_region$wateryear %in% c(elnino_strong, elnino_vstrong), 'southernosc'] <- 'El Nino - Strong'
claims_region[claims_region$wateryear %in% lanina_weak, 'southernosc'] <- 'La Nina - Weak'
claims_region[claims_region$wateryear %in% lanina_mod, 'southernosc'] <- 'La Nina - Moderate'
claims_region[claims_region$wateryear %in% lanina_strong, 'southernosc'] <- 'La Nina - Strong'
claims_region[claims_region$wateryear %in% neutral, 'southernosc'] <- 'Neutral'

ggplot() +
  geom_histogram(data = claims_region, aes(x = wateryear, fill = southernosc), 
                 color = 'darkgray', binwidth = 1) + 
  scale_fill_manual(breaks = c('El Nino - Strong', 'El Nino - Moderate', 'El Nino - Weak', 'Neutral', 
                               'La Nina - Weak', 'La Nina - Moderate', 'La Nina - Strong'), 
                    values = c('#ef8a62', '#b2182b', '#fddbc7', '#67a9cf', '#2166ac', '#d1e5f0', '#f7f7f7')) + 
  ggtitle('Number of Claims by Year')

ggplot() +
  geom_histogram(data = claims_region, aes(x = wateryear, weight = amountpaidonbuildingclaim, fill = southernosc), 
                 color = 'darkgray', binwidth = 1) + 
  scale_fill_manual(breaks = c('El Nino - Strong', 'El Nino - Moderate', 'El Nino - Weak', 'Neutral', 
                               'La Nina - Weak', 'La Nina - Moderate', 'La Nina - Strong'), 
                    values = c('#ef8a62', '#b2182b', '#fddbc7', '#67a9cf', '#2166ac', '#d1e5f0', '#f7f7f7')) + 
  ggtitle('Total Amount Paid on Claims by Year ($)')


#### goal for 07/29: pick 2-3 target locations for further analysis ####

## look at 2006
year <- 1995

claims_region_table1 <- aggregate(cbind(counter, amountpaidonbuildingclaim, amountpaidoncontentsclaim, 
                                        totalbuildinginsurancecoverage, 
                                        totalcontentsinsurancecoverage) ~ censustract, 
                                  data = claims_region[claims_region$wateryear == year,], sum)
names(claims_region_table1) <- c('censustract', 'numclaims', 'buildingclaim_total', 'contentsclaim_total', 
                                 'buildingcoverage_total', 'contentscoverage_total')
claims_region_table2 <- aggregate(cbind(crsdiscount, originalconstructiondate, amountpaidonbuildingclaim, 
                                        amountpaidoncontentsclaim, totalbuildinginsurancecoverage, 
                                        totalcontentsinsurancecoverage) ~ censustract,
                                  data = claims_region[claims_region$wateryear == year,], mean) 
names(claims_region_table2) <- c('censustract', 'avgCRSdiscount', 'avgyearbuilt', 'buildingclaim_avg', 
                                 'contentsclaim_avg', 'buildingcoverage_avg', 'contentscoverage_avg')
claims_region_summary <- merge(claims_region_table1, claims_region_table2, by = 'censustract', all = TRUE)

write.csv(claims_region_summary, './NFIP/claims_northernCA_1995_0729.csv')


claims_subset = claims_region[claims_region$wateryear == 2006 & claims_region$countycode == 6097,] #Sonoma
# View(claims_subset)
# View(aggregate(amountpaidonbuildingclaim ~ dateofloss, data = claims_subset, sum))


#### goal: determine insurance penetration ####

policies <- read.csv('./NFIP/CA_policies_0709.csv')

policies_inforce <- data.frame(matrix(unique(policies$censustract), ncol = 1))
names(policies_inforce) <- 'censustract'

for (year in 2010:2018) {
  end = ymd(paste(year, '10', '01', sep = '-'))
  start = end - years(1)
  
  policies_subset <- policies[(ymd(policies$policyeffectivedate) >= start) & 
                                (ymd(policies$policyeffectivedate) < end),]
  
  policies_summary <- data.frame(table(policies_subset$censustract))
  policies_inforce <- merge(policies_inforce, policies_summary, by.x = 'censustract', by.y = 'Var1', all.x = TRUE) 
  names(policies_inforce)[ncol(policies_inforce)] <- paste('year', year, sep = '_')
}
policies_inforce[is.na(policies_inforce)] <- 0

write.csv(policies_inforce, './NFIP/policies_inforce_0806.csv')
