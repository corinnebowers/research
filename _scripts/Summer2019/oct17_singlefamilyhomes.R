
## goal: prove that single family homes (SFH) are the best choice for my project

## packages
require(foreign)
require(lubridate)
require(ggplot2); theme_set(theme_bw())


## file address
setwd('C:/Users/cbowers/Desktop/Research')

## load in policies
policies <- read.csv('./_data/NFIP/CA_policies_1017.csv')
policies$count <- 1

## load in SFH
homes <- read.dbf('./_data/_working/singlefamilyhomes.dbf')
homes <- data.frame(homes[,c(7, 3, 4, 9, 10)])
names(homes) <- c('FIPS', 'SFH_NUM', 'SFH_PCT', 'HOUSEHOLDS', 'HOUSINGUNITS')

## calculate number of average annual policies in force (october 1st)
policies$wateryear <- year(ymd(policies$policyeffectivedate))
policies[month(ymd(policies$policyeffectivedate)) >= 10, 'wateryear'] <- 
  policies[month(ymd(policies$policyeffectivedate)) >= 10, 'wateryear'] + 1

policies_peryear <- aggregate(count ~ wateryear + censustract, data = policies, sum)
policies_peryear <- policies_peryear[policies_peryear$wateryear != 2020,]
policies_byCT <- aggregate(cbind(count, wateryear) ~ censustract, data = policies_peryear, mean)
policies_byCT <- policies_byCT[,-ncol(policies_byCT)]
names(policies_byCT) <- c('FIPS', 'policycount')

## calculate the number of SFH per year
SFH_peryear <- aggregate(count ~ wateryear + censustract, 
                         data = policies[policies$occupancytype == 1,], sum)
SFH_byCT <- aggregate(cbind(count, wateryear) ~ censustract, data = SFH_peryear, mean)
SFH_byCT <- SFH_byCT[,-ncol(SFH_byCT)]
names(SFH_byCT) <- c('FIPS', 'SFHcount')

## combine with SimplyAnalytics data
homes <- merge(homes, policies_byCT, by = 'FIPS', all.x = TRUE)
homes <- merge(homes, SFH_byCT, by = 'FIPS', all.x = TRUE)
homes <- homes[-nrow(homes),]  #remove the CT of ocean
homes[is.na(homes)] <- 0

homes$policy_pen <- homes$policycount/homes$HOUSINGUNITS
homes$SFH_pen <- homes$SFHcount/homes$SFH_NUM

## plot results
ggplot(data = homes) + 
  geom_point(aes(x = HOUSEHOLDS, y = policycount))

ggplot(data = homes) + 
  geom_point(aes(x = SFH_NUM, y = SFHcount)) + 
  ggtitle(label = 'Insurance Penetration Rates', 
          subtitle = 'Single Family Homes (SFH) in Sonoma County, CA') + 
  labs(x = 'SFH per Census Tract', y = 'Insured SFH per Census Tract')

ggplot(data = homes) + 
  geom_histogram(aes(x = policy_pen), color = 'black', fill = 'white')

ggplot(data = homes) + 
  geom_histogram(aes(x = SFH_pen), color = 'black', fill = 'white')

linreg <- lm(SFHcount ~ SFH_NUM, data = homes)
summary(linreg)


sum(homes$SFHcount) / sum(homes$policycount)
sum(homes$SFH_NUM) / sum(homes$HOUSEHOLDS)

sum(homes$SFHcount) / sum(homes$SFH_NUM)
sum(homes$policycount) / sum(homes$HOUSEHOLDS)

