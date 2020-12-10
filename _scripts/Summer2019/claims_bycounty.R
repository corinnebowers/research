## can't find it anywhere, so I'm recalculating this 

claimsdata <-  read.csv('C:/Users/cbowers/Desktop/Research/_data/NFIP/openFEMA_claims20190331.csv')

require(lubridate)
claimsdata <- claimsdata[ymd(claimsdata$dateofloss) >= ymd('2005-12-27') & 
                           ymd(claimsdata$dateofloss) <= ymd('2006-01-06'),]
claimsdata$claimvalue <- claimsdata$amountpaidonbuildingclaim + claimsdata$amountpaidoncontentsclaim

claimscounty <- aggregate(claimvalue ~ countycode, data = claimsdata, sum)

write.csv(claimscounty, 'C:/Users/cbowers/Desktop/Research/_data/_working/claims_2006storm_bycounty.csv')
