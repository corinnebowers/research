#### goal: recreate all the graphs that I'm showing Baker in my 08/13 meeting ####

require(lubridate)
require(ggplot2)

## load in claims
setwd('C:/Users/cbowers/Desktop/Research')
claims <- read.csv('./_data/NFIP/openFEMA_claims20190331.csv')
claims_save <- claims  # claims <- claims_save
claims$counter <- 1
keep_names <- c(2:3, 5, 7:10, 14:15, 17, 19, 22, 25:32, 34:40)
claims <- claims[,keep_names]
claim_names <- names(claims)

claims_CA <- claims[claims$state == 'CA',]


## slide #5: CA policies in force



## slide #7: CA losses by month
date_claims_CA <- aggregate(cbind(counter, amountpaidonbuildingclaim, amountpaidoncontentsclaim) ~ dateofloss, data = claims_CA, sum)

ggplot(claims_CA, aes(x = month(dateofloss))) + geom_bar() + scale_x_continuous(breaks = 1:12, minor_breaks = NULL) +
  ggtitle('CA: Number of Claims by Month, 1975-2018') + xlab('Month') + ylab('Number of Claims')

ggplot(date_claims_CA, aes(x = month(dateofloss))) + geom_bar() + scale_x_continuous(breaks = 1:12, minor_breaks = NULL) + 
  ggtitle('CA: Loss Days by Month, 1975-2018') + xlab('Month') + ylab('Number of Loss Days')

ggplot(claims_CA, aes(x = month(dateofloss), weight = amountpaidonbuildingclaim)) + 
  geom_bar(color = 'black', fill = c(rep('grey', 3), rep('white', 6), rep('grey', 3))) + 
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL) +
  ggtitle('CA: Value of Claims by Month ($), 1975-2018') + xlab('Month') + ylab('Value of Claims') +
  theme_bw()

ggplot(claims_CA, aes(x = month(dateofloss), weight = amountpaidonbuildingclaim/44)) + 
  geom_bar(color = 'black', fill = c(rep('grey', 3), rep('white', 6), rep('grey', 3))) + 
  scale_x_continuous(breaks = 1:12, minor_breaks = NULL) +
  ggtitle('CA: Average Claims by Month ($), 1975-2018') + xlab('Month') + ylab('Annual Claims Average') +
  theme_bw()



## slide #9: northern CA losses by ENSO year



## slide #13: Sonoma river gauges

