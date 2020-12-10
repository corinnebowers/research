
#### goal for today: make the policies-in-effect graph #### 
 
require(lubridate)
require(ggplot2)

setwd('C:/Users/Corinne/Documents/_research/NFIP')
CA_policies <- read.csv('./CA_policies_0709.csv')
CA_policies$counter <- 1

# CA_policyinfo <- aggregate(counter ~ countycode, data = CA_policies, sum)

policies_written <- data.frame(table(year(ymd(CA_policies$policyeffectivedate))))
policies_terminated <- data.frame(table(year(ymd(CA_policies$policyteminationdate))))
policies_terminated$Freq <- -policies_terminated$Freq

bargraph <- ggplot() + 
  geom_bar(data = policies_written, aes(x = Var1, y = Freq, fill = 'Policies Written'), stat = 'identity') + 
  geom_bar(data = policies_terminated, aes(x = Var1, y = Freq, fill = 'Policies Terminated'), stat = 'identity')
bargraph + ggtitle('CA Policies in Force') + 
  xlab('Year') + ylab('Number of Policies')
  
                  
## compare policies-in-effect (data) to policies-in-effect (internet)
policies_inforce <- data.frame(matrix(unique(CA_policies$censustract), ncol = 1))
names(policies_inforce) <- 'censustract'

for (year in 2010:2018) {
  end = ymd(paste(year, '10', '01', sep = '-'))
  start = end - years(1)
  
  policies_subset <- CA_policies[(ymd(CA_policies$policyeffectivedate) >= start) & 
                                 (ymd(CA_policies$policyeffectivedate) < end),]
  
  policies_summary <- data.frame(table(policies_subset$censustract))
  policies_inforce <- merge(policies_inforce, policies_summary, by.x = 'censustract', by.y = 'Var1', all.x = TRUE) 
  names(policies_inforce)[ncol(policies_inforce)] <- paste('year', year, sep = '_')
}
policies_inforce[is.na(policies_inforce)] <- 0
                    
policies_inforce_data <- apply(policies_inforce, 2, sum)
(policies_inforce_data[-1] - policies_inforce_internet[-1]) / policies_inforce_data[-1] * 100
policies_inforce_internet = c(271331, 276915, 268756, 259010, 252865, 237444, 231979, 295922, 239905, 229239)
policies_inforce = data.frame(cbind(2009:2018, policies_inforce_internet))
## answer: it's all about 2% different



