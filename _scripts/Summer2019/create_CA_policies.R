
## goal for today: subset policy datasets to CA policies only

## load all NFIP data
setwd('C:/Users/cbowers/Desktop/Research/_data/NFIP')

require(data.table)
policy_test <- fread('./openfema_policies20190331_00.csv', header = TRUE, nrows = 10000)
policynames_full <- names(policy_test)
# keep <- c(4:5, 7:12, 15:17, 19, 21, 25, 27:40, 43, 44:45)
keep <- c(5, 16, 19, 21, 25, 27:29, 32:33, 37, 43:44)

policy0 <- fread('./openfema_policies20190331_00.csv', header = TRUE, select = keep)
policy_names <- names(policy0)
CA_policies <- policy0[policy0$propertystate == 'CA',]
# save(policy0, file = 'policy0.Rda')
rm(policy0)

policy1 <- fread('./openfema_policies20190331_01.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy1[policy1$propertystate == 'CA',])
# save(policy1, file = 'policy1.Rda')
rm(policy1)

policy2 <- fread('./openfema_policies20190331_02.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy2[policy2$propertystate == 'CA',])
# save(policy2, file = 'policy2.Rda')
rm(policy2)

policy3 <- fread('./openfema_policies20190331_03.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy3[policy3$propertystate == 'CA',])
# save(policy3, file = 'policy3.Rda')
rm(policy3)

policy4 <- fread('./openfema_policies20190331_04.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy4[policy4$propertystate == 'CA',])
# save(policy4, file = 'policy4.Rda')
rm(policy4)

policy5 <- fread('./openfema_policies20190331_05.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy5[policy5$propertystate == 'CA',])
# save(policy5, file = 'policy5.Rda')
rm(policy5)

policy6 <- fread('./openfema_policies20190331_06.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy6[policy6$propertystate == 'CA',])
# save(policy6, file = 'policy6.Rda')
rm(policy6)

policy7 <- fread('./openfema_policies20190331_07.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy7[policy7$propertystate == 'CA',])
# save(policy7, file = 'policy7.Rda')
rm(policy7)

policy8 <- fread('./openfema_policies20190331_08.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy8[policy8$propertystate == 'CA',])
# save(policy8, file = 'policy8.Rda')
rm(policy8)

policy9 <- fread('./openfema_policies20190331_09.csv', header = FALSE, select = keep, col.names = policy_names)
# CA_policies_save <- CA_policies
CA_policies <- rbind(CA_policies, policy9[policy9$propertystate == 'CA',])
# save(policy9, file = 'policy9.Rda')
rm(policy9)

write.csv(CA_policies, './CA_policies_1028.csv', row.names = FALSE)

