
### goal: get familiar with FEMA's Natural Hazard Mitigation Assistance products ####

require(lubridate)
require(ggplot2)

setwd('C:/Users/Corinne/Documents/_research')


#### load in FEMA disaster declarations ####

# # FEMA Disaster Declarations Summary is a summarized dataset describing all federally declared disasters. This
# # dataset lists all official FEMA Disaster Declarations, beginning with the first disaster declaration in 1953
# # and features all three disaster declaration types: major disaster, emergency, and fire management assistance.
# # The dataset includes declared recovery programs and geographic areas.
disasters1 <- read.csv('./FEMA/DisasterDeclarationsSummaries.csv')
disasters1 <- disasters1[disasters1$state == 'CA',]

# # This data set contains a list of FEMA recognized areas affected by a disaster, and the type of disaster 
# # assistance the area is eligible for.
disasters2 <- read.csv('./FEMA/FemaWebDisasterDeclarations.csv')
disasters2 <- disasters2[disasters2$stateCode == 'CA',]

# # This data set contains financial assistance values, including the number of approved applications, 
# # as well as individual and public assistance grant amounts.
disasters3 <- read.csv('./FEMA/FemaWebDisasterSummaries.csv')

# # This data set contains general information on declared disasters, including the disaster number, declaration 
# # type, state, description, incident start and end dates, and incident type.
disasters4 <- read.csv('./FEMA/FemaWebDeclarationAreas.csv')
disasters4 <- disasters4[disasters4$stateCode == 'CA',]

disasters <- merge(disasters2, disasters3, by = 'disasterNumber', all.x = TRUE)
keep <- c(1, 7:8, 6, 9, 3, 4:5, 2, 12, 15:21)
disasters <- disasters[,keep]
disasters <- disasters[,1:10]

disasters$declarationDate <- date(disasters$declarationDate)
disasters$incidentBeginDate <- date(disasters$incidentBeginDate)
bool = disasters$incidentEndDate != ""
temp = rep(NA, nrow(disasters)); temp[bool] <- date(disasters[bool, 'incidentEndDate'])
disasters$incidentEndDate <- date(temp + ymd(origin))
bool = disasters$closeoutDate != ""
temp = rep(NA, nrow(disasters)); temp[bool] <- date(disasters[bool, 'closeoutDate'])
disasters$closeoutDate <- date(temp + ymd(origin))


#### tie HMAs to disasters ####

## hazard mitigation assistance projects (HMA_P) 
HMA_P <- read.csv('./FEMA/HazardMitigationAssistanceProjects.csv')
HMA_P <- HMA_P[HMA_P$state == 'California',]

bool = HMA_P$dateInitiallyApproved != ""
temp = rep(NA, nrow(HMA_P)); temp[bool] <- date(HMA_P[bool, 'dateInitiallyApproved'])
HMA_P$dateInitiallyApproved <- date(temp + ymd(origin))
bool = HMA_P$dateApproved != ""
temp = rep(NA, nrow(HMA_P)); temp[bool] <- date(HMA_P[bool, 'dateApproved'])
HMA_P$dateApproved <- date(temp + ymd(origin))
bool = HMA_P$dateClosed != ""
temp = rep(NA, nrow(HMA_P)); temp[bool] <- date(HMA_P[bool, 'dateClosed'])
HMA_P$dateClosed <- date(temp + ymd(origin))

HMA_P <- merge(disasters, HMA_P, by = 'disasterNumber', all.y = TRUE)
HMA_P <- HMA_P[HMA_P$incidentType %in% c(NA, 'Flood', 'Severe Storm(s)', 'Dam/Levee Break'),]
HMA_P <- HMA_P[HMA_P$county == 'Sonoma',]

## hazard mitigation assistance mitigated properties (HMA_MP)
HMA_MP <- read.csv('./FEMA/HazardMitigationAssistanceMitigatedProperties.csv')
HMA_MP <- HMA_MP[HMA_MP$state == 'California',]

bool = HMA_MP$dateInitiallyApproved != ""
temp = rep(NA, nrow(HMA_MP)); temp[bool] <- date(HMA_MP[bool, 'dateInitiallyApproved'])
HMA_MP$dateInitiallyApproved <- date(temp + ymd(origin))
bool = HMA_MP$dateApproved != ""
temp = rep(NA, nrow(HMA_MP)); temp[bool] <- date(HMA_MP[bool, 'dateApproved'])
HMA_MP$dateApproved <- date(temp + ymd(origin))
bool = HMA_MP$dateClosed != ""
temp = rep(NA, nrow(HMA_MP)); temp[bool] <- date(HMA_MP[bool, 'dateClosed'])
HMA_MP$dateClosed <- date(temp + ymd(origin))

HMA_MP <- merge(disasters, HMA_MP, by = 'disasterNumber', all.y = TRUE)
HMA_MP <- HMA_MP[HMA_MP$incidentType %in% c(NA, 'Flood', 'Severe Storm(s)', 'Dam/Levee Break'),]
HMA_MP <- HMA_MP[HMA_MP$propertyAction %in% c('Elevation', 'Acquisition', 'Relocation', 'Floodproofed', 'Other'),]
HMA_MP <- HMA_MP[HMA_MP$county == 'Sonoma',]


#### look at other housing assistance ####



