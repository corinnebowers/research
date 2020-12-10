
cat('\n#### REGRESSION_COUNTY ####\n\n')

## packages #######################################################################################
cat('loading packages...\n')
suppressMessages({
  require(ggplot2); theme_set(theme_bw())
  require(sf)
  require(raster)
  require(reshape2)
  require(tidyr)
  require(elevatr)
  require(dplyr)
  require(tigris); options(tigris_use_cache = TRUE)
  require(stringr)
  require(ncdf4)
  require(lubridate)
  require(units)
  require(dataRetrieval)
  require(rnoaa)
  require(exactextractr)
  require(censusapi); Sys.setenv(CENSUS_KEY = 'f2e090156b02ced027d4ed756f82c9a3a1aa38c9')
  require(foreach)
  require(parallel)
  require(doParallel)
  require(readr)
})


## setup ##########################################################################################
cat('initializing code...\n')

## global options ####
options(readr.num_columns = 0)
options(show.error.locations = TRUE)

## parallel backend ####
num_cores <- as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))*2/3

## helper functions ####
toNumber <- function(x) as.numeric(paste(x))
Mean <- function(x) ifelse(sum(is.na(x)) == length(x), NA, mean(x, na.rm = TRUE))
Sum <- function(x) sum(x, na.rm = TRUE)
Max <- function(x) max(x, na.rm = TRUE) 
Min <- function(x) min(x, na.rm = TRUE)

## EPSG coordinates ####
NAD <- 4269
albers <- 3310

## NFIP data #### 
load('./_data/NFIP/NFIP.Rdata'); claims$counter <- 1

## MERRA data ####
load('./_data/MERRA/Rutzcatalog.Rdata')
datelist <- seq(ymd('1980-01-01'), ymd('2017-12-31'), 'days')
hourlist <- rep(datelist, each = 8) #+ hours(rep(seq(0, 21, 3), length(datelist)))
LON <- seq(-105, -150, -0.625)
LAT <- seq(27.5, 52.5, 0.5)

## geometries #### 
california <- counties(state = 'CA', class = 'sf') %>% 
  dplyr::select(COUNTYFP, GEOID, NAME, NAMELSAD, ALAND, AWATER)
claims <- claims %>% 
  left_join(data.frame(countycode = 6000+toNumber(california$COUNTYFP), 
                       COUNTYNAME = california$NAME), by = 'countycode')

## create catalog #################################################################################
cat('creating county_catalog...\n')
timer <- Sys.time()

cat('...loading data...\n')
load('./_data/grid_catalog.Rdata')  #list of ARs by cell (ar_grid) and a spatial tracker (tracker)
ar.threshold <- 0.5 
tracker.raster <- tracker[,c(2,1,3,4)]
tracker.raster <- rasterFromXYZ(tracker.raster, crs = st_crs(california)$proj4string) 

cat('...looping through counties...\n')
cl <- makeCluster(num_cores)
registerDoParallel(cl)
county_catalog <- 
  foreach (c = 1:nrow(california), 
           .combine = 'rbind',
           .packages = c('dplyr', 'raster', 'sf', 'lubridate'),
           .inorder = FALSE) %dopar% {
    ## get cells associated with each county         
    county = california[c,]
    tracker.id <- rasterize(county, tracker.raster, getCover = TRUE, silent = TRUE) %>% 
     as.data.frame(xy = TRUE) %>% 
     subset(layer > 0) %>% 
     left_join(tracker, by = c('x'='lon', 'y'='lat')) %>% 
     dplyr::select(step) %>% 
     unlist %>% unname %>% sort  
    tracker.id <- tracker.id[!(tracker.id %in% c(827,1185))]  ## these ones cause issues
    
    ## identify all AR storms in the region of interest
    storm.df <- data.frame(date = seq(ymd('1980-01-01'), ymd('2019-12-31'), 'days'))
    storm.df[,paste(tracker.id)] <- 0
    hour.df <- storm.df
    for (id in 1:length(tracker.id)) {
     ar_list <- ar_grid[[tracker.id[id]]]
     ar_list$total_hours <- (ymd_h(paste(ar_list$end_date, ar_list$end_hour)) -
                               ymd_h(paste(ar_list$start_date, ar_list$start_hour))) %>% 
       as.numeric(units = 'hours')
     for (i in 1:nrow(ar_list)) {
       storm <- seq(ymd(ar_list[i, 'start_date']), ymd(ar_list[i, 'end_date']), 'days')
       storm.df[storm.df$date %in% storm, id+1] <- ar_list[i, 'IVT_max']
       hour.df[storm.df$date == storm[1], id+1] <- ar_list[i, 'total_hours']
     }
    }
    ## subset to rainy season
    storm.df <- storm.df %>% subset(month(date) %in% c(10:12, 1:3))
    hour.df <- hour.df %>% subset(month(date) %in% c(10:12, 1:3))
    storm.df$storm <- apply(storm.df[,2:(length(tracker.id)+1)], 1, function(x) sum(x > 0))
    
    ## number AR storms
    storm.df$ar <- 0
    ar <- 0
    for (i in 2:nrow(storm.df)) {
     if (storm.df[i, 'storm'] >= length(tracker.id)*ar.threshold) {
       if (storm.df[i-1, 'storm'] <= length(tracker.id)*ar.threshold) {
         ar <- ar + 1
       }
       storm.df[i, 'ar'] <- ar
     }
    }
    ## make a catalog
    catalog <- data.frame(AR = 1:max(storm.df$ar))
    for (ar in 1:max(storm.df$ar)) {
     catalog$start_day[ar] <- paste(min(ymd(storm.df[storm.df$ar == ar, 'date'])))
     catalog$end_day[ar] <- paste(max(ymd(storm.df[storm.df$ar == ar, 'date'])))
     catalog$IVT_max[ar] <- max(storm.df[storm.df$ar == ar, 2:(ncol(storm.df)-2)])
     catalog$duration[ar] <- max(apply(hour.df[storm.df$ar == ar, 2:(ncol(storm.df)-2)], 2, sum))
    }
    catalog %>% mutate(county = county$NAME)
  }

cat('...collating results...\n')
county_catalog <- county_catalog %>% 
  mutate(wateryear = year(start_day) + ifelse(month(start_day) %in% 10:12, 1, 0))
Sys.time() - timer
stopCluster(cl)

## get FEMA variables #############################################################################
cat('getting FEMA variables...\n')

## load data ####
load('./_data/FEMA/FEMA_API.Rdata')

## find flood experience (number of declared disasters in the last 5 years) ####
flood_declarations <- disaster_declarations %>%
  filter(grepl('FLOOD', declarationTitle)) %>%
  filter(incidentType != 'Fire')
flood_declarations_wy <- flood_declarations %>%
  mutate(wateryear = year(ymd_hms(incidentBeginDate))) %>%
  group_by(COUNTYFP = fipsCountyCode, wateryear) %>%
  summarize(num_floods = length(unique(disasterNumber)), .groups = 'drop')
flood_experience <- expand.grid(wy = 1980:2020, county = california$NAME)
flood_experience$flood_experience <- NA
# flood_experience$flood_experience_1yr <- NA
# flood_experience$flood_experience_5yr <- NA
# flood_experience$flood_experience_10yr <- NA
for (c in 1:nrow(california)) {
  for (wy in 1980:2020) {
    ## disaster declared in the last water year
    # floods <- flood_declarations_wy %>%
    #   filter(wateryear == wy-1) %>%
    #   filter(COUNTYFP == california$COUNTYFP[c])
    # flood_experience[flood_experience$wy == wy &
    #                    flood_experience$county == california$NAME[c],
    #                  'flood_experience_1yr'] <- (nrow(floods)>0)
    
    ## disaster declared in the last 5 water years
    floods <- flood_declarations_wy %>%
      filter(wateryear %in% (wy-5):(wy-1)) %>%
      filter(COUNTYFP == california$COUNTYFP[c])
    flood_experience[flood_experience$wy == wy &
                       flood_experience$county == california$NAME[c],
                     'flood_experience'] <- (nrow(floods)>0)
    
    # ## disaster declared in the last 10 water years
    # floods <- flood_declarations_wy %>%
    #   filter(wateryear %in% (wy-10):(wy-1)) %>%
    #   filter(COUNTYFP == california$COUNTYFP[c])
    # flood_experience[flood_experience$wy == wy &
    #                    flood_experience$county == california$NAME[c],
    #                  'flood_experience_10yr'] <- (nrow(floods)>0)
  }
}
county_catalog <- county_catalog %>% 
  left_join(flood_experience, by = c('county', 'wateryear' = 'wy'))

## match ARs to disaster declarations ####
flood_declarations <- flood_declarations %>%
  mutate(declarationDate = as.Date(ymd_hms(declarationDate)),
         incidentBeginDate = as.Date(ymd_hms(incidentBeginDate)),
         incidentEndDate = as.Date(ymd_hms(incidentEndDate))) %>%
  left_join(california %>% st_drop_geometry %>% dplyr::select(COUNTYFP, NAME),
            by = c('fipsCountyCode' = 'COUNTYFP'))

county_catalog$disaster_declaration <- NA
for (i in 1:nrow(california)) {
  catalog_subset <- county_catalog %>% filter(county == california$NAME[i])
  flood_declarations_subset <- flood_declarations %>% filter(NAME == california$NAME[i])
  county_ARdays <-
    foreach (d = 1:nrow(catalog_subset), .combine = 'rbind') %do% {
      seq(ymd(catalog_subset$start_day[d]), ymd(catalog_subset$end_day[d]), 'days') %>% 
        paste %>% cbind(catalog_subset$AR[d])
    }
  county_ARdays <- county_ARdays %>% as.data.frame %>% setNames(c('date', 'AR'))
  
  for (flood in 1:nrow(flood_declarations_subset)) {
    index <- which(
      county_ARdays$date %in% 
        paste(seq(flood_declarations_subset$incidentBeginDate[flood],
                  flood_declarations_subset$incidentEndDate[flood], 'days')))
    county_catalog[
      county_catalog$county == california$NAME[i] &
        county_catalog$AR %in% unique(county_ARdays[index, 'AR']), 
      'disaster_declaration'] <- flood_declarations_subset$disasterNumber[flood]
  }
}

# ## housing assistance ####
# flood_housing_assistance <- disaster_declarations %>%
#   right_join(housing_assistance %>% mutate(disasterNumber = toNumber(disasterNumber)),
#              by = c('disasterNumber', 'designatedArea' = 'county')) %>%
#   filter(grepl('FLOOD', declarationTitle)) %>%
#   filter(incidentType != 'Fire') %>%
#   mutate(declarationDate = as.Date(ymd_hms(declarationDate)),
#          incidentBeginDate = as.Date(ymd_hms(incidentBeginDate)),
#          incidentEndDate = as.Date(ymd_hms(incidentEndDate))) %>%
#   mutate(validRegistrations = validRegistrations.own + validRegistrations.rent,
#          approvedForFemaAssistance = approvedForFemaAssistance.own + approvedForFemaAssistance.rent,
#          totalApprovedIhpAmount = totalApprovedIhpAmount.own + totalApprovedIhpAmount.rent,
#          totalInspected = totalInspected.own + totalInspected.rent,
#          damagedHomesInspected = totalInspected -
#            (noFemaInspectedDamage.own + totalInspectedWithNoDamage.rent)) %>%
#   dplyr::select(disasterNumber, incidentType, declarationTitle, declarationDate, incidentBeginDate,
#          incidentEndDate, fipsCountyCode, validRegistrations, approvedForFemaAssistance,
#          totalApprovedIhpAmount, totalInspected, damagedHomesInspected) %>%
#   group_by(disasterNumber, fipsCountyCode) %>%
#   summarize(across(where(is.Date), first), across(where(is.numeric), Sum), .groups = 'drop') %>%
#   mutate(percentDamaged = 1 - (damagedHomesInspected/totalInspected),
#          approvalRate = approvedForFemaAssistance/validRegistrations) %>%
#   right_join(california %>% st_drop_geometry %>% dplyr::select(COUNTYFP, NAME), .,
#              by = c('COUNTYFP' = 'fipsCountyCode'))
# 
# ## public assistance ####
# flood_public_assistance <- disaster_declarations %>%
#   separate(designatedArea, c('county', NA), sep = ' \\(', fill = 'right') %>%
#   mutate(declarationDate = as.Date(ymd_hms(declarationDate)),
#          incidentBeginDate = as.Date(ymd_hms(incidentBeginDate)),
#          incidentEndDate = as.Date(ymd_hms(incidentEndDate))) %>%
#   right_join(public_assistance %>%
#                mutate(disasterNumber = toNumber(disasterNumber)) %>%
#                dplyr::select(-incidentType, -declarationDate),
#              by = c('disasterNumber', 'county')) %>%
#   filter(incidentType %in% c('Severe Storm(s)', 'Flood')) %>%
#   dplyr::select(county, disasterNumber, incidentBeginDate, incidentEndDate, projectAmount) %>%
#   group_by(county, disasterNumber) %>%
#   summarize(incidentBeginDate = first(incidentBeginDate),
#             incidentEndDate = first(incidentEndDate),
#             projectAmount = sum(projectAmount),
#             .groups = 'drop')
# 
# ## hazard mitigation ####
# flood_mitigated_properties <- HMA_properties %>%
#   filter(propertyAction %in% c('Elevation', 'Floodproofed') |
#            grepl('flood', str_to_lower(type)) |
#            grepl('flood', str_to_lower(title)) |
#            programArea == 'FMA') %>%
#   mutate(dateApproved = as.Date(ymd_hms(dateApproved)),
#          wateryear = year(dateApproved) + ifelse(month(dateApproved) %in% 10:12, 1, 0)) %>%
#   group_by(county, wateryear) %>%
#   summarize(properties = sum(numberOfProperties), .groups = 'drop')


## get precipitation & soil moisture ##############################################################
cat('getting precipitation & soil moisture for each event...\n')
timer <- Sys.time()

## open soil moisture file
soil_nc <- nc_open('./_data/soilmoisture/soilw.mon.mean.v2.nc')
# soil_nc <- nc_open('./_data/soilmoisture/NOAA/soilw.mon.mean.v2.nc')
soil_lat <- ncvar_get(soil_nc, 'lat')
soil_lon <- ncvar_get(soil_nc, 'lon') - 180
soil_time <- ymd('1800-01-01') + days(ncvar_get(soil_nc, 'time'))
soil_time <- data.frame(month = month(soil_time), year = year(soil_time))
soil <- ncvar_get(soil_nc, 'soilw')
nc_close(soil_nc)

## open precipitation file
load('./_data/daily_precip_by_county.Rdata')

## add hazard variables to catalog
cl <- makeCluster(num_cores)
registerDoParallel(cl)
catalog_vars <- 
  foreach (ar = 1:nrow(county_catalog),
           .combine = 'rbind',
           .packages = c('dplyr', 'lubridate', 'raster', 'exactextractr'),
           .inorder = FALSE) %dopar% {
    ## define input variables
    start <- ymd(county_catalog$start_day[ar]) - days(1)
    end <- ymd(county_catalog$end_day[ar]) + days(1)
    wy <- county_catalog$wateryear[ar]
    
    ## add damage data
    vars <- claims %>% 
     subset(COUNTYNAME == county_catalog$county[ar]) %>% 
     subset(ymd(dateofloss) >= ymd(county_catalog$start_day[ar])-days(1) & 
              ymd(dateofloss) <= ymd(county_catalog$end_day[ar])+days(1)) %>%
     summarize(num_claims = length(amountpaidonbuildingclaim),
               value_claims = Sum(amountpaidonbuildingclaim) + 
                 Sum(amountpaidoncontentsclaim)) %>% unlist
    
    ## storm total/max rainfall
    storm <- seq(start, end, 'days')
    precip.storm <- precip %>% 
     subset(paste(date) %in% paste(storm)) %>% 
     dplyr::select(county_catalog$county[ar])
    vars['precip.total'] <- sum(precip.storm)
    vars['precip.max'] <- max(precip.storm)
    
    ## max storm length (consecutive days of rainfall)
    vars['days.max'] <- sum(unlist(precip.storm) > 0)
    
    ## cumulative rainfall at various points
    prevweek <- seq(start - days(7), start - days(1), 'days')
    vars['precip.prevweek'] <- precip %>% 
     subset(paste(date) %in% paste(prevweek)) %>% 
     dplyr::select(county_catalog$county[ar]) %>% sum
    prevmonth <- seq(start - days(31), start - days(1), 'days')
    vars['precip.prevmonth'] <- precip %>% 
     subset(paste(date) %in% paste(prevmonth)) %>% 
     dplyr::select(county_catalog$county[ar]) %>% sum
    season <- seq(ymd(paste(wy-1, 9, 30)), start, 'days')
    vars['precip.season'] <- precip %>% 
     subset(paste(date) %in% paste(season)) %>% 
     dplyr::select(county_catalog$county[ar]) %>% sum
    
    ## soil moisture
    SM_stack <- raster::stack()
    monthrecord <- ifelse(month(start) <= month(end),
                         length(month(start):month(end)),
                         length((month(start)-12):month(end))) - 1
    startmonth <- ymd(paste(year(start), month(start), '1', sep = '-'))
    for (i in 0:monthrecord) {
     mo <- month(startmonth + months(i))
     yr <- year(startmonth + months(i))
     index <- (1:nrow(soil_time))[soil_time$month == mo & soil_time$year == yr]
     SM_raster <- raster(t(soil[,,index]), xmn = min(soil_lon), xmx = max(soil_lon), 
                         ymn = min(soil_lat), ymx = max(soil_lat))
     SM_stack <- raster::stack(SM_stack, SM_raster)
    }
    SM_avg <- mean(SM_stack); crs(SM_avg) <- projection(california)
    vars['soilmoisture'] <- 
      exact_extract(SM_avg, california %>% subset(NAME == county_catalog$county[ar]), 
                    'mean', progress = FALSE)
    ## return result
    c('AR' = county_catalog$AR[ar], 'county' = county_catalog$county[ar], vars)
  }
stopCluster(cl)
catalog_vars <- data.frame(catalog_vars)
catalog_vars$county <- paste(catalog_vars$county)
catalog_vars[,-2] <- apply(catalog_vars[,-2], 2, toNumber)

county_catalog <- county_catalog %>% left_join(catalog_vars, by = c('AR', 'county'))
Sys.time() - timer


## get location-based variables ###################################################################
cat('getting location-based variables...\n')

## land use/land cover
cat('...land use/land cover...\n')
timer <- Sys.time()
get.lulc <- function(x) x %>%
  group_by(value) %>%
  summarize(coverage = sum(coverage_fraction), .groups = 'drop') %>%
  mutate(fraction = prop.table(coverage)) %>%
  filter(value %in% 21:24) %>% #filter to developed classes
  .$fraction %>% sum
lulc <- raster('./_gis/NLCD_2016_Land_Cover_L48_20190424/NLCD_2016_Land_Cover_L48_20190424.img')
california$developed <- california %>%
  st_transform(albers) %>%
  st_transform(proj4string(lulc)) %>%
  exact_extract(lulc, ., progress = FALSE) %>%
  lapply(get.lulc) %>% unlist
Sys.time() - timer

#### imperviousness
cat('...imperviousness...\n')
timer <- Sys.time()
imperv <- raster('./_gis/NLCD_2016_Impervious_L48_20190405/NLCD_2016_Impervious_L48_20190405.img')
california$impervious <- exact_extract(imperv, california, 'mean', progress = FALSE)
Sys.time() - timer


#### percent within floodplain
cat('...percent within floodplain...\n')
timer <- Sys.time()
floodzone <- function(x) {
  if (x %in% c('A', 'A99', 'AE', 'AH', 'AO', 'V', 'VE')) {
    return('YES')
  } else if (x %in% c('D', 'X')) {
    return('NO')
  } else if (x == 'OPEN WATER') {
    return('WATER')
  } else {
    return(NA)
  }
}
NFHL <- st_read('./_gis/NFHL_06_20190810/S_Fld_Haz_Ar.shp', quiet = TRUE) %>% 
  st_transform(albers) %>% 
  mutate(FLOODPLAIN = factor(apply(data.frame(FLD_ZONE), 1, function(x) floodzone(x)))) %>% 
  filter(FLOODPLAIN == 'YES') %>% 
  st_buffer(dist = 0) %>% 
  select(FLOODPLAIN) %>% 
  st_intersection(california %>% st_transform(albers) %>% select(county = NAME)) %>% 
  mutate(part_area = st_area(.)) %>% 
  st_drop_geometry %>%
  group_by(county) %>%
  summarize(part_area = Sum(part_area), .groups = 'drop')
california <- california %>% 
  select(county = NAME) %>% 
  mutate(total_area = st_area(.)) %>% 
  full_join(NFHL, by = 'county') %>%
  mutate(pct_floodplain = toNumber(part_area/total_area)) %>% 
  mutate(pct_floodplain = ifelse(is.na(pct_floodplain), 0, pct_floodplain)) %>% 
  st_drop_geometry %>% 
  select(county, pct_floodplain) %>% 
  full_join(california, ., by = c('NAME' = 'county'))
Sys.time() - timer

# #### distance from the river
# cat('...distance from major rivers...\n')
# rivers <- st_read('./_gis/California/_hydrology/nhd_majorrivers/MajorRivers.shp', quiet = TRUE) %>%
#   st_zm %>% st_transform(albers)
# california$river_dist <- california %>% 
#   st_transform(albers) %>% 
#   st_centroid %>% 
#   st_distance(., y = rivers) %>% 
#   drop_units %>% 
#   apply(1, function(x) min(x)/1609.34)

#### elevation
cat('...elevation...\n')
california <- california %>% 
  st_transform(albers) %>% 
  st_centroid %>% 
  st_transform(NAD) %>% 
  get_elev_point %>% 
  st_drop_geometry %>% 
  select(NAME, elevation_m = elevation) %>% 
  full_join(california, ., by = 'NAME') 

#### ACS estimates (2018 5-year)
acs_vars <- listCensusMetadata(name = 'acs/acs5', vintage = 2018)
subject_vars <- listCensusMetadata(name = 'acs/acs5/subject', vintage = 2018)
profile_vars <- listCensusMetadata(name = 'acs/acs5/profile', vintage = 2018)

cat('...ACS population...\n')
california <- 
  getCensus(name = 'acs/acs5/profile', vars = 'DP05_0001E', vintage = 2018, 
            regionin = 'state:06', region = 'county:*') %>% 
  select(county, population = DP05_0001E) %>% 
  full_join(california, ., by = c('COUNTYFP' = 'county'))

cat('...ACS housing...\n')
california <- 
  getCensus(name = 'acs/acs5/profile', vars = 'group(DP04)', vintage = 2018,
            regionin = 'state:06', region = 'county:*') %>% 
  select(-state, -GEO_ID, -NAME) %>% 
  select(-ends_with('M'), -ends_with('A'), -ends_with('PE')) %>% 
  pivot_longer(cols = ends_with('E'), names_to = 'variable', values_to = 'estimate') %>% 
  left_join(profile_vars %>% select('name', 'label'), by = c('variable' = 'name')) %>% 
  separate(label, c(NA, 'group_title', 'group', 'description'), sep = '!!', fill = 'right') %>% 
  filter(grepl('1-unit', description)) %>% 
  group_by(county) %>% 
  summarize(SFH = sum(estimate), .groups = 'drop') %>% 
  full_join(california, ., by = c('COUNTYFP' = 'county'))

cat('...ACS median structure age...\n')
med_struct_age <- 
  getCensus(name = 'acs/acs5/profile', vars = 'group(DP04)', vintage = 2018,
            regionin = 'state:06', region = 'county:*') %>% 
  select(-state, -GEO_ID, -NAME) %>% 
  select(-ends_with('M'), -ends_with('A'), -ends_with('PE')) %>% 
  pivot_longer(cols = ends_with('E'), names_to = 'variable', values_to = 'estimate') %>% 
  left_join(profile_vars %>% select('name', 'label'), by = c('variable' = 'name')) %>% 
  separate(label, c(NA, 'group_title', 'group', 'description'), sep = '!!', fill = 'right') %>% 
  filter(group_title == 'YEAR STRUCTURE BUILT') %>%
  filter(!is.na(description)) %>% 
  select(county, description, estimate) %>% 
  pivot_wider(id_cols = county, names_from = description, values_from = estimate)
california <- med_struct_age[,11:2] %>% 
  apply(1, function(x) cumsum(x)/sum(x)) %>% t %>% 
  apply(1, function(x) c(x[last(which(x < 0.5))], last(which(x < 0.5)), 
                         x[first(which(x > 0.5))])) %>% t %>% 
  as.data.frame %>% setNames(c('prop.start', 'id.start', 'prop.end')) %>% 
  cbind(county = med_struct_age$county, .) %>% 
  mutate(id.end = id.start + 1) %>% 
  left_join(data.frame(year = c(seq(1939, 2009, 10), 2013, 2020), id = 1:10), 
            by = c('id.start' = 'id')) %>% rename(year.start = year) %>% 
  left_join(data.frame(year = c(seq(1939, 2009, 10), 2013, 2020), id = 1:10), 
            by = c('id.end' = 'id')) %>% rename(year.end = year) %>%
  mutate(med_struct_age = round(year.start + (0.5-prop.start)*
                                  (year.end-year.start)/(prop.end-prop.start))) %>% 
  select(county, med_struct_age) %>% 
  full_join(california, ., by = c('COUNTYFP' = 'county'))

# cat('...ACS age vulnerability...\n')
# pop_age <-
#   getCensus(name = 'acs/acs5/subject', vars = 'group(S0101)', vintage = 2018,
#             regionin = 'state:06', region = 'county:*') %>% 
#   select(-state, -GEO_ID, -NAME) %>% 
#   select(-ends_with('M'), -ends_with('A')) %>% 
#   pivot_longer(cols = ends_with('E'), names_to = 'variable', values_to = 'estimate') %>% 
#   left_join(subject_vars %>% select('name', 'label'), by = c('variable' = 'name')) %>% 
#   separate(label, c(NA, 'sex', NA, 'age_type', 'age'), sep = '!!', fill = 'right') %>% 
#   filter(grepl('Total', sex)) %>% 
#   filter(!is.na(age)) %>% 
#   filter(age_type == 'SUMMARY INDICATORS')
# california <- pop_age %>% 
#   pivot_wider(id_cols = county, names_from = age, values_from = estimate) %>% 
#   dplyr::select(county,
#                 oldage_dependency = `Old-age dependency ratio`,
#                 child_dependency = `Child dependency ratio`) %>% 
#   full_join(california, ., by = c('COUNTYFP' = 'county'))
  
cat('...ACS median income...\n')
med_income <- 
  getCensus(name = 'acs/acs5', vars = 'group(B19001)', vintage = 2018,
            regionin = 'state:06', region = 'county:*') %>% 
  select(-state, -GEO_ID, -NAME) %>% 
  select(-ends_with('M'), -ends_with('A')) %>% 
  pivot_longer(cols = ends_with('E'), names_to = 'variable', values_to = 'estimate') %>% 
  left_join(acs_vars %>% select('name', 'label'), by = c('variable' = 'name')) %>% 
  separate(label, c(NA, NA, 'income_bracket'), sep = '!!', fill = 'right') %>% 
  filter(!is.na(income_bracket)) %>% 
  pivot_wider(id_cols = county, names_from = income_bracket, values_from = estimate)
income_bracket <- 1e3*c(seq(10, 50, 5), 60, 75, 100, 125, 150, 200, 1000)  
california <- med_income[,-1] %>% 
  apply(1, function(x) cumsum(x)/sum(x)) %>% t %>% 
  apply(1, function(x) c(x[last(which(x < 0.5))], last(which(x < 0.5)), 
                         x[first(which(x > 0.5))])) %>% t %>% 
  as.data.frame %>% setNames(c('prop.start', 'id.start', 'prop.end')) %>% 
  cbind(county = med_income$county, .) %>% 
  mutate(id.end = id.start + 1) %>% 
  left_join(data.frame(income.start = income_bracket, id = 1:16), by = c('id.start' = 'id')) %>% 
  left_join(data.frame(income.end = income_bracket, id = 1:16), by = c('id.end' = 'id')) %>% 
  mutate(med_income = round(income.start + (0.5-prop.start)*
                              (income.end-income.start)/(prop.end-prop.start))) %>% 
  select(county, med_income) %>% 
  full_join(california, ., by = c('COUNTYFP' = 'county'))

cat('...ACS income inequality...\n')
california <- 
  getCensus(name = 'acs/acs5', vars = 'group(B19083)', vintage = 2018,
            regionin = 'state:06', region = 'county:*') %>% 
  select(county, gini_index = B19083_001E) %>% 
  full_join(california, ., by = c('COUNTYFP' = 'county'))


#### CDC social vulnerability index
cat('...social vulnerability...\n')
svi <- st_read('./_gis/SVI2018_CALIFORNIA_county/SVI2018_CALIFORNIA_county.shp', quiet = TRUE)
california <- svi %>% 
  st_drop_geometry %>% 
  dplyr::select(FIPS, SVI_socioecon = SPL_THEME1, SVI_household = SPL_THEME2, 
                SVI_minority = SPL_THEME3, SVI_houstrans = SPL_THEME4) %>% 
  full_join(california, ., by = c('GEOID' = 'FIPS'))


## get time-based variables #######################################################################
cat('getting time-based variables...\n')
timer <- Sys.time()

## set up dataframe
df_time <- data.frame(DATE = seq(ymd('1975-01-01'), ymd('2019-12-31'), 'days'))
df_time$YEAR <- year(df_time$DATE)
df_time$MONTH <- month(df_time$DATE)

##  PDO & ENSO
load('./_data/PDO_ENSO.Rdata')
df_time <- merge(df_time, PDO, by = c('YEAR', 'MONTH'), all.x = TRUE)
df_time <- merge(df_time, ENSO, by = c('YEAR', 'MONTH'), all.x = TRUE)

## days since start of rainy season
df_time$WATERYEAR <- ifelse(df_time$MONTH %in% 10:12, df_time$YEAR + 1, df_time$YEAR)
df_time$rainyseason <- toNumber(df_time$DATE - ymd(paste(df_time$WATERYEAR-1, '-10-1', sep = '')))

## CRS 
CA_CID <- read_csv('./_data/CRS FOIA/CA.csv', skip = 4) %>% filter(!is.na(CID))
load('./_data/CRS FOIA/crs_merged.Rdata')
crs.county <- crs %>% 
  filter(State == 'CA') %>% 
  filter(!is.na(cTot)) %>% 
  filter(!is.na(`Community Name`)) %>% 
  group_by(CID, Year) %>% 
  summarize(community = `Community Name`, score = Mean(toNumber(cTot)), .groups = 'drop') %>% 
  left_join(CA_CID %>% mutate(CID = str_sub(CID, end = 6)), by = 'CID') %>% 
  left_join(california %>% 
              select(NAMELSAD, NAME) %>% 
              mutate(NAMELSAD = str_to_upper(NAMELSAD)) %>% 
              st_drop_geometry, 
            by = c('County' = 'NAMELSAD')) %>% 
  select(CID, year = Year, county = NAME, map_date = `Curr Eff\nMap Date`, score) %>% 
  group_by(county, year) %>% 
  summarize(map_date = Max(mdy(map_date)), score = mean(score), .groups = 'drop') %>% 
  pivot_wider(id_cols = c(county, map_date), names_from = year, values_from = score) %>% 
  group_by(county) %>% 
  summarize(map_date = Max(ymd(map_date)), 
            across(where(is.numeric), ~Mean(.x)), .groups = 'drop')
temp <- crs.county %>% apply(1, function(x) last(which(!is.na(x)))) 
for (i in 1:length(which(temp < 24))) {
  row <- which(temp < 24)[i]
  crs.county[row, (temp[row]+1):24] <- crs.county[row, temp[row]]
}
crs.county <- crs.county %>% 
  pivot_longer(cols = c(-county, -map_date), names_to = 'year', values_to = 'score')

## transfer time information to ARs
county_catalog$CRS <- NA
for (ar in 1:nrow(county_catalog)) {
  ## define input variables
  start <- ymd(county_catalog$start_day[ar]) - days(1)
  end <- ymd(county_catalog$start_day[ar]) + days(1)
  wy <- county_catalog$wateryear[ar]
  df_time_start <- df_time %>% subset(DATE == start)
  df_time_subset <- df_time %>% subset(DATE >= start & DATE <= end)

  ## assign variables to ARs
  county_catalog$PDO[ar] <- Mean(toNumber(df_time_subset$PDO))
  county_catalog$ENSO[ar] <- Mean(toNumber(df_time_subset$ENSO))
  county_catalog$rainyseason[ar] <- df_time_start$rainyseason

  crs.ar <- crs.county %>% filter(year == wy & county == county_catalog$county[ar])
  if (nrow(crs.ar) == 1) {
    county_catalog$CRS[ar] <- crs.ar$score
  }
}
Sys.time() - timer


## save out #######################################################################################
cat('showing warnings...\n')
warnings()

cat('saving results...\n')
save(california, county_catalog, file = './_results/regression_dataframe_1117.Rdata')
cat('done!\n\n')

