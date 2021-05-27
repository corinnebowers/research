
###################################################################################################

## @param
## hydrograph (data.frame): list of synthetic hydrographs
## grid (data.frame): list of existing LISFLOOD runs
## buildings (data.frame): geographic information for buildings of interest
## probabilistic (logical): choice to incorporate uncertainty
## n.inun (integer): number of inundation realizations to generate per hydrograph
## n (double): size of search neighborhood for IDW interpolator
## p (double): power function for IDW interpolator
## alpha (double): anisotropy correction factor for IDW interpolator

## @return
## inundation (data.frame): list of inundation heights (meters) by building & simulation

generate_inundation <- function(hydrograph, buildings, foundations, 
                                probabilistic = FALSE, n.inun = 1, 
                                n = 5, p = 1, alpha = 0.89) {

  ## fix input parameters
  if (!probabilistic) n.inun <- 1

  ## separate building coordinates from building info
  buildings.coord <- buildings %>% st_set_crs(NAD) %>% 
    st_transform(crs(dem)) %>% st_coordinates
  
  ## load sample information
  samples <- read.table('C:/Users/cbowers/Desktop/samples_grid.txt', header = TRUE)
  # samples <- read.table('C:/Users/cbowers/Desktop/samples_grid.txt', header = TRUE)
  samples_scale <- samples %>% 
    mutate(Qp.std = Qp %>% punif(min = 0, max = 8000) %>% 
             qunif(min = 1, max = exp(2)) %>% log,
           Qp.norm = qnorm(Qp.std/2)) %>% 
    mutate(tp.std = tp %>% punif(min = 0, max = 200) %>% 
             qunif(min = 1, max = exp(2)) %>% log,
           tp.norm = qnorm(tp.std/2)) %>% 
    mutate(sim = 1:nrow(.))
  
  samples_scale_new <- samples_scale
  samples_new <- samples
  load('C:/Users/cbowers/Desktop/samples.Rdata')

  print('calculating inundation depths...')
  
  ## calculate inundation depth at each building
  pb <- txtProgressBar(min = 0, max = n.inun*nrow(hydrograph), style = 3)
  inundation.list <- 
    foreach (inun = 1:n.inun) %:% 
    foreach (i = 1:nrow(hydrograph), 
      .packages = c('raster', 'terra', 'dplyr', 'foreach', 'pracma'),
      .export = c('surrogate'),
      .combine = 'cbind',
      .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
        surrogate(
          Qp = hydrograph$Qp_m3s[i], tp = hydrograph$tp_hrs[i],
          sample.table = samples_scale,
          sample.loc = '/home/groups/bakerjw/cbowers/LISFLOOD/grid/maxfiles/',
          # sample.loc = 'C:/Users/cbowers/Desktop/LISFLOOD/sonoma_sherlock/21-02-21 gridded/results/',
          n, p, alpha, probabilistic) %>% 
          rast %>% terra::extract(buildings.coord)
        }
  cat('\n')
  
  ## clean out buildings + simulations that never flood
  wet.bldg <- inundation.list %>% 
    lapply(FUN = function(inun) apply(inun, 1, function(x) sum(x)>0)) %>% 
    do.call(cbind, .) %>% apply(1, all) %>% which
  wet.sim <- inundation.list %>% 
    lapply(FUN = function(inun) apply(inun, 2, function(x) sum(x)>0)) %>% 
    do.call(cbind, .) %>% apply(1, all) %>% which
  inundation <- inundation.list %>% 
    lapply(function(inun) inun[wet.bldg, wet.sim] %>% as.matrix)
  rm(inundation.list)

  ## attach building + simulation information
  attr(inundation, 'wet.bldg') <- wet.bldg
  attr(inundation, 'buildings') <- cbind(id = 1:nrow(buildings), buildings.coord)[wet.bldg,]
  attr(inundation, 'sim') <- hydrograph[wet.sim,] %>% select(starts_with('n'))
  attr(inundation, 'n.inun') <- ifelse(probabilistic, n.inun, NA)

  return(inundation)
}


###################################################################################################

## @param
## Qp (double): value of peak flow (m3/s) we want to calculate inundation for
## tp (double): value of time to peak flow (hrs) we want to calculate inundation for
## grid (data.frame): list of existing LISFLOOD runs
## n (double): size of search neighborhood for IDW interpolator
## p (double): power function for IDW interpolator
## alpha (double): anisotropy correction factor for IDW interpolator
## probabilistic (logical): choice to incorporate uncertainty

## @return
## prediction (raster): inundation map predicted by IDW interpolator

surrogate <- function(sample.table, sample.loc, Qp, tp, n, p, alpha, probabilistic) {

  ## perform normal score transformation on new point
  Qp.new <- interp1(sort(sample.table$Qp), sort(sample.table$Qp.norm), Qp)*alpha
  tp.new <- interp1(sort(sample.table$tp), sort(sample.table$tp.norm), tp)*(1-alpha)
  
  ## find distances to new point
  distances <- sample.table %>% 
    mutate(Qp.scale = Qp.norm*alpha, tp.scale = tp.norm*(1-alpha)) %>% 
    mutate(dist = sqrt((Qp.scale-Qp.new)^2 + (tp.scale-tp.new)^2)) %>% 
    dplyr::select(sim, dist) %>% arrange(dist)

  if (distances[1,'dist']==0) {
    ## predict based on exact match
    prediction <- raster(paste0(sample.loc, 'gridded', distances[1,'sim'], '.max'))
    
  } else {
    ## find the nearest maps
    nearest <- distances[1:n, 'sim']
    
    ## calculate the weights of the nearest maps
    weights <- 1/(distances[1:n, 'dist']^p)
    weights <- weights/sum(weights)
    
    if (probabilistic) {
      ## choose a random map from weighted sampling
      prediction <- sample(nearest, 1, prob = weights) %>% 
        paste0(sample.loc, 'gridded', ., '.max') %>% raster
    } else {
      ## calculate weighted average of all maps
      files <- paste0(sample.loc, 'gridded', nearest, '.max')
      prediction <- foreach (ii = 1:n, .combine = '+') %do% (raster(files[ii])*weights[ii])
    }
  }
  return(prediction)
}



