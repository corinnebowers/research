
## @param
## fileloc (string): location of lisflood.exe (has to end with a slash)
## dem (raster): raster elevation file 
    # note: will be aggregated to coarser pixels if the area is too large for LISFLOOD to handle 
## river (polygon shapefile): polygon of river, with a field for width
## lulc (raster): raster LULC file
## parname (string): name of the .par file
## bciname (string): name of the .bci file

## @return
## par (data.frame): contents of the .par file to be written out
## writes raster files out to lisflood folder

write_lisflood_rasters <- function(fileloc, dem, river, lulc, parname, bciname) {
  ## start .par file
  par <- data.frame(matrix(ncol = 2))
  par[2,] <- c('dirroot', 'results')  #folder to store simulation results
  par[3,] <- c('sim_time', 10*24*3600)  #length of simulation, in seconds
  par[4,] <- c('initial_tstep', 1)
  par[5,] <- c('massint', 3600)  #save interval for mass balance eq
  par[6,] <- c('saveint', 24*3600)  #save interval for inundation rasters
  
  ## check resolution of elevation raster
  if (prod(res(dem)) > 1e6) {
    dem <- raster::aggregate(dem, fact = ceiling(prod(res(dem))/1e6))
  }
  ## write elevation raster to file
  writeRaster(dem, format = 'ascii',
              filename = paste0(fileloc, 'russian.dem.asc'),
              overwrite = TRUE)
  par[8,] <- c('DEMfile', 'russian.dem.asc')
  
  ## load river width and write to file
  width <- rasterize(x = river %>% st_cast(to = 'MULTILINESTRING'), y = dem, field = 'WIDTH')
  proj4string(width) <- proj4string(dem)
  writeRaster(width, format = 'ascii',
              filename = paste0(fileloc, 'russian.width.asc'),
              overwrite = TRUE)
  par[10,] <- c('SGCwidth', 'russian.width.asc')
  par[11,] <- c('SGCn', 0.05)
  par[12,] <- c('SGCbank', 'russian.dem.asc')
  
  ## write boundary edges to .bci file
  width.df <- as.data.frame(width, xy = TRUE) %>% subset(!is.na(layer))
  edge.in <- width.df[which(width.df$y == max(width.df$y)),]
  edge.out <- width.df[which(width.df$x == min(width.df$x)),]
  par[14,] <- c('bcifile', paste0(bciname, '.bci'))
  par[16,] <- c('startfile', 'russian.start.wd')
  
  ## roughness coefficient
  lulc <- resample(projectRaster(lulc, dem), dem)
  writeRaster(lulc, format = 'ascii',
              filename = paste0(fileloc, 'russian.n.asc'),
              overwrite = TRUE)
  par[18,] <- c('fpfric', 0.06)
  par[19,] <- c('manningfile', 'russian.n.asc')

  ## other parameters for .par file
  par[21:23, 1] <- c('acceleration', 'elevoff', 'debug')
  
  return(list(par, edge.in, edge.out))
}




## @param
## fileloc (string): location of lisflood.exe (has to end with a slash)
## parname (string): name of the .par file
## runoff (data.frame): list of synthetic runoff events
## drainarea (double): drainage area of the basin, in square meters
## tp (double): time from the end of precipitation to the peak discharge, in seconds
## m (double): shape parameter for hydrograph 
## baseflow (double): normal river baseflow, in m3/s
## simlength (double): length of LISFLOOD simulation, in seconds
## plot (logical): print plot to console? 

## @return
## inundation (raster stack): stack of synthetic inundation extents

generate_inundation <- function(fileloc, dem, river, lulc, parname, bciname, runoff, drainarea, 
                                tp = 48*3600, m = 4, baseflow = 2, simlength = 86400*10, plot = TRUE) {
  ## convert runoff to discharge
  runoff <- runoff %>% mutate(discharge_m3s = runoff_mm/1e3 * drainarea / duration / 3600)
  
  ## set up LISFLOOD simulation
  raster.list <- write_lisflood_rasters(fileloc, dem, river, lulc, parname, bciname)
  par <- raster.list[[1]]; edge.in <- raster.list[[2]]; edge.out <- raster.list[[3]]
  cmd <- paste0('cd ', fileloc, ' & lisflood ', parname, '.par')
  
  ## define write_lisflood_files function
  write_lisflood_files <- function(i, par, fileloc, parname, bciname, runoff, tp, m, baseflow, simlength) {
    ## set up hydrograph variables
    t <- seq(0, simlength, 360)
    baseflow <- baseflow / 100  #m2/s
    
    ## create hydrograph
    Qp <- runoff[i,'discharge_m3s'] / 100  #m2/s
    q <- apply(cbind(exp(m*(1-t/tp)) * (t/tp)^m * Qp, rep(baseflow, length(t))), 1, max)
    
    ## write .bdy file
    bdy <- matrix(c('LISFLOOD', NA, paste0('flow', i), NA, length(t), 'seconds'), 
                  byrow = TRUE, ncol = 2)
    bdy <- rbind(bdy, cbind(q, t))
    write.table(bdy, file = paste0(fileloc, 'flow', i, '.bdy'), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
    
    ## edit .par and .bci file
    par[1,] <- c('resroot', paste0('test', i))  #prefix name on all results files
    par[15,] <- c('bdyfile', paste0('flow', i, '.bdy'))
    write.table(par, paste0(fileloc, parname, '.par'),
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t', na = '')
    
    bci <- data.frame(matrix(c('N', min(edge.in$x), max(edge.in$x), 'QVAR', paste0('flow', i),
                               'W', min(edge.out$y), max(edge.out$y), 'FREE', NA),
                             nrow = 2, byrow = TRUE))
    write.table(bci, file = paste0(fileloc, bciname, '.bci'), 
                row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t')
    
  }

  pb <- txtProgressBar(min = 0, max = nrow(runoff), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  inundation <- 
    foreach (i = 1:nrow(runoff),
             .packages = 'raster',
             .options.snow = opts) %dopar% {
      ## define input files
      write_lisflood_files(i, par, fileloc, parname, bciname, runoff, tp, m, baseflow, simlength)
      
      ## run LISFLOOD
      setwd(fileloc)
      shell(shQuote(cmd, type = 'cmd'))
  
      ## save results to raster brick
      raster(paste0(fileloc, 'results/test', i, '.max'))
    }

  inundation <- do.call('stack', inundation)
  crs(inundation) <- crs(dem)
  return(inundation)
}




## @param
## inundation (raster stack): stack of synthetic inundation extents
## runoff (data.frame): list of synthetic runoff events
## river (polygon shapefile): polygon of river, with a field for width
## struct (shapefile): building locations within study area
## housing (shapefile): number of single family homes per census tract within study area
## buffer (double): buffer around river where no homes are allowed, in meters
## grid.size (double): resolution of building density grid, in degrees lat/lon
## n.depth (integer): number of flood depths to generate per runoff event

## @return
## depth (data.frame): synthetic flood depths by building
  # attributes: simulation = tracker list of n.AR, n.precip, & n.runoff
  #             censustract = CT identifier for each building

generate_flood_depth <- function(inundation, runoff, river, struct, housing, 
                                 buffer = 50, grid.size = 0.02, n.depth = 1) {
  print('defining input information...')
  ## set up depth tracker dataframe
  if (sum(c('n.AR', 'n.precip', 'n.runoff') %in% names(runoff)) == 3) {
    n.AR <- max(runoff$n.AR)
    n.precip <- max(runoff$n.precip)
    n.runoff <- max(runoff$n.runoff)
    depth.tracker <- expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.runoff = 1:n.runoff)
    depth.tracker$lisflood.id <- 1:nrow(depth.tracker)
  } else {
    depth.tracker <- data.frame(lisflood.id = 1:nlayers(inundation))
  }
  depth.tracker <- expand.grid(lisflood.id = depth.tracker$lisflood.id, n.depth = 1:n.depth) %>% 
    full_join(depth.tracker, by = 'lisflood.id')

  ## import census tracts
  california <- counties(state = 'CA', class = 'sf')
  CT <- tracts(state = 'CA', county = 'sonoma', class = 'sf') %>% 
    left_join(data.frame(COUNTYFP = california$COUNTYFP, COUNTYNAME = california$NAME), by = 'COUNTYFP') %>% 
    mutate(COUNTYID = toNumber(STATEFP)*1e3 + toNumber(COUNTYFP), GEOID = toNumber(GEOID))

  ## buffer river
  rr_buffer <- river %>% 
    st_transform(albers) %>% 
    st_union %>% 
    st_buffer(buffer) %>% 
    st_transform(st_crs(housing))
  
  ## define split function
  nsplit <- function(x, n) {
    p <- x/sum(x)
    diff(round(n*cumsum(c(0,p))))
  }
  
  ## match structures to census tracts
  locations <- struct %>% st_intersection(housing)
  
  ## distribute SFH based on building density
  print('distributing SFH...')
  pb <- txtProgressBar(min = 0, max = n.depth, style = 3)
  buildings <- 
    foreach (k = 1:n.depth,
             .packages = c('dplyr', 'sf', 'foreach'),
             .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
      foreach (i = 1:nrow(housing),
               .combine = 'rbind',
               .packages = c('dplyr', 'sf')) %do% {
        ## find the number of buildings within the polygon
        locations.subset <- locations %>% subset(GEOID == housing$GEOID[i])
        ## generate SFH locations
        n <- housing$HousingUnits[i]
        if(n > 0) {
          ## randomly subset to SFH
          index <- sample(1:nrow(locations.subset), size = n, replace = TRUE)
          ## assign latlongs to buildings
          cbind(st_coordinates(locations.subset[index,]), GEOID = housing$GEOID[i]) %>% 
            data.frame %>% 
            st_as_sf(coords = c('X', 'Y'), crs = st_crs(CT)) %>% 
            mutate(id = 1:nrow(.))
        }
      }
    }

  ## extract flood depths at SFH locations
  print('extracting flood depths...')
  
  pb <- txtProgressBar(min = 0, max = n.depth*nlayers(inundation), style = 3)
  depth <- 
    foreach (k = 1:n.depth,
             .combine = 'rbind',
             .packages = c('dplyr', 'sf', 'raster', 'velox'), 
             .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %:% 
      foreach (j = 1:nlayers(inundation), 
               .combine = 'rbind') %dopar% {
        flood <- inundation[[j]]
        buildings[[k]] %>% 
          st_transform(proj4string(flood)) %>%
          velox(flood)$extract_points(.) %>% c
        # depthval[is.na(depthval)] <- 0
      } 

  ## send back to function
  print('returning calculated values...')
  depth_save <- depth
  attributes(depth)$simulation <- depth.tracker
  attributes(depth)$censustract <- buildings[[1]]$GEOID
  depth_save <<- depth_save
  
  keep <- which(apply(depth, 2, function(x) sum(is.na(x)) != length(x)))
  depth <- depth[,keep]
  attributes(depth)$simulation <- depth.tracker
  attributes(depth)$censustract <- buildings[[1]]$GEOID[keep]
  return(depth)
  
  ## possible plots: 
  # could have user insert a sample realization number & output a map of buildings with depth values
  # also could have a chart of average flood depth by census tract
  
}




## @param
## inundation (raster stack): stack of synthetic inundation extents
## aoi (sf): polygon bounding region of interest

## @return
## prints plot to console

plot_inundation <- function(inundation, aoi) {
  sonoma <- tigris::tracts(state = 'CA', county = 'Sonoma', class = 'sf')
  g <- ggplot() + 
    geom_sf(data = sonoma %>% st_intersection(aoi) %>% st_transform(proj4string(inundation[[1]])), 
            fill = NA, color = 'grey75') + 
    geom_sf(data = aoi %>% st_transform(proj4string(inundation[[1]])), color = 'black', alpha = 0.5) + 
    ggtitle('Probabilistic Inundation Map') + 
    theme_void() + theme(plot.title = element_text(hjust = 0.5))
  
  for (i in 1:nlayers(inundation)) {
    g <- g + geom_raster(data = inundation[[i]] %>% 
                           as.data.frame(xy = TRUE) %>% 
                           setNames(c('x','y','layer')) %>% 
                           subset(layer > 0), 
                         aes(x = x, y = y), fill = 'blue', alpha = 1.5/nlayers(inundation))
  }
  print(g)
}
