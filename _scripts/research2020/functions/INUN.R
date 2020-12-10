
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
  
  # ## troubleshooting
  # par[8,] <- c('DEMfile', 'flood_valley.dem.asc')
  # par[10,] <- c('SGCwidth', 'flood_valley_SGC.width.asc')
  # par[11,] <- c('SGCn', 0.035)
  # par[12,] <- c('SGCbank', 'flood_valley.dem.asc')
  # par[13,] <- c('SGCbed', 'flood_valley_SGC.bed.asc')
  # par[14,] <- c('bcifile', 'flood_valley_SGC.bci')
  # par[16,] <- c('startfile', 'flood_valley_SGC.depth.asc')
  # par[18,] <- c('fpfric', 0.06)
  
  ## other parameters for .par file
  par[21:23, 1] <- c('acceleration', 'elevoff', 'debug')
  
  edge.in <<- edge.in
  edge.out <<- edge.out
  return(par)
}




## @param
## i (counter): iteration number
## par (data.frame): contents of the .par file to be written out
## fileloc (string): location of lisflood.exe (has to end with a slash)
## parname (string): name of the .par file
## bciname (string): name of the .bci file
## runoff (data.frame): list of synthetic runoff events
## tp (double): time from the end of precipitation to the peak discharge, in seconds
## m (double): shape parameter for hydrograph 
## baseflow (double): normal river baseflow, in m3/s
## simlength (double): length of LISFLOOD simulation, in seconds

## @return
## writes text files out to lisflood folder

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
  # bci <- data.frame(matrix(c('E', paste('200000'), 197650, 'QVAR', paste0('flow', i),
  #                            'P', 422972, 198081, 'QFIX', 1), 
  #                          nrow = 2, byrow = TRUE))
  write.table(bci, file = paste0(fileloc, bciname, '.bci'), 
              row.names = FALSE, col.names = FALSE, quote = FALSE, sep = '\t')
  
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
  runoff <- runoff %>% 
    mutate(discharge_m3s = runoff_mm/1e3 * drainarea / duration / 3600)
  
  ## set up LISFLOOD simulation
  par <- write_lisflood_rasters(fileloc, dem, river, lulc, parname, bciname)
  cmd <- paste0('cd ', fileloc, ' & lisflood ', parname, '.par')
  setwd(fileloc)
  inundation <- stack()
  flood <- list()
  
  for (i in 1:nrow(runoff)) {
    ## define input files
    write_lisflood_files(i, par, fileloc, parname, bciname, runoff, tp, m, baseflow, simlength)
    
    ## run LISFLOOD
    setwd(fileloc)
    shell(shQuote(cmd, type = 'cmd'))

    ## save results to raster brick
    lisflood <- raster(paste0(fileloc, 'results/test', i, '.max'))
    flood[[i]] <- plot(lisflood)
    # lisflood <- raster(paste0(fileloc, 'results/test.max')); crs(lisflood) <- crs(dem)
    
    inundation <- stack(inundation, lisflood)
  }
  # if (plot) { plot_inundation(inundation, aoi) }

  # crs(inundation) <- crs(dem)
  return(inundation)
}




## @param
## river (polygon shapefile): polygon of river, with a field for width
## struct (point shapefile): building locations within study area
## housing (shapefile): number of single family homes per census tract within study area
## buffer (double): buffer around river where no homes are allowed, in meters
## grid.size (double): resolution of building density grid, in degrees lat/lon

## @return
## coords.df (shapefile): individual houses with latlong locations

generate_building_locations <- function(river, struct, housing, buffer, grid.size) {
  ## import census tracts
  CT <- tracts(state = 'CA', county = 'sonoma', class = 'sf')
  CT <- merge(CT, data.frame(COUNTYFP = california$COUNTYFP, COUNTYNAME = california$NAME),
              by = 'COUNTYFP', all.x = TRUE)
  CT$COUNTYID <- toNumber(CT$STATEFP)*1e3 + toNumber(CT$COUNTYFP)
  CT$GEOID <- toNumber(CT$GEOID)
  
  rr_buffer <- river %>% 
    st_transform(albers) %>% 
    st_union %>% 
    st_buffer(buffer) %>% 
    st_transform(st_crs(housing))
  nsplit <- function(x, n) {
    p <- x/sum(x)
    diff(round(n*cumsum(c(0,p))))
  }
  
  ## generate building coordinates
  # pb <- txtProgressBar(min = 0, max = nrow(housing), style = 3)
  coords <- list()
  # for (i in 1:nrow(housing)) {
  #   ## identify a polygon
  #   n <- housing$HousingUnits[i]
  #   
  #   ## split that polygon into little pieces
  #   pieces <- suppressWarnings(suppressMessages(housing[i,] %>% 
  #     st_make_grid(offset = round(st_bbox(housing[i,])[c('xmin','ymin')]/2, 3)*2 - grid.size/2,
  #                  cellsize = grid.size, what = 'polygons') %>% st_sf %>% 
  #     mutate(pieces_id = 1:nrow(.)) %>% 
  #     st_difference(rr_buffer)))
  #   
  #   ## find the distribution of buildings within the polygon
  #   buildings <- suppressWarnings(suppressMessages(struct %>% 
  #     st_intersection(housing[i,]) %>%
  #     st_intersection(pieces) %>% 
  #     st_drop_geometry %>% 
  #     group_by(pieces_id) %>% 
  #     summarize(num_struct = length(pieces_id)) %>% 
  #     mutate(num_SFH = nsplit(num_struct, n)) %>% 
  #     subset(num_SFH > 0)))
  #   
  #   ## assign latlongs to buildings
  #   coords[[i]] <- data.frame(X = NA, Y = NA)[-1,]
  #   if (nrow(buildings) > 0) {
  #     for (j in 1:nrow(buildings)) {
  #       coords[[i]] <- rbind(coords[[i]], 
  #                            st_sample(st_transform(pieces[pieces$pieces_id == buildings$pieces_id[j],], albers),
  #                                      size = buildings$num_SFH[j], type = 'random') %>% 
  #                              st_transform(st_crs(CT)) %>% st_coordinates)
  #     }
  #     coords[[i]]['id'] <- housing$GEOID[i]
  #   }
  #   setTxtProgressBar(pb, i)
  # }
  
  ## subset buildings 
  for (i in 1:nrow(housing)) {
    ## find the number of buildings within the polygon
    buildings <- suppressWarnings(suppressMessages(struct %>% st_intersection(housing[i,])))
    
    ## subset to SFH
    n <- housing$HousingUnits[i]
    index <- sample(1:nrow(buildings), size = n, replace = TRUE)
    
    ## assign latlongs to buildings
    if(length(index) > 0) {
      coords[[i]] <- data.frame(cbind(st_coordinates(buildings[index,]), 
                                      id = rep(housing$GEOID[i], length(index))))
    }
    # setTxtProgressBar(pb, i)
  }
  
  ## assign housing ids
  coords.df <- coords %>%
    lapply(t) %>%
    unlist %>%
    matrix(ncol = 3, byrow = TRUE) %>%
    as.data.frame %>%
    st_as_sf(coords = c('V1', 'V2'), crs = st_crs(CT))
  coords.df <- coords.df[order(coords.df$V3),] %>%
    mutate(GEOID = V3, id = 1:nrow(.)) %>%
    select(-V3)
  coords %>% lapply(names)
  return(coords.df)
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
  
  ## find n.AR, n.precip, & n.runoff
  n.AR <- max(runoff$n.AR)
  n.precip <- max(runoff$n.precip)
  n.runoff <- max(runoff$n.runoff)
  
  depth.tracker <- expand.grid(n.AR = 1:n.AR, n.precip = 1:n.precip, n.runoff = 1:n.runoff)
  depth.tracker$lisflood.id <- 1:nrow(depth.tracker)
  depth.tracker <- expand.grid(lisflood.id = depth.tracker$lisflood.id, n.depth = 1:n.depth) %>% 
    full_join(depth.tracker, by = 'lisflood.id')
  
  depth <- data.frame(cbind(lisflood.id = depth.tracker$lisflood.id, 
                            n.depth = depth.tracker$n.depth, 
                            matrix(nrow = nrow(depth.tracker), ncol = sum(housing$HousingUnits))))
  
  buildings <- list()
  pb <- txtProgressBar(min = 0, max = length(depth.tracker), style = 3)
  for (j in 1:n.depth) {
    ## distribute SFH based on building density
    buildings[[j]] <- generate_building_locations(river, struct, housing, buffer, grid.size)
    
    ## find flood depth at each building
    for (i in 1:nlayers(inundation)) {
      flood <- inundation[[i]]
      depthval <- buildings[[j]] %>% 
        st_transform(proj4string(flood)) %>% 
        as('Spatial') %>% 
        extract(flood, .)
      depthval[is.na(depthval)] <- 0
      depth[(depth$lisflood.id == i & depth$n.depth == j), -(1:2)] <- depthval
    }
    setTxtProgressBar(pb, (j-1)*nlayers(inundation) + i)
  }
  
  attributes(depth)$simulation <- depth.tracker
  attributes(depth)$censustract <- buildings[[1]]$GEOID
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
