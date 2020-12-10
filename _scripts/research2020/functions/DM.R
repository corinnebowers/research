
## @param
## depth (data.frame): synthetic flood depths by building
  # attributes: simulation = tracker list of n.AR, n.precip, & n.runoff
  #             censustract = CT identifier for each building
## curve (keyword): "hazus" uses the HAZUS-MH depth-damage functions
##                  "flemo" uses the FLEMOps depth-damage functions
##                  "beta" uses the beta distribution curves from Wing et al. (2020)
##                  "average" takes an average of all three
## n.damage (integer): number of flood damages to generate per inundation event

## @return
## damage.prob (list): synthetic damages by building
  # attributes: simulation = tracker list of n.AR, n.precip, & n.runoff
  #             censustract = CT identifier for each building

## note: running the probabilistic version makes no difference if curve = "hazus" or "flemo"

generate_flood_damage <- function(depth, curve = 'average', n.damage = 1) {
  load('D:/Research/_data/depthdamage/depthdamage.Rdata')
  generate_single_flood_damage <- function(depth, curve) {
    damage <- matrix(nrow = nrow(depth), ncol = ncol(depth))
    
    interp <- function(x, y, xi) {
      n <- length(x)
      xx <- c(x[1], (x[2:n] + x[1:(n - 1)])/2, x[n])
      yy <- c(y, y[n])
      yi <- approx(xx, yy, xi, method = "constant")$y
      return(yi)
    }
    
    depthval <- unname(unlist(depth))
    if (curve == 'hazus') {
      damage[] <- interp(hazus$ft, hazus$x1, depthval)
      damage[depthval == 0] <- 0
      
    } else if (curve == 'flemo') {
      damage[] <- interp(flemo$ft, flemo$PQ_SFH, depthval)
      damage[depthval == 0] <- 0
      
    } else if (curve == 'beta') {
      depthval_clean <- depthval[!is.na(depthval) & round(depthval) > 0]
      depthval_clean <- ifelse(depthval_clean > 50, 50, depthval_clean)
      damage[!is.na(depthval) & round(depthval) > 0] <- 
        rbeta(n = length(depthval_clean), 
              shape1 = beta.dist[match(round(depthval_clean), beta.dist$water_ft), 'alpha'],
              shape2 = beta.dist[match(round(depthval_clean), beta.dist$water_ft), 'beta'])
      damage[round(depthval) == 0] <- 0
      
    } else if (curve == 'average') {
      damage_hazus <- damage
      damage_hazus[] <- interp(hazus$ft, hazus$x1, depthval)/100
      damage_hazus[depthval == 0] <- 0
      
      damage_flemo <- damage
      damage_flemo[] <- interp(flemo$ft, flemo$PQ_SFH, depthval)/100
      damage_flemo[depthval == 0] <- 0
      
      depthval_clean <- depthval[!is.na(depthval) & round(depthval) > 0]
      depthval_clean <- ifelse(depthval_clean > 50, 50, depthval_clean)
      damage_beta <- damage
      damage_beta[!is.na(depthval) & round(depthval) > 0] <- 
        rbeta(n = length(depthval_clean), 
              shape1 = beta.dist[match(round(depthval_clean), beta.dist$water_ft), 'alpha'],
              shape2 = beta.dist[match(round(depthval_clean), beta.dist$water_ft), 'beta'])
      damage_beta[round(depthval) == 0] <- 0
      
      damage <- cbind(c(damage_hazus), c(damage_flemo), c(damage_beta)) %>% 
        rowMeans %>% matrix(nrow = nrow(damage), ncol = ncol(damage))

    } else {
      stop('Not a valid keyword for "curve".')
    }
    
    damage <- data.frame(damage)
    return(damage)
  }
  
  pb <- txtProgressBar(min = 0, max = n.damage, style = 3)
  damage.prob <- 
    foreach (i = 1:n.damage, 
             .packages = c('dplyr', 'abind'),
             .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
      generate_single_flood_damage(depth, curve)
    }

  attributes(damage.prob)$simulation <- attributes(depth)$simulation
  attributes(damage.prob)$censustract <- attributes(depth)$censustract
  return(damage.prob)
}
