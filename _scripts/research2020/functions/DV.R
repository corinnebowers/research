
## @param
## damage.prob (list): synthetic damages by building
  # attributes: simulation = tracker list of n.AR, n.precip, & n.runoff
  #             censustract = CT identifier for each building
## value_mean (double): mean of single family home values
## value_sd (double): standard deviation of single family home values
## n.loss (integer): number of loss estimates to generate per damage event

## @return
## loss.prob (data.frame): losses by census tract for each synthetic event

generate_loss_estimates <- function(damage.prob, value_mean, value_sd = 0, n.loss = 1) {
  generate_single_loss_estimates <- function(damage.prob, value_mean, value_sd) {
    n.damage <- length(damage.prob)
    loss <- 
      foreach (i = 1:n.damage, 
               .combine = 'rbind', 
               .packages = 'dplyr') %do% {
        print(i)
        ## find losses by building 
        damage <- damage.prob[[i]]
        value <- rnorm(n = prod(dim(damage)), mean = value_mean, sd = value_sd) %>% 
          matrix(nrow = nrow(damage), ncol = ncol(damage))
        loss_bybuilding <- damage * value
        keep <- which(apply(loss_bybuilding, 1, function(x) sum(x) != 0))
        loss_bybuilding <- loss_bybuilding[keep,]
        
        ## aggregate losses to census tract level
        temp <- data.frame(cbind(GEOID = attr(damage.prob, 'censustract'), t(loss_bybuilding))) %>% 
          group_by(GEOID) %>% 
          summarize_all(sum)
        temp %>% 
          select(-GEOID) %>% t %>% 
          cbind(attr(damage.prob, 'simulation')[keep,]) %>% 
          setNames(c(paste0('CT', temp$GEOID), names(attr(damage.prob, 'simulation')))) %>% 
          mutate(n.damage = i)
      }
    return(loss)
  }
  
  pb <- txtProgressBar(min = 0, max = n.loss, style = 3)
  loss.prob <- 
    foreach (i = 1:n.loss, 
             .combine = 'rbind', 
             .packages = c('dplyr', 'foreach'),
             .options.snow = list(progress = function(n) setTxtProgressBar(pb, n))) %dopar% {
      generate_single_loss_estimates(damage.prob, value_mean, value_sd) %>% 
        mutate(n.loss = i)
    }
  index <- names(loss.prob) %in% c(names(attr(damage.prob, 'simulation')), 'n.damage', 'n.loss')
  loss.prob <- loss.prob[,c(which(index), which(!index))]
  return(loss.prob)
}




## @param
## loss (data.frame): losses by census tract for each synthetic event

## @return
## print plots to console

plot_loss_distribution <- function(loss) {
  ## plot histogram of total loss estimates
  loss.dist <- cbind(loss[,1:6], total_loss = apply(loss[,-(1:6)], 1, sum))
  g.dist <- ggplot(loss.dist) + 
    geom_histogram(aes(x = total_loss/1e6), color = 'black', fill = 'white', bins = sqrt(nrow(loss.dist))) + 
    ggtitle('Distribution of Total Loss') + 
    labs(x = 'Total Loss ($millions)', y = 'Frequency of Occurrence') + 
    theme_classic()
  print(g.dist)
  
  ## plot spatial distribution of loss
  sonoma <- tigris::tracts(state = 'CA', county = 'Sonoma', class = 'sf')
  sonoma$GEOID <- toNumber(sonoma$GEOID)
  loss.sf <- loss %>% 
    melt(id.vars = c('n.AR', 'n.precip', 'n.runoff', 'n.depth', 'n.damage', 'n.loss'), 
         variable.name = 'GEOID', value.name = 'Loss') %>% 
    mutate(GEOID = toNumber(gsub('CT', '', GEOID))) %>% 
    group_by(GEOID) %>% 
    summarize(mean_loss = mean(Loss)) %>% 
    full_join(sonoma, ., by = 'GEOID')
  g.map <- ggplot() + 
    geom_sf(data = sonoma, fill = NA, color = 'grey70') +
    geom_sf(data = loss.sf, aes(fill = mean_loss)) + 
    scale_fill_viridis_c(direction = -1, na.value = NA) + 
    theme_void()
  print(g.map)
  
  ## print overall mean loss
  loss.mean <- Mean(loss.dist$total_loss)
  print(loss.mean)
}
