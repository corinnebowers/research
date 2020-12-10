
## this is just a test to see if I can get something to run

# data <- matrix(rnorm(1e4), nrow = 1e2)
# hist(apply(data, 1, max))


## version 2

library(dplyr)
library(ggplot2)

data <- rnorm(4e4) %>% matrix(nrow = 2e2)
ggplot(data = data.frame(rowmax = apply(data, 1, max))) +
  geom_histogram(aes(x = rowmax), bins = sqrt(nrow(data)))
ggsave('./sherlock_test_plot.jpg', width = 5, height = 5)
print('all done!')