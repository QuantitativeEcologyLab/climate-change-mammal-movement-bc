library('dplyr') # for data wrangling (mutate(), %>%, etc.)
library('tidyr') # for data wrangling (unnest(), pivot_*, etc.)
library('purrr') # for functional programming (map_***(), etc.)
library('ctmm')  # for movement models

d <- readRDS('data/tracking-data/full-dataset.rds') %>%
  mutate(variogram = map(tel, \(.tel) ctmm.guess(data = .tel, # initial guess
                                                 interactive = FALSE)),
         model = map2(tel, variogram, # fit movement model
                      \(.tel, .var) ctmm.select(data = .tel, CTMM = .var,
                                                trace = 1)))
# fit utilization distribution with weights optimized based on autocorrelation 
d$ud <- list(NULL)
for(i in 1:nrow(d)) {
  cat('Fitting AKDE ', i, ' of ', nrow(d), ' (', d$species[i], ') at ',
      as.character(Sys.time()), '...\n', sep = '')
  d$ud[i] <- akde(data = d$tel[[i]], CTMM = d$model[[i]],
                  weights = TRUE) %>% list()
}

saveRDS(d, 'models/movement-models.rds')
