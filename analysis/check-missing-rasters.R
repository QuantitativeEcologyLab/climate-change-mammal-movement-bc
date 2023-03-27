library(dplyr)
library(ctmm)

d <- readRDS('data/tracking-data/full-dataset.rds')

times <- d %>%
  pull(tel) %>%
  purrr::map(\(.t) .t$timestamp %>%
               as.POSIXct() %>%
               round(units = 'hours') %>%
               unique() %>%
               as.character()) %>%
  unlist() %>%
  unique() %>%
  sort() %>%
  lubridate::as_datetime() %>%
  unique() %>%
  format(format = '%Y-%m-%d-%H') %>%
  paste0('data/ecmwfr-data/', ., '-data.nc')

downloaded <- file.exists(times)

times[which(! downloaded)]
