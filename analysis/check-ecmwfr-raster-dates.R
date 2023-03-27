library('raster')
library('dplyr')
library('purrr')
library('furrr')
library('progressr')

d <- tibble(filename = list.files('data/ecmwfr-data/', pattern = '.nc',
                                  full.names = TRUE))

plan(multisession, workers = availableCores(logical = FALSE))
check <- 
  \(.fn, .ts) {
    .ts <-
      as.POSIXct(.ts) %>%
      round(units = 'hours') %>%
      format(format = 'data/ecmwfr-data/%Y-%m-%d-%H-data.nc')
    return(.ts == .fn)
  }

d <- mutate(
  d,
  timestamp = future_map_chr(filename,
                             \(.fn) raster(x = .fn, varname = 't2m')@z[[1]]))

d <- mutate(d, matches = future_map2_lgl(filename, timestamp, check))

beepr::beep()

# check which file names do not match the timestamps
filter(d, !matches)

# check if all data points have a raster
all_times <-
  readRDS('data/tracking-data/full-dataset.rds') %>%
  pull(tel) %>%
  purrr::map(\(.t) .t$timestamp %>%
               lubridate::as_datetime(tz = 'America/Los_Angeles') %>%
               round(units = 'hours') %>%
               unique() %>% # uniques within telemetries
               lubridate::as_datetime(tz = 'UTC') %>%
               format(format = 'data/ecmwfr-data/%Y-%m-%d-%H-data.nc') %>%
               as.character()) %>%
  unlist() %>%
  unique() %>% # uniques between telemetries
  sort()

exists <- file.exists(all_times)

missing_times <- all_times[! exists]
missing_times
beepr::beep(2)

#' *all midnight rasters failed to download?*
times <- stringr::str_replace(missing_times, pattern = '00-data.nc',
                              replacement = '24-data.nc')

# check if all files exist
missing_times <-
  readRDS('models/movement-models.rds') %>%
  slice(1) %>%
  mutate(annotated = future_imap(tel, \(.tel, i) { # add speeds and weather
    cat('Working on animal ', i, ', species: ', species[i], '\n', sep = '')
    
    # check existence of files
    weather_data <-
      data.frame(.tel) %>% # telemetry as data frame
      mutate(nearest_hour = round(timestamp, units = 'hours') %>%
               format(format = '%Y-%m-%d-%H'),
             # check if raster exists
             missing = imap_lgl(
               nearest_hour,
               \(fn, ..i) {
                 paste0('data/ecmwfr-data/', nearest_hour[..i], '-data.nc') %>%
                   file.exists()
               }))
  })) %>%
  select(animal, species, dataset_name, annotated) %>%
  tidyr::unnest(annotated) %>%
  filter(missing) %>%
  pull(nearest_hour) %>%
  as.POSIXct(format = '%Y-%m-%d-%H')
