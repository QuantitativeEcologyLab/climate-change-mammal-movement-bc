library('sp')      # for working with spatial data
library('raster')  # for working with raster data
library('ncdf4')   # for nc datasets
library('dplyr')   # for data wrangling (e.g., mutate, %>%, ...)
library('purrr')   # for functional programming (e.g., map_***())
library('furrr')   # for parallel functional programming (e.g., future_map())
library('ggplot2') # for fancy plots
library('ctmm')    # for movement modeling
source('functions/detrended_speeds.R') # to remove baseline noise in speed

theme_set(theme_bw())
K <- 1e-6 # scaling constant for weights

# add weather data and speeds to the telemetry data ----
# see: https://confluence.ecmwf.int/display/CKB/ERA5-Land%3A+data+documentation

# if(file.exists('data/standardized-speeds.rds')) {
if(FALSE) {
  d <- readRDS('data/standardized-speeds.rds')
} else {
  #' to parallelize using `furrr`
  plan(multisession, workers = availableCores(logical = FALSE))
  d <-
    readRDS('models/movement-models.rds') %>%
    mutate(annotated = future_imap(tel, \(.tel, i) { # add speeds and weather
      cat('Working on animal ', i, ', species: ', species[i], '\n', sep = '')
      
      # telemetry as unprojected spatial points object
      .tel_sp <- spTransform(SpatialPoints.telemetry(.tel), '+proj=longlat')
      
      # estimate speeds
      speeds_data <-
        detrend.speeds(DATA = .tel, CTMM = model[[i]]) %>%
        transmute(speed_low = low, speed_est = est, speed_high = high)
      
      # add weather data
      weather_data <-
        data.frame(.tel) %>% # telemetry as data frame
        #' *note*: `map()` cannot accept rasters as input
        mutate(
          nearest_hour = timestamp %>%
            lubridate::as_datetime() %>%
            round(units = 'hours') %>%
            format(format = '%Y-%m-%d-%H'),
          file_name = paste0('data/ecmwfr-data/', nearest_hour, '-data.nc'),
          # temperature (currently degrees Kelvin)
          temp_c = imap_dbl(
            file_name,
            \(fn, ..i) {
              if(file.exists(fn)) {
                fn %>%
                  raster(varname = 't2m') %>%
                  raster::extract(.tel_sp[..i])
              } else {
                NA_real_
              }
            }),
          # hourly precipitation (currently m)
          tp_mm = imap_dbl(
            file_name,
            \(fn, ..i) {
              if(file.exists(fn)) {
                fn %>%
                  raster(varname = 'tp') %>%
                  extract(.tel_sp[..i])
              } else {
                NA_real_
              }
            }),
          # snow depth (currently m)
          sde_mm = imap_dbl(
            file_name,
            \(fn, ..i) {
              if(file.exists(fn)) {
                fn %>%
                  raster(varname = 'sde') %>%
                  extract(.tel_sp[..i])
              } else {
                NA_real_
              }
            })
        ) %>%
        # only keep relevant columns
        select(c(timestamp, longitude, latitude, temp_c, tp_mm, sde_mm)) %>%
        mutate(temp_c = temp_c - 273.15, # convert from Kelvin to Celsius
               tp_mm = tp_mm * 1e3, # convert from m to mm
               sde_mm = sde_mm * 1e3) # convert from m to mm
      
      # return weather and speed data
      return(bind_cols(weather_data, speeds_data))
    })) %>%
    select(animal, species, dataset_name, annotated) %>%
    tidyr::unnest(annotated)
  
  plan(sequential)
  
  # check how many weather values are NA
  #' *6 rasters failed to download repeatedly; likely missing from the server*
  filter(d, is.na(temp_c) | is.na(tp_mm) | is.na(sde_mm))
  
  # check negatives in precip (should be near zero)
  range(d$tp_mm, na.rm = TRUE)
  sum(d$tp_mm < 0, na.rm = TRUE)
  mean(d$tp_mm < 0, na.rm = TRUE)
  
  # convert negatives in precip and snow depth to zero
  d <- mutate(d,
              tp_mm = if_else(tp_mm < 0, 0, tp_mm),
              sde_mm = if_else(sde_mm < 0, 0, sde_mm))
  
  # add AKDE weights
  d0 <- readRDS('models/movement-models.rds') %>%
    mutate(tel = map2(ud, tel, \(.ud, .tel) {
      .tel %>%
        data.frame() %>%
        mutate(weight = K * # scaling constant
                 unique(.ud$DOF.H) * # effective sample size
                 .ud$weights) %>% # 1 / nrow(tel)
        select(timestamp, weight)
    })) %>%
    select(animal, tel) %>%
    tidyr::unnest(tel)
  
  d <- left_join(d, d0, by = c('animal', 'timestamp'))
  
  saveRDS(d, 'data/standardized-speeds.rds')
}

if(FALSE) {
  d <- readRDS('data/standardized-speeds.rds')
  # check for correlation between relationships
  ggplot(d, aes(temp_c, tp_mm)) +
    geom_hex() +
    geom_smooth(method = 'gam', formula = y ~ s(x), color = 'red3') +
    labs(x = paste0('Temperature (\U00B0', 'C)'),
         y = 'Hourly precipitation (mm)')
  
  ggplot(d, aes(temp_c, sde_mm)) +
    geom_hex() +
    geom_smooth(method = 'gam', formula = y ~ s(x), color = 'red3') +
    labs(x = paste0('Temperature (\U00B0', 'C)'),
         y = 'Snow depth (mm)',
         method = 'gam', formula = y ~ s(x))
  
  ggplot(d, aes(tp_mm, sde_mm)) +
    geom_hex() +
    geom_smooth(method = 'gam', formula = y ~ s(x), color = 'red3') +
    labs(x = 'Precipitation (mm)', y = 'Snow depth (mm)',
         method = 'gam', formula = y ~ s(x))
  
  # check relationships with speed estimates
  ggplot(d, aes(temp_c, speed_est)) +
    facet_wrap(~ species) +
    geom_hex() +
    geom_smooth(method = 'gam', formula = y ~ s(x, k = 5), color = 'red3') +
    labs(x = 'Temperature (\u00B0C)', y = 'Estimated speed (m/s)')
  
  ggplot(d, aes(sqrt(tp_mm), speed_est)) +
    facet_wrap(~ species) +
    geom_hex() +
    geom_smooth(method = 'gam', formula = y ~ s(x, k = 5), color = 'red3') +
    labs(x = 'sqrt(Precipitation) (sqrt(mm))', y = 'Estimated speed (m/s)')
  
  ggplot(d, aes(sde_mm, speed_est)) +
    facet_wrap(~ species) +
    geom_hex() +
    geom_smooth(method = 'gam', formula = y ~ s(x, k = 5), color = 'red3') +
    labs(x = 'Snow depth (mm)', y = 'Estimated speed (m/s)')
}
