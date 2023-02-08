library('sp')      # for working with spatial data
library('raster')  # for working with raster data
library('ncdf4')   # for nc datasets
library('dplyr')   # for data wrangling (e.g., mutate, %>%, ...)
library('purrr')   # for functional programming (e.g., map_***())
library('ggplot2') # for fancy plots
library('ctmm')    # for movement modeling
source('functions/detrended_speeds.R') # to remove baseline noise in speed

theme_set(theme_bw())

#' *parallelize using furrr*

# add weather data and speeds to the telemetry data ----
# if(file.exists('data/standardized-speeds.rds')) {
if(FALSE) {
  d <- readRDS('data/standardized-speeds.rds')
} else {
  d <-
    readRDS('models/movement-models.rds') %>%
    mutate(annotated = imap(tel, \(.tel, i) {
      cat('Working on animal', i, '\n')
      
      # telemetry as unprojected spatial points object
      .tel_sp <- spTransform(SpatialPoints.telemetry(.tel), '+proj=longlat')
      
      # estimate speeds
      speeds_data <-
        detrend.speeds(DATA = .tel, CTMM = model[[i]]) %>%
        transmute(speed_low = low, speed_est = est, speed_high = high)
      
      # add weather data if speeds can be estimated
      if(any(! is.na(speeds_data$speed_est))) {
        # telemetry as data frame
        weather_data <-
          data.frame(.tel) %>%
          # annotated weather
          #' *note*: `map()` cannot accept rasters as input
          mutate(
            nearest_hour = round(timestamp, units = 'hours') %>%
              format(format = '%Y-%m-%d-%H'),
            # temperature (degrees Kelvin)
            temp_c = imap_dbl(
              nearest_hour,
              \(fn, ..i) {
                paste0('data/ecmwfr-data/', nearest_hour[..i],
                       '-data.nc') %>%
                  raster(varname = 't2m') %>%
                  raster::extract(.tel_sp[..i])
              }),
            # hourly precipitation (mm)
            tp_mm = imap_dbl(
              nearest_hour,
              \(fn, ..i) {
                paste0('data/ecmwfr-data/', nearest_hour[..i],
                       '-data.nc') %>%
                  raster(varname = 'tp') %>%
                  extract(.tel_sp[..i])
              }),
            # snow depth (mm)
            sde_mm = imap_dbl(
              nearest_hour,
              \(fn, ..i) {
                paste0('data/ecmwfr-data/', nearest_hour[..i],
                       '-data.nc') %>%
                  raster(varname = 'sde') %>%
                  extract(.tel_sp[..i])
              })
          ) %>%
          select(c(timestamp, longitude, latitude, temp_c, tp_mm,
                   sde_mm)) %>%
          mutate(temp_c = temp_c - 273.15) # convert from Kelvin to Celsius
      } else { # otherwise add NAs
        weather_data <-
          transmute(data.frame(.tel),
                    timestamp, longitude, latitude, # keep necessary columns
                    temperature = NA_real_, # add NAs for the other columns
                    tot_precip = NA_real_, 
                    snow_depth = NA_real_)
      }
      
      return(bind_cols(weather_data, speeds_data))
    })) %>%
    select(animal, species, dataset_name, annotated) %>%
    tidyr::unnest(annotated)
  saveRDS(d, 'data/standardized-speeds.rds')
}

if(FALSE) {
  ggplot(d) +
    facet_wrap(~ species) +
    geom_point(aes(temp_c, speed_est), alpha = 0.3) +
    # geom_errorbar(aes(temp_c, ymin = speed_low, ymax = speed_high),
    #               alpha = 0.3) +
    labs(x = 'Temperature (\u00B0C)', y = 'Estimated speed (m/s)')
  
  ggplot(d) +
    facet_wrap(~ species) +
    geom_point(aes(tp_mm, speed_est), alpha = 0.3) +
    labs(x = 'Precipitation (mm)', y = 'Estimated speed (m/s)')
  
  ggplot(d) +
    facet_wrap(~ species) +
    geom_point(aes(sde_mm, speed_est), alpha = 0.3) +
    labs(x = 'Snow depth (mm)', y = 'Estimated speed (m/s)')
}
