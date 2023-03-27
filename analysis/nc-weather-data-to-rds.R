library('sp')      # for working with spatial data
library('raster')  # for working with raster data
library('ncdf4')   # for nc datasets
library('dplyr')   # for data wrangling (e.g., mutate, %>%, ...)
library('purrr')   # for functional programming (e.g., map_***())
library('ggplot2') # for fancy plots

if(FALSE) {
  # to rename unzipped nc files based on the year(s)
  
}

for(.file in list.files('data/weather-data', pattern = '*.nc',
                        recursive = FALSE)) {
  nc_data <- nc_open(.file, write = FALSE)
  
  time <- ncvar_get(nc_data, 'time')
  cat(.file, ': ',
      paste(as.POSIXct(range(time) * 60^2,
                       origin = as.POSIXct('1900-01-01 00:00:00'),
                       format = '%s') %>%
              format('%Y-%m-%d %H:%M') %>%
              as.character() %>%
              paste0(collapse = ' - ')), '\n', sep = '')
  rm(time)
}

# open the netCDF file
nc_data <- nc_open('data/weather-data/data-1998-2000.nc', write = FALSE)

# import the variables ----
names(nc_data$var) # check variable names

# import each variable
long <- ncvar_get(nc_data, 'longitude')
lat <- ncvar_get(nc_data, 'latitude')
t <- ncvar_get(nc_data, 'time')
temp <- ncvar_get(nc_data, 't2m')
# snow <- ncvar_get(nc_data, 'snowc')
prec <- ncvar_get(nc_data, 'tp')

#' note that *the variables are already on the correct scale*, so there is no
#' need to multiply by the `scale_factor` or add `add_offset` to the values.
#' e.g.:
range(temp) # in Kelvin
range(temp) - 273.15 # in Celsius

# check what values are used for NAs
ncatt_get(nc = nc_data, varid = 'time', attname = 'missing_value') # no NAs
ncatt_get(nc = nc_data, varid = 't2m', attname = 'missing_value') # has NAs
# ncatt_get(nc = nc_data, varid = 'snowc', attname = 'missing_value') # has NAs
ncatt_get(nc = nc_data, varid = 'tp', attname = 'missing_value') # has NAs

# save value as an object
na_value <- ncatt_get(nc_data, varid = 't2m', attname = 'missing_value')$value

# check the NA value is the same for all three variables
na_value ==
  c(ncatt_get(nc = nc_data, varid = 't2m', attname = 'missing_value')$value,
    # ncatt_get(nc = nc_data, varid = 'snowc', attname = 'missing_value')$value,
    ncatt_get(nc = nc_data, varid = 'tp', attname = 'missing_value')$value)

# close the netCDF file
nc_close(nc_data)

# switch the fill value for NA
temp[temp == na_value] <- NA
prec[prec == na_value] <- NA
# snow[snow == na_value] <- NA

# check test raster orientation
bc <- sf::st_as_sf(canadianmaps::PROV) %>% # convert to spatial features object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  sf::st_geometry() # extract boundaries only

if(FALSE) {
  # temperature
  raster(t(temp[, , 1]), xmn = min(long), xmx = max(long), ymn = min(lat),
         ymx = max(lat)) %>%
    terra::plot(main = as.POSIXct(t[1] * 3600,
                                  origin = c('1900-01-01 00:00:00')))
  plot(bc, add = TRUE)
  
  # total precipitation
  raster(t(prec[, , 1]), xmn = min(long), xmx = max(long), ymn = min(lat),
         ymx = max(lat)) %>%
    terra::plot(main = as.POSIXct(t[1] * 3600,
                                  origin = c('1900-01-01 00:00:00')))
  plot(bc, add = TRUE)
  
  # snow depth (features are hard to see with this raster)
  raster(t(snow[, , 1]), xmn = min(long), xmx = max(long), ymn = min(lat),
         ymx = max(lat)) %>%
    terra::plot(main = as.POSIXct(t[6750] * 3600,
                                  origin = c('1900-01-01 00:00:00')))
  plot(bc, add = TRUE)
  
  # features are easier to see with this raster
  raster(t(snow[, , 6750]), xmn = min(long), xmx = max(long), ymn = min(lat),
         ymx = max(lat)) %>%
    terra::plot(main = as.POSIXct(t[6750] * 3600,
                                  origin = c('1900-01-01 00:00:00')))
  plot(bc, add = TRUE)
}

# add data for following years ----
for(.file in c('data/weather-data/data-2001-2003.nc',
               'data/weather-data/data-2004-2006.nc',
               'data/weather-data/data-2007-2009.nc',
               'data/weather-data/data-2019-2021.nc')) {
  
  # open the netCDF file
  cat('Importing', .file, '\n')
  nc_data <- nc_open(.file, write = FALSE)
  
  # import the variables ----
  names(nc_data$var) # check variable names
  
  .long <- ncvar_get(nc_data, 'longitude')
  .lat <- ncvar_get(nc_data, 'latitude')
  .t <- ncvar_get(nc_data, 'time')
  .temp <- ncvar_get(nc_data, 't2m')
  # .snow <- ncvar_get(nc_data, 'snowc')
  .prec <- ncvar_get(nc_data, 'tp')
  
  if(any(c(long, lat) != c(.long, .lat))) {
    stop(paste('File', .file, 'has different coordinates!'))
  }
  
  # save value as an object
  na_value <- ncatt_get(nc_data, varid = 't2m', attname = 'missing_value')$value
  
  # check the NA value is the same for all three variables
  na_values <-
    c(ncatt_get(nc = nc_data, varid = 't2m', attname = 'missing_value')$value,
      # ncatt_get(nc = nc_data, varid = 'snowc', attname = 'missing_value')$value,
      ncatt_get(nc = nc_data, varid = 'tp', attname = 'missing_value')$value)
  
  if(! all(na_values == na_value)) {
    stop(paste('File', .file, 'uses', paste(unique(na_value), collapse = ', '),
               'as NA values instead of', na_value))
  }
  
  # close the netCDF file
  nc_close(nc_data)
  
  # switch the fill value for NA
  .temp[.temp == na_value] <- NA
  # .snow[.snow == na_value] <- NA
  .prec[.prec == na_value] <- NA
  
  temp <- abind::abind(temp, .temp)
  prec <- abind::abind(prec, .prec)
  # snow <- abind::abind(snow, .snow)
  t <- c(t, .t)
}

# convert temperature from Kelvin to Celsius ----
range(temp)
temp <- temp - 273.15
range(temp)

# transpose each individual matrix ----
layout(t(1:2))
raster(temp[, , 1], xmn = min(long), xmx = max(long), ymn = min(lat),
       ymx = max(lat)) %>%
  terra::plot()
plot(bc, add = TRUE)
raster(t(temp[, , 1]), xmn = min(long), xmx = max(long), ymn = min(lat),
       ymx = max(lat)) %>%
  terra::plot()
plot(bc, add = TRUE)
layout(1)

# need to change dimensions before transposing to avoid alignment issues
temp_t <- array(NA, dim = c(dim(temp)[c(2:1, 3)]))
prec_t <- array(NA, dim = c(dim(prec)[c(2:1, 3)]))
# snow_t <- array(NA, dim = c(dim(snow)[c(2:1, 3)]))

for(k in 1:dim(temp)[3]) temp_t[, , k] <- t(temp[, , k])
for(k in 1:dim(prec)[3]) prec_t[, , k] <- t(prec[, , k])
# for(k in 1:dim(snow)[3]) snow_t[, , k] <- t(snow[, , k])

raster(temp_t[, , 1], xmn = min(long), xmx = max(long), ymn = min(lat),
       ymx = max(lat)) %>%
  terra::plot()
plot(bc, add = TRUE)

# create a single, final raster brick for each parameter ----
b_temp <- brick(temp_t, xmn = min(long), xmx = max(long), ymn = min(lat),
                ymx = max(lat), crs = CRS('+proj=longlat'))
b_prec <- brick(prec_t, xmn = min(long), xmx = max(long), ymn = min(lat),
                ymx = max(lat), crs = CRS('+proj=longlat'))
# b_snow <- brick(snow_t, xmn = min(long), xmx = max(long), ymn = min(lat),
#                 ymx = max(lat), crs = CRS('+proj=longlat'))

# sanity check
plot(b_temp[[1]])
plot(bc, add = TRUE)
plot(b_prec[[1]])
plot(bc, add = TRUE)
# plot(b_snow[[1]])
# plot(bc, add = TRUE)

# save the rasters and times ----
saveRDS(b_temp, 'data/weather-data/temperature.rds')
saveRDS(b_prec, 'data/weather-data/total-precipitation.rds')
# saveRDS(b_snow, 'data/weather-data/snow-depth.rds')

# convert hours since 01-01-1900 UTC to seconds since 01-01-1900 UTC,
# then convert to date using pacific timezone
times <- as.POSIXct(t * 60^2,
                    origin = as.POSIXct('1900-01-01 00:00:00', tz = 'UTC'),
                    format = '%s', tz = 'America/Los_Angeles')
head(times)
range(times)
saveRDS(times, 'data/weather-data/timestamps.rds')
