library('ctmm')   # for movement data
library('ecmwfr') # for data from EU Centre for Medium-range Weather Forecasts
library('dplyr')  # for data wrangling

#' might want to see how `wf_request_batch()` works

# get your login info from the copernicus website
if(FALSE) wf_set_key(user = '', key = '', service = 'cds')

# find which dates are needed
times <- readRDS('data/tracking-data/full-dataset.rds') %>%
  pull(tel) %>%
  purrr::map(\(.t) .t$timestamp %>%
               as.POSIXct() %>%
               round(units = 'hours') %>%
               unique() %>%
               as.character()) %>%
  unlist() %>%
  unique() %>%
  sort()

head(times) # check format

#' *NOTE:* if the download fails due to an internal server error or an error
#'         similar to the following:
#'         - `No encoding supplied: defaulting to UTF-8.`,
#'         - `Error in if (private$status == "completed") { :`
#'             `argument is of length zero`,
#'         try again after a minute or two.

#' if the download fails, run the following line to start from the failed time
times <- times[times >= timestamp]
times[1]

hist(lubridate::decimal_date(as.POSIXct(times)))

# to create a new request:
#' *1* copy API from Copernicus website
#' *2* paste it and highlight it and use `Addins` > `MARS to list`
#' *3* edit as needed to run a test download or download all data  
for(timestamp in times) {
  cat(paste0('Downloading ', timestamp, ' at ', Sys.time(), '...\n'))
  request <-
    list(
      variable = c('2m_temperature', 'snow_depth', 'total_precipitation'),
      year = format(as.POSIXct(timestamp), '%Y'),
      month = format(as.POSIXct(timestamp), '%m'),
      day = format(as.POSIXct(timestamp), '%d'),
      time = paste0(format(as.POSIXct(timestamp), '%H'), ':00'),
      area = c(54, -121, 47, -113),
      format = 'netcdf.zip',
      dataset_short_name = 'reanalysis-era5-land',
      target = 'download.netcdf.zip')
  
  # download the data
  ncfile <- wf_request(user = '165099', request = request, transfer = TRUE,
                       path = 'data/ecmwfr-data', verbose = FALSE)
  unzip(zipfile = 'data/ecmwfr-data/download.netcdf.zip',
        exdir = 'data/ecmwfr-data')
  file.rename(from = 'data/ecmwfr-data/data.nc',
              to = paste0('data/ecmwfr-data/',
                          format(as.POSIXct(timestamp), format = '%Y-%m-%d-%H'),
                          '-data.nc'))
}
