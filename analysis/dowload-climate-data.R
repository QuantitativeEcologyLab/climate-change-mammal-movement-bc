library('ecmwfr') # for data from EU Centre for Medium-Range Weather Forecasts
library('dplyr')  # for data wrangling

wf_set_key(user = '165099',
           key = '5612de2d-a0f8-438e-883f-5e8160e1a336',
           service = 'cds')

# find which dates are needed
dates <- readRDS('data/tracking-data/full-dataset.rds') %>%
  pull(tel) %>%
  purrr::map(\(.t) as.character(unique(as.Date(.t$timestamp)))) %>%
  unlist() %>%
  unique() %>%
  sort()

# to create a new request:
#' *1* copy API from Copernicus website
#' *2* paste it and highlight it and use `Addins` > `MARS to list`
#' *3* edit as needed to run a test download or download all data  
for(day in dates) {
  message(paste0('Downloading ', day, '...'))
  request <-
    list(
      variable = c('2m_temperature', 'snow_depth', 'total_precipitation'),
      year = lubridate::year(day),
      month = lubridate::month(day),
      day = lubridate::day(day),
      time = c('00:00', '01:00', '02:00', '03:00', '04:00', '05:00',
               '06:00', '07:00', '08:00', '09:00', '10:00', '11:00',
               '12:00', '13:00', '14:00', '15:00', '16:00', '17:00',
               '18:00', '19:00', '20:00', '21:00', '22:00', '23:00'),
      area = c(54, -121, 47, -113),
      format = 'netcdf.zip',
      dataset_short_name = 'reanalysis-era5-land',
      target = 'download.netcdf.zip')
  
  # download the data
  ncfile <- wf_request(user = '165099', request = request, transfer = TRUE,
                       path = 'data/ecmwfr-data', verbose = TRUE)
  unzip(zipfile = 'data/ecmwfr-data/download.netcdf.zip',
        exdir = 'data/ecmwfr-data')
  file.rename(from = 'data/ecmwfr-data/data.nc',
              to = paste0('data/ecmwfr-data/', day, '-data.nc'))
}
