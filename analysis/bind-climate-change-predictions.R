library('purrr')     # for functional programming (map_***())
library('dplyr')     # for dara wrangling (mutate, transmute, etc.)
library('tidyr')     # for data wrangling (pivot_longer, pivot_wider, nest)
library('stringi')   # for working with strings
library('lubridate') # for working with dates

# to bind all climate projections from each year together into a single RDS file
d <-
  # import all files
  map_dfr(
    list.files(
      'climate-na/area-dem-2022-11-27-low-res',
      full.names = TRUE, # necessary to import the files
      pattern = '@'), # only yearly datasets have a "@"
    \(.fname) {
      readr::read_csv(.fname, col_types = '?') %>%
        mutate(file = .fname) # add column of filename for scenario & year
    }) %>%
  # add scenario and year columns
  mutate(scenario = substr(file,
                           start = stri_locate_last(file, regex = '/')[1] + 1,
                           stop = stri_locate_first(file, regex = '@')[1] - 1),
         year = substr(file,
                       start = stri_locate_first(file, regex = '@')[1] + 1,
                       stop = nchar(file) - nchar('.csv'))) %>%
  # only keep necessary columns
  select(scenario, year, Latitude, Longitude, Elevation, Tave01, Tave02, Tave03,
         Tave04, Tave05, Tave06, Tave07, Tave08, Tave09, Tave10, Tave11, Tave12,
         PPT01, PPT02, PPT03, PPT04, PPT05, PPT06, PPT07, PPT08, PPT09, PPT10,
         PPT11, PPT12) %>%
  # pivot to long format
  pivot_longer(-c(scenario, year, Latitude, Longitude, Elevation),
               names_to = 'parameter', values_to = 'value') %>%
  # extract time and parameter columns 
  mutate(month = map_chr(parameter,
                         \(.chr) substr(.chr, nchar(.chr) - 1, nchar(.chr))),
         dec_date = decimal_date(date(paste(year, month, '15', sep = '-'))),
         month = as.numeric(month),
         year = as.numeric(year),
         parameter = map_chr(parameter,
                             \(.chr) substr(.chr, 1, nchar(.chr) - 2))) %>%
  # create separate columns for temperature and precipitation
  pivot_wider(names_from = parameter, values_from = value) %>%
  # convert monthly total precip to hourly total precip
  mutate(first_day = as.Date(paste(year, month, '01', sep = '-')),
         next_month = if_else(month != '12', as.numeric(month + 1), 1),
         next_year = if_else(month != '12', year, year + 1),
         last_day = as.Date(paste(next_year, next_month, '01', sep = '-')),
         hours = as.numeric((last_day - first_day)) * 24,
         tot_precip = PPT / hours) %>%
  # drop temporary columns
  select(-c(first_day, next_month, next_year, last_day, hours, PPT)) %>%
  # change to names used in the models
  rename(temperature = Tave,
         latitude = Latitude,
         longitude = Longitude,
         elevation = Elevation) %>%
  # place month and decimal date columns after the year column
  relocate(c(month, dec_date), .after = year)

# save the output for later use
saveRDS(d, 'data/climate-yearly-projections.rds')
