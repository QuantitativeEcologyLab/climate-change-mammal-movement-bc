library('climatenaR')   # to download climate data and projections

#' if necessary, install the `climatenaR` package with
#' `remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)`
#' *NOTE:* the `climatenaR` package requires the ClimateNA software.
#'         See https://register.climatena.ca/ to download it

#' metadata for `climateNA` data at:
#' https://s3-us-west-2.amazonaws.com/www.cacpd.org/documents/ClimateNAv7_manual.pdf

#' change the working directory as required by `climatenaR`
setwd('H:/GitHub/climate-change-animal-movement/climate-na/')

if(! file.exists('bc-dem.csv')) {
  #' convert the bc DEM to a csv as required by `climatenaR`
  demToCSV(file = '../data/bc-dem.tif',
           outdir = '.', # save in climate-na folder
           srs = NULL) # keep NULL if in lat/long
  
  # check the csv
  read.csv('bc-dem.csv', nrows = 5)
}

# download climate data projections ----
projClimateNA(file = 'bc-dem.csv',
              tFrame = 'M', # monthly averages
              exe = 'ClimateNA_v7.31.exe', # exe location (in working directory)
              scen = '8GCM', # 8GCMs_ensemble General Circulation Model
              ssp = c('S1', 'S2', 'S3', 'S5'), # Shared Socioeconomic Pathway scenarios
              years = '2100') # need to convert years to character

# download historical climate data ----
# tracking data ranges from 1998 to 2021
histClimateNA(file = 'bc-dem.csv',
              dateR = '2020', # year
              tFrame = 'M', # monthly averages
              exe = 'ClimateNA_v7.31.exe', # exe location (in working directory)
              outdir = 'bc-dem')
