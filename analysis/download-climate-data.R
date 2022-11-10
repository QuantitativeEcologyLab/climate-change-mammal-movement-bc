library('climatenaR') # to download climate data and projections
repo_dir <- '~/GitHub/climate-change-animal-movement/' # edit if needed

#' import a Digital Elevation Model (*DEM*) for the region(s) of interest
dem <- 'data/my-dem.tif'

#' convert the DEM to a csv (following the format required by `climatenaR`)
if(! file.exists('data/my-dem.csv')) {
  # conver the DEM to a csv format
  demToCSV(file = dem,      # the DEM object
           outdir = 'data', # save to "data" folder
           srs = NULL)      # keep NULL if in lat/long
  
  # check the csv
  head(read.csv('data/my-dem.csv'))
}

# download historical data
setwd('C:/ClimateNA_v30') # change wd to location of exe file
for(dates in c('1950', '1951_1980', '1981_2010', )) {
  cat('Downloading data for ', dates, '...\n', sep = '') # to track progress
  histClimateNA(
    file = paste0(repo_dir, 'data/my-dem.csv'), # DEM
    dateR = dates, # range of dates from `for` loop
    tFrame = 'M', # monthly averages
    exe = 'ClimateNA_v7.30.exe', # exe location
    outdir = 'climate-change-animal-movement/historical') # output folder
}

#' download projected data
projClimateNA(
  file = paste0(repo_dir, 'data/my-dem.csv'),
  tFrame = 'M', # monthly averages
  exe = 'ClimateNA_v7.30.exe', # exe location
  scen = c("8GCM"), # 8GCMs_ensemble General Circulation Model
  ssp = c("S1", "S2", "S3"), # Shared Socioeconomic Pathway scenarios
  years = c("Y2", "Y3", "Y4", "Y5")) # 2021-2040 to 2081-2100

# convert csv to a TIFF (necessary for spatial cropping)
csv_file <- read.csv('output_ssp126Y.csv')
CSVtoTIFF(Longitude = csv_file$Longitude,
          Latitude = csv_file$LAtitude,
          Value = csv_file$Tmin,
          filename = 'NovaScotia_ssp126Y_Tmin.tif',
          outdir = paste0(repo_dir, 'data'))
