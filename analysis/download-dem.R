library('dplyr')        # for data wrangling
library('canadianmaps') # to download a shapefile of BC
library('elevatr')      # to download digital elevation models
library('purrr')        # for functional programming (map_***(), etc.)
library('ctmm')         # to work with telemetry data
library('sp')           # for spatial data
library('sf')           # for spatial data

#' if necessary, install the `climatenaR` package with
#' `remotes::install_github('burnett-m/climatenaR', build_vignettes = TRUE)`
#' *NOTE:* the `climatenaR` package requires the ClimateNA software.
#'         See https://register.climatena.ca/ to download it

repo_dir <- '~/GitHub/climate-change-animal-movement/' # edit if needed

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# convert all telemetry datasets to spatial data points ----
tel_bboxes <- readRDS('data/tracking-data/full-dataset.rds') %>%
  pull(tel) %>%
  map(\(.tel) {
    .tel <- SpatialPoints.telemetry(.tel) %>%
      st_as_sf() %>%
      st_transform(crs = '+proj=longlat') %>%
      extent() %>%
      as('SpatialPolygons')
  })

data_bbox <- tel_bboxes %>%
  do.call(what = raster::bind) %>%
  as('SpatialPolygons')
projection(data_bbox) <- '+proj=longlat'
data_bbox <- as.sf(data_bbox)

plot(data_bbox, border = 'red')
plot(bc_shp, add = TRUE)

# import a Digital Elevation Model (DEM) for all of BC
bc_dem <-
  get_elev_raster(locations = bc_shp, # extent of the data
                  z = 3, # zoom (~10 km resolution in BC)
                  expand = 1, # add buffer around locations
                  clip = 'bbox') # clip tiles to expanded bounding box

# import a Digital Elevation Model (DEM) for the region(s) of interest
#' requires the `progress` package
dem <- get_elev_raster(locations = data_bbox, # extent of the data
                       z = 3, # zoom (~10 km resolution in BC)
                       expand = 1, # add buffer around locations
                       clip = 'bbox') # clip tiles to expanded bounding box
if(FALSE) {
  plot(dem)
  plot(data_bbox, border = 'red', add = TRUE)
  plot(bc_shp, add = TRUE)
}
raster::writeRaster(dem, 'data/area-dem-low-res.tif')

get_elev_raster(locations = data_bbox, # extent of the data
                z = 11, # zoom (~50 m resolution in BC)
                expand = 1, # add buffer around locations
                clip = 'bbox') %>% # clip tiles to expanded bounding box
  raster::writeRaster('data/area-dem-high-res.tif')

# convert the low-res DEM to a csv (required by climatenaR)
climatenaR::demToCSV(file = 'data/area-dem-2022-11-27-low-res.tif',
                     outdir = 'data', # save to "data" folder
                     srs = NULL) # keep NULL if in lat/long

# check the csv
read.csv('data/area-dem-2022-11-27-low-res.csv', nrows = 5)
