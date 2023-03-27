library('sp')           # for spatial data
library('sf')           # for spatial data
library('dplyr')        # for data wrangling
library('canadianmaps') # for shapefile of BC
library('elevatr')      # for downloading DEM

# shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

bc_dem <- get_elev_raster(locations = bc_shp, z = 5, clip = 'locations')

# ensure res is not lower than that of the HFI raster
if(all(terra::res(bc_dem) >=
       terra::res(raster::raster('data/ml_hfi_v1_2019.nc')))) {
  cat('All good.\n')
} else {
  stop('DEM RESOLUTION IS TOO LOW.')
}

plot(bc_dem)
plot(bc_shp, add = TRUE)

raster::writeRaster(bc_dem, 'data/bc-dem.tif')

# ensure res is not lower than that of the HFI raster
if(all(terra::res(raster::raster('data/bc-dem.tif')) >=
       terra::res(raster::raster('data/ml_hfi_v1_2019.nc')))) {
  cat('All good.\n')
} else {
  stop('DEM RESOLUTION IS TOO LOW.')
}
