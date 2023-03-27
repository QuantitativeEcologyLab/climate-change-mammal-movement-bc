library('dplyr') # for data wrangling
library('purrr') # for functional programming
library('sf')    # for working with spatial data
library('sp')    # for working with spatial data
library('terra') # for raster data (faster than raster)
library('canadianmaps') # to download a shapefile of BC

# import a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# original rasters from http://www.earthenv.org/landcover
dem <- rast('data/area-dem-low-res.tif')
plot(dem)
plot(bc_shp, add = TRUE)

# forest raster
rast('data/uncropped-rasters/consensus_full_class_1.tif') %>%
  crop(dem) %>%
  writeRaster('data/forest.tif')

plot(rast('data/forest.tif'))
plot(bc_shp, add = TRUE, lwd = 3)

# raster of open water
layout(matrix(1:8, ncol = 2, byrow = TRUE))
w <- rast('data/uncropped-rasters/consensus_full_class_12.tif') %>%
  crop(dem)
plot(w, main = 'Percent water')
plot(bc_shp, add = TRUE)
hist(w, breaks = 100, main = 'Percent water')

w01 <- ceiling(w / 100) # convert anything above 0 to a 1
plot(w01, main = 'Yes/No water')
hist(w01, main = 'Yes/No water')

w1 <- classify(w01, cbind(0, NA))
plot(w1, main = 'Water only', col = 'blue')
hist(w1, main = 'Water only')

dist <- distance(w1)
plot(dist, main = 'Distance from nearest water')
hist(dist, main = 'Distance from nearest water')
writeRaster(dist, 'data/distance-from-water.tif')

# create a raster of distance to water for all of BC ----
library('canadianmaps') # for shapefile of bc

# download a shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() # extract boundaries only

# raster of tree cover
raster('data/uncropped-rasters/consensus_full_class_1.tif') %>%
  crop(bc_shp) %>%
  mask(bc_shp) %>%
  writeRaster('data/bc-forest.tif')
plot(raster('data/bc-forest.tif'))

# raster of open water
layout(matrix(1:8, ncol = 2, byrow = TRUE))
w <- rast('data/uncropped-rasters/consensus_full_class_12.tif') %>%
  crop(bc_shp) %>%
  aggregate(8)
plot(w, main = 'Percent water')
hist(w, breaks = 100, main = 'Percent water')

w01 <- ceiling(w / 100) %>% # convert anything above 0 to a 1
  mask(st_as_sf(bc_shp)) # remove sea
plot(w01, main = 'Yes/No water')
hist(w01, main = 'Yes/No water')

w1 <- classify(w01, cbind(0, NA))
plot(w1, main = 'Water only', col = 'blue')
hist(w1, main = 'Water only')

dist <- distance(w1)
dist <- mask(dist, st_as_sf(bc_shp))
plot(dist, main = 'Distance from nearest water')
hist(dist, main = 'Distance from nearest water')
writeRaster(dist, 'data/bc-distance-from-water-coarse.tif')
