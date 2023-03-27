
library('canadianmaps') # for shapefile of BC
library('sp')           # for working with spatial data
library('sf')           # for working with spatial data
library('dplyr')        # for data wrangling (e.g., mutate, %>%, ...)

# shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() %>% # extract boundaries only
  st_as_sf() # back to sf format for cropping rasters

for(SPECIES in c('Oreamnos_americanus', 'Puma_concolor', 'Rangifer_tarandus',
                 'Ursus_arctos_horribilis', 'Cervus elaphus')[c(1, 3)]) {
  
  # import tracking data
  d <- readRDS('data/tracking-data/full-dataset.rds') %>%
    filter(species == SPECIES) %>%
    pull(tel) %>%
    map_dfr(data.frame) %>%
    tibble() %>%
    select(longitude, latitude) %>%
    SpatialPoints()
  
  range_file <-
    case_when(SPECIES == 'Oreamnos_americanus' ~ 'mountain-goat/ORAM_HCA.shp',
              SPECIES == 'Puma_concolor' ~ 'NA',
              SPECIES == 'Rangifer_tarandus' ~ 'caribou/Herd_Boundaries.shp',
              SPECIES == 'Ursus_arctos_horribilis' ~ 'NA',
              SPECIES == 'Cervus elaphus' ~ 'NA')
  
  range <-
    read_sf(paste0('data/species-ranges-shapefiles/', range_file)) %>%
    st_geometry() %>%
    st_transform(crs = '+proj=longlat') %>%
    st_buffer(dist = 1e4)
  
  plot(bc_shp)
  plot(range, add = TRUE, col = 'yellow')
  plot(d, add = TRUE, pch = 19)
}
