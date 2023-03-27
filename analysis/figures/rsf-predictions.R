library('canadianmaps') # for shapefile of BC
library('elevatr')      # to download digital elevation models
library('sp')           # for working with spatial data
library('sf')           # for working with spatial data
library('raster')       # for working with raster data (tifs)
library('ncdf4')        # for working with nc raster data
library('dplyr')        # for data wrangling (e.g., mutate, %>%, ...)
library('purrr')        # for functional programming (e.g., map_***())
library('tidyr')        # for data wrangling (pivot_*() and unnest())
library('ggplot2')      # for fancy plots
library('ctmm')         # for movement modeling
library('mgcv')         # for GAMs
library('readr')        # for importing CSVs quickly and efficiently
library('khroma')       # for colorblind-friendly figures
library('rphylopic')    # for animal silhouettes
library('cowplot')      # for fancy multi-panel figures
source('analysis/figures/default-ggplot-theme.R')

extract <- raster::extract # conflicts with tidyr::extract
plot <- raster::plot # conflicts with ctmm::plot

# shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() %>% # extract boundaries only
  st_as_sf() # back to sf format for cropping rasters

# import data ----
#'differences in resolution are ok because I'm using `raster::extract`
# elevation
elev_r <- raster('data/bc-dem.tif') %>%
  crop(bc_shp) %>%
  mask(bc_shp)

# forest (data from http://www.earthenv.org/landcover)
forest_r <- raster('data/bc-forest.tif')

# distance to nearest water body (data from http://www.earthenv.org/landcover)
dist_w_r <- raster('data/bc-distance-from-water.tif')

# resource and weather data for predictions
newd <-
  bind_rows(read_csv('climate-na/bc-dem/bc-dem_2020.csv') %>%
              mutate(scenario = '2020'),
            read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp126@2100.csv') %>%
              mutate(scenario = '2100 SSP 1-2.6'),
            read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp245@2100.csv') %>%
              mutate(scenario = '2100 SSP 2-4.5'),
            read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp370@2100.csv') %>%
              mutate(scenario = '2100 SSP 3-7.0'),
            read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp585@2100.csv') %>%
              mutate(scenario = '2100 SSP 5-8.5')) %>%
  select(scenario, Latitude, Longitude, Tave01:Tave12, PPT01:PPT12) %>%
  pivot_longer(cols = c(Tave01:Tave12, PPT01:PPT12)) %>%
  mutate(value = if_else(value < -9000, NA_real_, value),
         month = as.numeric(substr(name, nchar(name) - 1, nchar(name))),
         name = case_when(grepl('Tave', name) ~ 'temp_c',
                          grepl('PPT', name) ~ 'tp_mm')) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  rename(x = Longitude, y = Latitude) %>%
  mutate(n_days = lubridate::days_in_month(month),
         tp_mm = tp_mm / (n_days * 24), # convert mm/month to mm/h
         sde_mm = 0, # there are now predictions of snow depth
         elev_m = extract(x = elev_r, y = tibble(x, y)),
         forest_perc = extract(x = forest_r, y = tibble(x, y)),
         dist_w_m = extract(x = dist_w_r, y = tibble(x, y)),
         sde_mm = 0) # no data on 2100 snow depth available; excluded in preds

# check rainfall predictions are sensible
newd %>%
  group_by(x, y) %>%
  summarize(tp_mm = mean(tp_mm, na.rm = TRUE)) %>%
  ggplot() +
  geom_raster(aes(x, y, fill = tp_mm * 24 * 365))

for(SPECIES in c('Oreamnos_americanus', 'Puma_concolor', 'Rangifer_tarandus',
                 'Ursus_arctos_horribilis', 'Cervus elaphus')) {
  
  cat(paste0('Making plots for ', SPECIES, '...\n'))
  
  # import tracking data
  d <- readRDS('data/tracking-data/full-dataset.rds') %>%
    filter(species == SPECIES) %>%
    pull(tel) %>%
    map_dfr(data.frame) %>%
    tibble()
  
  # import the Hierarchical Resource Selection Function
  m <- case_when(
    SPECIES == 'Cervus elaphus' ~
      'models/hrsf-Cervus-elaphus-2023-03-14-tenth-of-dataset.rds',
    SPECIES == 'Oreamnos_americanus' ~
      'models/hrsf-Oreamnos-americanus-2023-03-12.rds',
    SPECIES == 'Puma_concolor' ~
      'models/hrsf-Puma-concolor-2023-03-13.rds',
    SPECIES == 'Rangifer_tarandus' ~
      'models/hrsf-Rangifer-tarandus-2023-03-13.rds',
    SPECIES == 'Ursus_arctos_horribilis' ~
      'models/hrsf-Ursus-arctos-horribilis-2023-03-13.rds') %>%
    readRDS()
  
  # extract sampling times ---
  range(d$timestamp)
  range(d$timestamp) %>% diff()
  
  # find min and max elevations used by animals
  MIN_ELEV_M <- min(filter(m$model, `detected/K` != 0)$elev_m, na.rm = TRUE)
  MAX_ELEV_M <- max(filter(m$model, `detected/K` != 0)$elev_m, na.rm = TRUE)
  
  # make predictions 
  preds <- mutate(
    newd,
    mu = predict(m, newdata = newd, type = 'response',
                 # exclude the intercept to scale to an average of 1
                 exclude = c('(Intercept)',
                             'ti(forest_perc,sqrt(sde_mm))',
                             'ti(sqrt(dist_w_m),sqrt(sde_mm))',
                             'ti(elev_m,sqrt(sde_mm))'))) %>%
    # take yearly averages
    group_by(scenario, x, y, elev_m, forest_perc, dist_w_m) %>%
    summarize(mu = mean(mu), .groups = 'drop') %>%
    # remove locations with altitudes too far from the data
    mutate(mu = if_else(elev_m < MIN_ELEV_M - 500, NA_real_, mu),
           mu = if_else(elev_m > MAX_ELEV_M + 500, NA_real_, mu),
           # remove locations too far from water (relative to the data)
           mu = if_else(sqrt(dist_w_m) > max(m$model$`sqrt(dist_w_m)`),
                        NA_real_, mu))
  
  if(SPECIES == 'Rangifer_tarandus') {
    bounds <-
      read_sf('data/species-ranges-shapefiles/caribou/Herd_Boundaries.shp') %>%
      st_geometry() %>%
      st_transform(crs = '+proj=longlat') %>%
      st_as_sf() %>%
      st_make_valid()
    
    in_habitat <- mask(elev_r, bounds)
    values(in_habitat) <- ! is.na(values(in_habitat))
    preds$in_habitat <- extract(in_habitat, select(preds, x, y)) > 0
  } else {
    preds$in_habitat <- TRUE
  }
  
  SPECIES %>%
    tolower() %>%
    stringr::str_replace_all(pattern = '_', replacement = '-') %>%
    stringr::str_replace_all(pattern = ' ', replacement = '-') %>%
    paste0('data/habitat-quality-', ., '.rds') %>% 
    saveRDS(preds, file = .)
  
  # get phylopic for the species
  # pic <- case_when(
  #   SPECIES == 'Cervus elaphus' ~
  #     image_data('dbf886d8-42f3-4d7b-8a93-45d4b3e72f9c', size = '512')[[1]],
  #   SPECIES == 'Oreamnos_americanus' ~
  #     'models/hrsf-Oreamnos-americanus-2023-03-07.rds',
  #   SPECIES == 'Puma_concolor' ~
  #     'models/hrsf-Puma-concolor-2023-03-07.rds',
  #   SPECIES == 'Rangifer_tarandus' ~
  #     'models/hrsf-Rangifer-tarandus-2023-03-07.rds',
  #   SPECIES == 'Ursus_arctos_horribilis' ~
  #     'models/hrsf-Ursus-arctos-horribilis-2023-03-08.rds')
  
  p_1 <-
    preds %>%
    #' convert to `log2` scale and cap `mu`
    mutate(mu = log2(mu),
           mu = if_else(mu > 2, 2, mu),
           mu = if_else(mu < -2, -2, mu)) %>%
    mutate(scenario = factor(
      scenario,
      levels = c('2020', '2100 SSP 1-2.6', '2100 SSP 2-4.5',
                 '', '2100 SSP 3-7.0', '2100 SSP 5-8.5'))) %>%
    ggplot() +
    facet_wrap(~ scenario) +
    geom_sf(data = bc_shp, fill = 'grey75', color = NA) +
    geom_raster(aes(x, y, fill = mu),# alpha = in_habitat),
                na.rm = TRUE) +
    geom_sf(data = bc_shp, fill = NA, color = 'black') +
    scale_fill_PRGn(name = expression(bold(Habitat~quality~(log[2]))),
                    limits = c(-2, 2), na.value = NA,
                    breaks = -2:2, labels = 2^(-2:2)) +
    scale_alpha_manual(values = c(0.75, 1), breaks = c(FALSE, TRUE),
                       guide = 'none') +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_void() +
    theme(text = element_text(face = 'bold'),
          strip.text = element_text(size = 13),
          legend.position = c(0.04, 0.73))
  
  # create figure of change since 2020
  preds_change <- preds %>%
    select(scenario, x, y, elev_m, mu, in_habitat) %>%
    pivot_wider(names_from = scenario, values_from = mu) %>%
    pivot_longer(cols = -c(x, y, elev_m, `2020`, in_habitat),
                 names_to = 'scenario', values_to = 'mu') %>%
    mutate(mu = mu / `2020`)
  
  SPECIES %>%
    tolower() %>%
    stringr::str_replace_all(pattern = '_', replacement = '-') %>%
    stringr::str_replace_all(pattern = ' ', replacement = '-') %>%
    paste0('data/habitat-quality-change-', ., '.rds') %>% 
    saveRDS(preds_change, file = .)
  
  p_2 <-
    preds_change %>%
    # drop locations with altitudes too far from the data
    # mutate(mu = if_else(elev_m < MIN_ELEV_M - 500, NA_real_, mu),
    #        mu = if_else(elev_m > MAX_ELEV_M + 500, NA_real_, mu)) %>%
    #' convert to `log2` scale and cap `mu`
    mutate(mu = log2(mu),
           mu = if_else(mu > 2, 2, mu),
           mu = if_else(mu < -2, -2, mu)) %>%
    ggplot() +
    facet_wrap(~ scenario) +
    geom_sf(data = bc_shp, fill = 'grey75', color = NA) +
    geom_raster(aes(x, y, fill = mu),# alpha = in_habitat),
                na.rm = TRUE) +
    geom_sf(data = bc_shp, fill = NA, color = 'black') +
    scale_fill_BuRd(name=expression(bold(Change~'in'~habitat~quality~(log[2]))),
                    limits = c(-2, 2), na.value = 'transparent',
                    breaks = -2:2, labels = 2^(-2:2), reverse = TRUE)+
    scale_alpha_manual(values = c(0.75, 1), breaks = c(FALSE, TRUE),
                       guide = 'none') +
    scale_x_continuous(NULL, breaks = NULL) +
    scale_y_continuous(NULL, breaks = NULL) +
    theme_void() +
    theme(text = element_text(face = 'bold'),
          strip.text = element_text(size = 13),
          legend.position = c(0.04, 0.73))
  
  if(exists('bounds')) {
    p_1 <- p_1 +
      geom_sf(data = bounds, color = 'white', fill = NA, linewidth = 1) +
      geom_sf(data = bounds, color = 'black', fill = NA)
    p_2 <- p_2 + geom_sf(data = bounds, color = 'black', fill = NA)
  }
  
  # make density plots of each resource
  p_3 <- preds %>%
    filter(scenario == '2020') %>% # no need to repeat data
    filter(! is.na(mu)) %>%
    select(elev_m, forest_perc, dist_w_m) %>%
    pivot_longer(cols = everything()) %>%
    mutate(name = case_when(name == 'dist_w_m' ~ 'Distance from water (m)',
                            name == 'elev_m' ~ 'Elevation (m)',
                            name == 'forest_perc' ~ 'Forest cover (%)')) %>%
    ggplot() +
    facet_wrap(~ name, ncol = 1, strip.position = 'bottom', scales = 'free') +
    geom_histogram(aes(value), bins = 10, color = 'black', fill = 'grey75') +
    ylab('Number of raster cells') +
    theme(strip.placement = 'outside', strip.background = element_blank(),
          axis.title.x = element_blank(),
          strip.text = element_text(face = 'bold', size = 11),
          plot.background = element_rect(color = 'black',
                                         fill = 'transparent'))
  
  p <- plot_grid(p_1,
                 plot_grid(p_2, NULL, rel_widths = c(5, 1)),
                 ncol = 1, labels = c('a.', 'b.'),
                 label_x = 0.04, label_y = 0.95, align = 'v', axis = 'l')
  ggdraw(p) +
    draw_plot(p_3, x = 0.71, y = 0, width = 0.29, height = 0.75) +
    draw_label(label = 'c.', fontface = 'bold', x = 0.73, y = 0.74)
  
  ggsave(paste0('figures/bc-habitat-quality-', SPECIES, '.png'),
         width = 12, height = 12, dpi = 600, bg = 'white', scale = 1.25)
  
  if(SPECIES == 'Rangifer_tarandus') rm(bounds)
}; beepr::beep()
