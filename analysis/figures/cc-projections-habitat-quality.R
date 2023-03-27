library('mgcv')    # for GAMs
library('ctmm')    # for movement data
library('sf')      # for spatial data
library('sp')      # for spatial data
library('dplyr')   # for data wrangling (e.g., mutate, %>%, ...)
library('tidyr')   # for data wrangling (e.g., pivot_***(), ...)
library('purrr')   # for functional programming
library('ggplot2') # for fancy plots
library('khroma')  # for colorblind-friendly figures
library('terra')   # for raster data
library('stringr') # for working with strings
source('analysis/figures/default-ggplot-theme.R')
source('analysis/figures/bc-shapefile.R')

# import climate projections ----
projections <- readRDS('data/climate-yearly-projections.rds') %>%
  rename(elev_m = elevation,
         temp_c = temperature,
         tp_mm = tot_precip) %>%
  mutate(tp_mm = tp_mm  * 1e3 / (lubridate::days_in_month(month) * 24),
         sde_mm = 0) %>% # no snow depth projections available
  nest(data = -c(longitude, latitude))

# find average locations for every species ----
newd <-
  readRDS('models/movement-models.rds') %>%
  # find average location for each animal
  mutate(location = map(model, \(.m) {
    SpatialPoints(.m$mu, proj4string = CRS(.m@info$projection)) %>%
      spTransform(CRSobj = CRS('+proj=longlat')) %>%
      suppressWarnings() %>%
      as.data.frame() %>%
      rename(longitude = x, latitude = y)
  })) %>%
  select(animal, species, location) %>%
  unnest(location) %>%
  # average within species
  group_by(species) %>%
  summarize(longitude = mean(longitude, na.rm = TRUE),
            latitude = mean(latitude, na.rm = TRUE)) %>%
  mutate(data = map2(longitude, latitude, \(.lo, .la) {
    filter(projections,
           longitude - .lo == min(longitude - .lo),
           latitude - .la == min(latitude - .la))$data[[1]]
  })) %>%
  unnest(data) %>%
  mutate(
    scenario = stringi::stri_replace_all(scenario, '',
                                         regex = '8GCMs_ensemble_ssp'),
    scenario = paste0(substr(scenario, 1, 1), '-',
                      substr(scenario, 2, 2), '.',
                      substr(scenario, 3, 3)),
  forest_perc = extract(rast('data/forest.tif'),
                        tibble(longitude, latitude))$consensus_full_class_1,
  dist_w_m = extract(rast('data/distance-from-water.tif'),
                     tibble(longitude, latitude))$consensus_full_class_12,
  species = case_when(species == 'Rangifer_tarandus' ~ 'Caribou',
                      species == 'Puma_concolor' ~ 'Cougar',
                      species == 'Cervus elaphus' ~ 'Elk',
                      species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                      species == 'Oreamnos_americanus' ~ 'Mountain goat'))

# import HRSFs to predict habitat quality ----
hq <- tibble(
  species = c('Caribou', 'Cougar', 'Elk', 'Grizzly bear', 'Mountain goat'),
  model = map(c('models/hrsf-Rangifer-tarandus-2023-03-13.rds',
                'models/hrsf-Puma-concolor-2023-03-13.rds',
                'models/hrsf-Cervus-elaphus-2023-03-14-tenth-of-dataset.rds',
                'models/hrsf-Ursus-arctos-horribilis-2023-03-13.rds',
                'models/hrsf-Oreamnos-americanus-2023-03-12.rds'),
              readRDS))

# predict habitat quality
hq <- mutate(hq,
             preds = map2(model, species,
                          \(.m, .sp) {
                            mutate(
                              filter(newd, species == .sp),
                              mu = predict(
                                .m, newdata = filter(newd, species == .sp),
                                type = 'response',
                                # no predictions of snow depth available
                                exclude = c('(Intercept)',
                                            'ti(forest_perc,sqrt(sde_mm))',
                                            'ti(sqrt(dist_w_m),sqrt(sde_mm))',
                                            'ti(elev_m,sqrt(sde_mm))'))) %>%
                              # take yearly averages
                              group_by(scenario, year) %>%
                              summarize(mu = mean(mu), .groups = 'drop')
                          }))

hq_rel <-
  hq %>%
  select(-model) %>%
  unnest(preds) %>%
  # scale to ralative change
  group_by(species, scenario) %>%
  mutate(ref = mu[2],
         mu_rel = mu / ref) %>%
  ungroup() %>%
  mutate(mu_rel = if_else(mu_rel < 0.25, 0.25, mu_rel),
         mu_rel = if_else(mu_rel > 4, 4, mu_rel))

# plot the estimated change
ggplot(hq_rel, aes(year, mu_rel, color = scenario)) +
  facet_wrap(~ species, scales = 'fixed') +
  geom_hline(yintercept = 1, color = 'grey') +
  geom_point() +
  geom_smooth(se = FALSE, method = 'gam', formula = y ~ s(x)) +
  xlab(NULL) +
  scale_y_continuous(expression(Relative~change~'in'~habitat~quality~(log[2])),
                     trans = 'log2') +
  scale_color_brewer('Scenario', type = 'div', palette = 5, direction = -1,
                     aesthetics = c('color', 'fill')) +
  theme(legend.position = c(0.85, 0.2))

ggsave('figures/climate-change-habitat-quality-relative.png',
       width = 10, height = 5, dpi = 600, bg = 'white')
