library('terra')
library('ctmm')
library('dplyr')
library('purrr')
library('tidyr')
library('ggplot2')
library('mgcv')
library('sp')
library('stringi')
source('analysis/figures/default-ggplot-theme.R')

#' to create a figure of average speed, accounting for the fact that animals may
#' not be moving

# import climate projections ----
projections <- readRDS('data/climate-yearly-projections.rds') %>%
  nest(data = -c(longitude, latitude))

# ensure locations are unique (i.e., all datasets are of same length)
unique(map_dbl(projections$data, nrow))

# raster of locations to find nearest cell (rounding coords loses precision)
# import binomial GAM (moving or not) ----
m_binom <- readRDS('models/binomial-gam-2023-03-03.rds')

# import gamma GAM (movement speed given that animal is moving) ----
m_gamma <- readRDS('models/gamma-gam-2023-02-27.rds')

# find average locations for every species
locations <-
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
            latitude = mean(latitude, na.rm = TRUE))

ggplot(locations, aes(longitude, latitude, color = species)) +
  coord_equal() +
  geom_point() +
  scale_color_brewer('Species', type = 'qual', palette = 6)

# add temperatures to create a tibble of new data to predict from
newd <-
  locations %>%
  mutate(data = map2(longitude, latitude, \(.lo, .la) {
    filter(projections,
           longitude - .lo == min(longitude - .lo),
           latitude - .la == min(latitude - .la))$data[[1]]
  }),
  times = list(tibble(time_of_day = seq(0, 1, length.out = 10)))) %>%
  unnest(data) %>%
  mutate(doy = lubridate::yday(lubridate::date_decimal(dec_date))) %>%
  unnest(times) %>%
  mutate(scenario = stri_replace_all(scenario, '',
                                     regex = '8GCMs_ensemble_ssp'),
         scenario = paste0(substr(scenario, 1, 1), '-',
                           substr(scenario, 2, 2), '.',
                           substr(scenario, 3, 3)))

# use variable names used in the model
newd <- rename(newd,
               temp_c = temperature,
               tp_mm = tot_precip) %>%
  mutate(sde_mm = 0)

# figure of all animals ----
preds <- mutate(newd,
                p_moving = predict(m_binom, newdata = newd, type = 'response',
                                   exclude = c(
                                     's(sqrt(sde_mm),species)',
                                     'ti(temp_c,sqrt(sde_mm),species)',
                                     'ti(sqrt(tp_mm),sqrt(sde_mm),species)')),
                speed = predict(m_gamma, newdata = newd, type = 'response',
                                exclude = c(
                                  's(sqrt(sde_mm),species)',
                                  'ti(temp_c,sqrt(sde_mm),species)',
                                  'ti(sqrt(tp_mm),sqrt(sde_mm),species)')),
                speed = speed * p_moving, # overall average
                speed = speed / 1000 * 24 * 60 * 60) %>% # from m/s to km/day
  # average movement speed without assuming animal is moving
  group_by(species, longitude, latitude, scenario, year,
           month, dec_date, elevation, temp_c, tp_mm) %>%
  summarize(speed = mean(speed), .groups = 'drop') %>%
  group_by(species, year, scenario) %>%
  summarize(q_05 = quantile(speed, 0.025),
            q_95 = quantile(speed, 0.975),
            mu = mean(speed),
            .groups = 'drop') %>%
  mutate(species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'))

# changes in mean yearly speed
ggplot(preds) +
  facet_wrap(~ species, scales = 'free_y') +
  geom_point(aes(year, mu, color = scenario, group = scenario), alpha = 0.5) +
  # algorithm occasionally does not converge if k = 10
  geom_smooth(aes(year, mu, color = scenario, group = scenario), linewidth = 1,
              method = 'gam', formula = y ~ s(x, k = 9), se = FALSE,
              method.args = list(family = Gamma(link = 'log'))) +
  scale_color_brewer('Scenario', type = 'div', palette = 5, direction = -1,
                     aesthetics = c('color', 'fill')) +
  labs(x = NULL, y = 'Estimated speed (km/day)') +
  theme(legend.position = c(0.85, 0.2)) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

ggsave('figures/climate-change-projections.png',
       width = 10, height = 5, dpi = 600, bg = 'transparent', scale = 0.75)

# relative changes in mean yearly speed
preds %>%
  filter(year == min(year)) %>%
  group_by(species) %>%
  summarize(mu0 = mean(mu)) %>%
  right_join(preds, by = 'species') %>%
  mutate(mu = mu / mu0) %>%
  ggplot() +
  facet_wrap(~ species) +
  geom_hline(yintercept = 1, alpha = 0.3) +
  geom_point(aes(year, mu, color = scenario, group = scenario), alpha = 0.5) +
  geom_smooth(aes(year, mu, color = scenario, group = scenario), linewidth = 1,
              method = 'gam', formula = y ~ s(x), se = FALSE,
              method.args = list(family = Gamma(link = 'log'))) +
  scale_color_brewer('Scenario', type = 'div', palette = 5, direction = -1,
                     aesthetics = c('color', 'fill')) +
  labs(x = NULL, y = 'Relative change in speed') +
  theme(legend.position = c(0.85, 0.2)) +
  guides(color = guide_legend(override.aes = list(alpha = 1)))

ggsave('figures/climate-change-projections-relative.png',
       width = 10, height = 5, dpi = 600, bg = 'transparent')

# relative changes in mean yearly speed with 90% quantiles
preds %>%
  filter(year == min(year)) %>%
  group_by(species) %>%
  summarize(mu0 = mean(mu)) %>%
  right_join(preds, by = 'species') %>%
  mutate(q_05 = q_05 / mu0,
         q_95 = q_95 / mu0,
         mu = mu / mu0) %>%
  ggplot() +
  facet_wrap(~ species, scales = 'free_y') +
  geom_ribbon(aes(year, ymin = q_05, ymax = q_95, fill = scenario,
                  group = scenario), alpha = 0.2) +
  geom_line(aes(year, q_05, color = scenario, group = scenario)) +
  geom_line(aes(year, q_95, color = scenario, group = scenario)) +
  geom_smooth(aes(year, mu, color = scenario, group = scenario), linewidth = 1,
              method = 'gam', formula = y ~ s(x), se = FALSE,
              method.args = list(family = Gamma(link = 'log'))) +
  scale_color_brewer('Scenario', type = 'div', palette = 5, direction = -1,
                     aesthetics = c('color', 'fill')) +
  labs(x = NULL, y = 'Relative change in speed') +
  ylim(c(0, NA)) +
  theme(legend.position = c(0.85, 0.2)) +
  guides(fill = guide_legend(override.aes = list(alpha = 0)))

ggsave('figures/climate-change-projections-quantiles.png',
       width = 10, height = 5, dpi = 600, bg = 'transparent', scale = 0.75)
