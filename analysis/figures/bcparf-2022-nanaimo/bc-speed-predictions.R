library('raster')  # for raster data
library('dplyr')   # for data wrangling
library('tidyr')   # for data wrangling
library('purrr')   # for functional programming
library('mgcv')    # for generalized additive models
library('ggplot2') # for fancy figures
library('khroma')  # for colorblind-friendly palettes
source('analysis/figures/default-ggplot-theme.R') # for consistent theme

# create a color palette
muted <- color('muted')
plot_scheme(muted(5), colours = TRUE, names = TRUE, size = 0.9)

sunset <- color('sunset')
plot_scheme(sunset(5), colours = TRUE, names = TRUE, size = 0.9)

# import shapefile of bc
# download a shapefile of British Columbia
bc_shp <-
  sf::st_as_sf(canadianmaps::PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  sf::st_geometry() # extract boundaries only

# import climate data ---
# csv for 2100 1-2.6 has impossible values for problematic latitudes > 500,
# and rasters are at a lower resolution (so import is faster) 
d <- bind_rows(
  bind_cols(raster('data/bc-temperature-2005.tif') %>%
              as.data.frame(xy = TRUE) %>%
              rename(temperature = layer),
            raster('data/bc-tot_precip-2005.tif') %>%
              as.data.frame(xy = FALSE) %>%
              rename(tot_precip = layer)) %>%
    mutate(scenario = '2005'),
  bind_cols(raster('data/bc-temperature-2100-ssp126.tif') %>%
              as.data.frame(xy = TRUE) %>%
              rename(temperature = layer),
            raster('data/bc-tot_precip-2100-ssp126.tif') %>%
              as.data.frame(xy = FALSE) %>%
              rename(tot_precip = layer)) %>%
    mutate(scenario = 'SSP 1-2.6'),
  bind_cols(raster('data/bc-temperature-2100-ssp585.tif') %>%
              as.data.frame(xy = TRUE) %>%
              rename(temperature = layer),
            raster('data/bc-tot_precip-2100-ssp585.tif') %>%
              as.data.frame(xy = FALSE) %>%
              rename(tot_precip = layer)) %>%
    mutate(scenario = 'SSP 5-8.5'))

#' remove NAs; kept because `geom_raster` is faster than `geom_tile`
d <- filter(d, ! is.na(temperature), ! is.na(tot_precip))

# check data
ggplot(d) +
  facet_wrap(~ scenario) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = temperature))

ggplot(d) +
  facet_wrap(~ scenario) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = tot_precip))

# import models ----
m_1 <- readRDS('models/binomial-gam-2022-12-08.rds')
m_2 <- readRDS('models/gamma-gam-2022-12-08.rds')

# predict speeds ----
# create new data for predictions
newd <- expand_grid(doy = 100,
                    time_of_day = 0.5,
                    species = unique(m_1$model$species)) %>%
  mutate(data = list(d)) %>%
  unnest(data)

preds <-
  mutate(newd,
         p_moving = predict(m_1, newdata = newd, type = 'response',
                            exclude = c('s(time_of_day,species)',
                                        's(doy,species)')),
         speed = predict(m_1, newdata = newd, type = 'response',
                         exclude = c('s(time_of_day,species)',
                                     's(doy,species)')),
         # convert to overall average speed (accounting for P(moving))
         speed = p_moving * speed) %>%
  mutate(species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'),
         speed = speed / 1e3 * 24 * 60 * 60) # convert from m/s to km/day

# figure of estimated bear speeds in 2005  ----
bear_2005 <-
  preds %>%
  filter(scenario == '2005', species == 'Grizzly bear') %>%
  # cap speeds near 95% quantile
  mutate(speed = if_else(speed > 0.35, 0.35, speed))

p_bear_2005 <-
  ggplot() +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = speed), bear_2005) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_gradientn('Speed (km/day)', colours = muted(5),
                       na.value = '#DDDDDD',
                       limits = c(0.2, 0.5),
                       breaks = c(0.2, 0.5)) +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank())

ggsave('figures/bcparf-2022-nanaimo/grizzly-bc-2005-speeds.png',
       plot = p_bear_2005, scale = 0.75, width = 16, height = 8, dpi = 300,
       bg = 'white')

# figure of estimated bear speeds in 2100 ----
bear_2100 <-
  preds %>%
  filter(scenario != '2005', species == 'Grizzly bear') %>%
  # cap speeds at 95% quantile
  mutate(speed = if_else(speed > 0.5, 0.5, speed))

p_bear_2100 <-
  ggplot() +
  facet_wrap(~ scenario) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = speed), bear_2100) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_gradientn('Speed (km/day)', colours = muted(5),
                       na.value = '#DDDDDD',
                       limits = c(0.2, 0.5),
                       breaks = c(0.2, 0.5)) +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), strip.background = element_blank())

ggsave('figures/bcparf-2022-nanaimo/grizzly-bc-2100-speeds.png',
       plot = p_bear_2100, scale = 0.75, width = 16, height = 8, dpi = 300,
       bg = 'white')

# figure of change in estimated bear speeds between 2005 and 2100 ----
bear_diff <-
  preds %>%
  filter(species == 'Grizzly bear') %>%
  select(x, y, speed, scenario) %>%
  pivot_wider(values_from = speed, names_from = scenario) %>%
  mutate(`SSP 1-2.6` = `SSP 1-2.6` - `2005`,
         `SSP 5-8.5` = `SSP 5-8.5` - `2005`) %>%
  select(- `2005`) %>%
  pivot_longer(c(`SSP 1-2.6`, `SSP 5-8.5`))

p_bear_diff <-
  ggplot() +
  facet_wrap(~ name) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = value), bear_diff) +
  scale_x_continuous(NULL, breaks = NULL) +
  scale_y_continuous(NULL, breaks = NULL) +
  scale_fill_gradientn('Change in speed (km/day)', colours = sunset(5),
                       na.value = '#FFFFFF',
                       limits = c(-0.3, 0.3),
                       breaks = c(-0.3, 0.3)) +
  theme(panel.border = element_blank(), axis.ticks = element_blank(),
        axis.text = element_blank(), strip.background = element_blank())

ggsave('figures/bcparf-2022-nanaimo/grizzly-bc-diff-speeds.png',
       plot = p_bear_diff, scale = 0.75, width = 16, height = 8, dpi = 300,
       bg = 'white')


# figures of estimated relative speeds for all animals ----
preds_rel <-
  preds %>%
  # drop unnecessary columns
  select(scenario, species, x, y, speed) %>%
  # cap speeds at 95% quantile for each species and scenario
  group_by(species, scenario) %>%
  mutate(speed = if_else(speed > quantile(speed, 0.95, na.rm = TRUE),
                         true = quantile(speed, 0.95, na.rm = TRUE),
                         false = speed)) %>%
  ungroup() %>%
  # convert to relative speeds based on the 2005 max
  pivot_wider(values_from = speed, names_from = scenario) %>%
  group_by(species) %>%
  mutate(max_2005 = max(`2005`, na.rm = TRUE),
         `2005` = `2005` / max_2005,
         `SSP 1-2.6` = `SSP 1-2.6` / max_2005 - 1,
         `SSP 5-8.5` = `SSP 5-8.5` / max_2005 - 1) %>%
  ungroup()

# current time (2005) ---
p_2005 <-
  ggplot() +
  facet_wrap(~ species) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = `2005`), preds_rel) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL) +
  scale_fill_gradientn('Relative speed', colours = muted(5),
                       limits = c(0, 1), na.value = '#DDDDDD')

ggsave('figures/bcparf-2022-nanaimo/bc-speeds-2005.png',
       plot = p_2005, scale = 0.75, width = 16, height = 8, dpi = 300,
       bg = 'white')

# best-case scenario ----
p_126 <- 
  ggplot() +
  facet_wrap(~ species) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = `SSP 1-2.6`), preds_rel) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL) +
  scale_fill_gradientn('Speed', colours = sunset(5), limits = c(-0.6, 0.6),
                       na.value = '#DDDDDD')

ggsave('figures/bcparf-2022-nanaimo/bc-speeds-2100-126.png',
       plot = p_126, scale = 0.75, width = 16, height = 8, dpi = 300,
       bg = 'white')

# worst-case scenario ----
p_585 <-
  ggplot() +
  facet_wrap(~ species) +
  geom_sf(data = bc_shp) +
  geom_raster(aes(x, y, fill = `SSP 5-8.5`), preds_rel) +
  scale_x_continuous(NULL) +
  scale_y_continuous(NULL) +
  scale_fill_gradientn('Speed', colours = sunset(5), limits = c(-0.6, 0.6),
                       na.value = '#DDDDDD')

ggsave('figures/bcparf-2022-nanaimo/bc-speeds-2100-585.png',
       plot = p_585, scale = 0.75, width = 16, height = 8, dpi = 300,
       bg = 'white')
