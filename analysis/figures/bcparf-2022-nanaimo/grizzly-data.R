library('dplyr')     # for data wrangling
library('lubridate') # for smother date wrangling
library('mgcv')      # for Generalized Additive Models
library('ggplot2')   # for fancy plots
library('khroma')    # for colorblind-friendly color palettes
source('analysis/figures/default-ggplot-theme.R') # bold text and no grids

d <- readRDS('data/standardized-speeds.rds') %>%
  filter(species == 'Ursus_arctos_horribilis') %>%
  filter(! is.na(speed_est)) %>% # drop NA speeds
  mutate(animal = factor(animal),
         species = factor(species),
         moving = speed_est > 0.01, # assume 1 cm/s is equal to not moving
         time_of_day = (hour(timestamp) / 24 +
                          minute(timestamp) / 60 / 24 +
                          second(timestamp) / 60 / 60 / 24),
         doy = yday(timestamp))

# remove the infinite speed value
c(inf = sum(is.infinite(d$speed_est)), n = nrow(d))
d <- filter(d, is.finite(speed_est))
c(inf = sum(is.infinite(d$speed_est)), n = nrow(d))

# plot P(movement) ----
(d %>%
   filter(species == 'Ursus_arctos_horribilis') %>%
   select(moving, time_of_day, doy, temp_c, tp_mm) %>%
   mutate(time_of_day = time_of_day * 24,
          tp_mm = tp_mm * 1e3) %>%
   tidyr::pivot_longer(-moving) %>%
   mutate(name = case_when(name == 'doy' ~ 'Day of year',
                           name == 'temp_c' ~ 'Temperature (\U00B0\U0043)',
                           name == 'time_of_day' ~ 'Time of day (hours)',
                           name == 'tp_mm' ~ 'Precipitation (mm)')) %>%
   ggplot() +
   facet_wrap(~ name, scales = 'free_x', strip.position = 'bottom') +
   geom_jitter(aes(value, moving), alpha = 0.5, pch = '.') +
   xlab(NULL) +
   scale_y_discrete(NULL, labels = c('Not moving', 'Moving')) +
   theme(strip.background = element_blank(),
         strip.placement = 'outside')) %>%
  ggsave(filename = 'figures/bcparf-2022-nanaimo/data-moving-grizzly.png',
         width = 6, height = 5, dpi = 300, bg = 'transparent')

# plot speed ----
(d %>%
  filter(species == 'Ursus_arctos_horribilis') %>%
  select(speed_est, time_of_day, doy, temp_c, tp_mm) %>%
  mutate(time_of_day = time_of_day * 24,
         tp_mm = tp_mm * 1e3) %>%
  tidyr::pivot_longer(-speed_est) %>%
  mutate(name = case_when(name == 'doy' ~ 'Day of year',
                          name == 'temp_c' ~ 'Temperature (\U00B0\U0043)',
                          name == 'time_of_day' ~ 'Time of day (hours)',
                          name == 'tp_mm' ~ 'Precipitation (mm)')) %>%
  ggplot() +
  facet_wrap(~ name, scales = 'free_x', strip.position = 'bottom') +
  geom_point(aes(value, speed_est), pch = '.') +
  xlab(NULL) +
  ylab('Movement (m/s)') +
  theme(strip.background = element_blank(),
        strip.placement = 'outside')) %>%
  ggsave(filename = 'figures/bcparf-2022-nanaimo/data-speeds-grizzly.png',
         width = 6, height = 5, dpi = 300, bg = 'transparent')
