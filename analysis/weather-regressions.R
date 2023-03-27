library('dplyr')     # for data wrangling
library('lubridate') # for smother date wrangling
library('mgcv')      # for Generalized Additive Models
library('ggplot2')   # for fancy plots
library('khroma')    # for colorblind-friendly color palettes
source('analysis/figures/default-ggplot-theme.R') # bold text and no grids

pal <- c('#66CCEE', '#ff8c00', '#4477AA', '#009900', '#CCBB44')
khroma::plot_scheme(pal)

FIG_W <- 10
FIG_H <- 5

# can't add speed weights based on 95% CI because some uppers are Inf
d <- readRDS('data/standardized-speeds.rds') %>%
  filter(! is.na(speed_est)) %>% # drop NA speeds
  mutate(animal = factor(animal),
         species = factor(species),
         moving = speed_est > 0.01, # assume 1 cm/s is equal to not moving
         #' *CHANGE TIMES TO PST FROM UTC* - - - - - - - - - - - - - - - - - - -
         time_of_day = (hour(timestamp) / 24 +
                          minute(timestamp) / 60 / 24 +
                          second(timestamp) / 60 / 60 / 24),
         doy = yday(timestamp))

# drop values with NA weather values (6 rasters fail to download)
d %>%
  filter(is.na(temp_c), is.na(tp_mm), is.na(sde_mm)) %>%
  pull(timestamp) %>%
  round('hours') %>%
  unique()

d <- filter(d, ! is.na(temp_c), ! is.na(tp_mm), ! is.na(sde_mm))

# remove the infinite speed value(s)
c(inf = sum(is.infinite(d$speed_est)), n = nrow(d))
d <- filter(d, is.finite(speed_est))
c(inf = sum(is.infinite(d$speed_est)), n = nrow(d))

# change lowest value to exp(-14)
# d <- mutate(d, tp_mm = if_else(tp_mm < exp(-14), exp(-14), tp_mm))
# hist(d$tp_mm, breaks = 250, ylim = c(0, 2e5))
# hist(sqrt(d$tp_mm), breaks = 50)

# model P(movement) ----
if(FALSE) {
  m_1 <-
    bam(moving ~
          # to account for changes in behavior within days
          s(time_of_day, species, k = 5, bs = 'fs', xt = list(bs = 'cc')) +
          # to account for changes in behavior within years
          s(doy, species, k = 10, bs = 'fs', xt = list(bs = 'cc')) +
          # no common effect of precipitation or snow depth
          temp_c +
          # species-level effect of temperature
          s(temp_c, species, k = 10, bs = 'fs') +
          # species-level effect of precipitation
          s(sqrt(tp_mm), species, k = 5, bs = 'fs') +
          # species-level effect of snow depth
          s(sqrt(sde_mm), species, k = 5, bs = 'fs') +
          # how each species reacts to temperature:precip interaction
          ti(temp_c, sqrt(tp_mm), species, bs = c('tp', 'tp', 're')) +
          # how each species reacts to temperature:sde interaction
          ti(temp_c, sqrt(sde_mm), species, bs = c('tp', 'tp', 're')) +
          # how each species reacts to temperature:precip interaction
          ti(sqrt(tp_mm), sqrt(sde_mm), species, bs = c('tp', 'tp', 're')),
        family = binomial(link = 'logit'),
        data = d,
        method = 'fREML', # fast REML
        discrete = TRUE,  # discretize the posterior for faster computation
        select = TRUE, # perform model shrinkage
        knots = list(time_of_day = c(0, 1), doy = c(0.5, 366.5)),# for bs = 'cc'
        # use multiple threads, print progress (excessive threads slow it down)
        control = gam.control(nthreads = 4, trace = TRUE))
  saveRDS(m_1, paste0('models/binomial-gam-', Sys.Date(), '.rds'))
} else {
  m_1 <- readRDS('models/binomial-gam-2023-03-03.rds')
}

#' *the temporal terms could be problematic because they are related to weather*
#' check `time_of_day` term
newd_tod <- expand.grid(species = unique(d$species),
                        time_of_day = seq(0, 1, by = 0.001),
                        doy = 0,
                        temp_c = 0,
                        tp_mm = 0,
                        sde_mm = 0)
bind_cols(newd_tod,
          predict(m_1, newdata = newd_tod, terms = c('s(time_of_day,species)'),
                  type = 'link', se.fit = TRUE)) %>%
  mutate(mu = m_1$family$linkinv(fit),
         lwr = m_1$family$linkinv(fit - 1.96 * se.fit),
         upr = m_1$family$linkinv(fit + 1.96 * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(time_of_day, ymin = lwr, ymax = upr, fill = species),
              alpha = 0.3) +
  geom_line(aes(time_of_day, mu, color = species)) +
  scale_color_bright(name = 'Species') +
  scale_fill_bright(name = 'Species') +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ylab('P(moving)')

#' check `doy` term
newd_doy <- expand.grid(species = unique(d$species),
                        time_of_day = 12,
                        doy = 1:366,
                        temp_c = 0,
                        tp_mm = 0,
                        sde_mm = 0)
bind_cols(newd_doy,
          predict(m_1, newdata = newd_doy, terms = c('s(doy,species)'),
                  type = 'link', se.fit = TRUE)) %>%
  mutate(mu = m_1$family$linkinv(fit),
         lwr = m_1$family$linkinv(fit - 1.96 * se.fit),
         upr = m_1$family$linkinv(fit + 1.96 * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(doy, ymin = lwr, ymax = upr, fill = species), alpha = 0.3) +
  geom_line(aes(doy, mu, color = species)) +
  scale_color_bright(name = 'Species') +
  scale_fill_bright(name = 'Species') +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ylab('P(moving)')

#' check `sde_mm` term
newd_sde <- expand.grid(species = unique(d$species),
                        time_of_day = 12,
                        doy = 0,
                        temp_c = 0,
                        tp_mm = 0,
                        sde_mm = seq(min(d$sde_mm), quantile(d$sde_mm, 0.95),
                                     length.out = 400))
bind_cols(newd_sde,
          predict(m_1, newdata = newd_sde, terms = c('s(sqrt(sde_mm),species)'),
                  type = 'link', se.fit = TRUE)) %>%
  mutate(mu = m_1$family$linkinv(fit),
         lwr = m_1$family$linkinv(fit - 1.96 * se.fit),
         upr = m_1$family$linkinv(fit + 1.96 * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(sde_mm, ymin = lwr, ymax = upr, fill = species), alpha = 0.3) +
  geom_line(aes(sde_mm, mu, color = species), linewidth =  1) +
  scale_color_bright(name = 'Species') +
  scale_fill_bright(name = 'Species') +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  ylab('P(moving)')

plot(m_1, pages = 1, scheme = 1, se = TRUE, trans = m_1$family$linkinv)
summary(m_1)

#' pick values for`tp_mm`: 0, average, heavy precip
precips <-
  c(min(d$tp_mm), # no precip
    mean(filter(d, tp_mm > 0)$tp_mm), # mean if > 0
    quantile(filter(d, tp_mm > 0)$tp_mm, 0.95)) # heavy precip
# plot precips and maximum for reference
plot(c(precips, max(d$tp_mm)), type = 'b', pch = 19, col = c(rep(4, 3), 2))

#' plot effect of `temp_c`
newd_temp_c <- expand.grid(species = unique(d$species),
                           temp_c = seq(-20, 25, by = 0.1),
                           time_of_day = 12,
                           doy = 0,
                           tp_mm = 0,
                           sde_mm = 0)
bind_cols(newd_temp_c,
          predict(m_1, newdata = newd_temp_c,
                  terms = c('(Intercept)', 's(temp_c,species)', 'temp_c'),
                  type = 'link', se.fit = TRUE)) %>%
  mutate(mu = m_1$family$linkinv(fit),
         lwr = m_1$family$linkinv(fit - 1.96 * se.fit),
         upr = m_1$family$linkinv(fit + 1.96 * se.fit),
  species =
  case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
            species == 'Puma_concolor' ~ 'Cougar',
            species == 'Rangifer_tarandus' ~ 'Caribou',
            species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
            species == 'Cervus elaphus' ~ 'Elk')) %>%
  ggplot() +
  geom_ribbon(aes(temp_c, ymin = lwr, ymax = upr, fill = species), alpha = .2) +
  geom_line(aes(temp_c, mu, color = species), linewidth =  1) +
  scale_color_muted(name = 'Species') +
  scale_fill_muted(name = 'Species') +
  scale_x_continuous(paste0('Temperature (', '\U00B0', 'C)'), expand = c(0, 0))+
  scale_y_continuous('P(moving)', expand = c(0, 0), limits = c(0, 1)) +
  theme(legend.position = c(0.85, 0.85))

ggsave('figures/p-moving-temperature.png', width = 6, height = 6, dpi = 600)

# predictions for mid june ----
newd <-
  expand.grid(
    # a few discrete time points
    time_of_day = c(0, 6, 12, 18) / 24,
    # presentation day
    doy = yday(date('2022-06-15')),
    # gradient of temperature because it changes smoothly over time
    temp_c = gratia:::seq_min_max(d$temp_c, n = 250),
    # only some precipitation values because precipitation happens suddenly
    tp_mm = precips,
    # no snow
    sde_mm = 0,
    # include all species
    species = unique(d$species))

preds_m1 <- bind_cols(newd,
                      predict(m_1, newdata = newd, se.fit = TRUE,
                              )) %>%
  mutate(mu = m_1$family$linkinv(fit),
         lwr = m_1$family$linkinv(fit - 1.96 * se.fit),
         upr = m_1$family$linkinv(fit + 1.96 * se.fit),
         time_of_day = paste0(time_of_day * 24, ':00') %>%
           as.POSIXct(format = '%H:%M') %>%
           format('%H:%M'),
         tp_mm = case_when(tp_mm == precips[1] ~ 'No precip.',
                           tp_mm == precips[2] ~ 'Average precip.',
                           tp_mm == precips[3] ~ 'Heavy precip.') %>%
           factor(levels = c('No precip.', 'Average precip.', 'Heavy precip.')),
         species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'))

ggplot(preds_m1) +
  facet_grid(tp_mm ~ time_of_day) +
  geom_line(aes(temp_c, mu, color = species), linewidth = 1) +
  labs(x = 'Temperature (\U00B0\U0043)', y = 'P(moving)', title = 'No snow!') +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  scale_color_bright(name = 'Species')

# model mean speed given that animals are moving ----
if(FALSE) {
  m_2 <-
    bam(speed_est ~
          # to account for changes in behavior within days
          s(time_of_day, species, k = 10, bs = 'fs', xt = list(bs = 'cc')) +
          # to account for changes in behavior within years
          s(doy, species, k = 10, bs = 'fs', xt = list(bs = 'cc')) +
          # no common effect of temperature, precipitation, or snow depth
          # species-level effect of temperature
          s(temp_c, species, k = 10, bs = 'fs') +
          # species-level effect of precipitation
          s(sqrt(tp_mm), species, k = 5, bs = 'fs') +
          # species-level effect of snow depth
          s(sqrt(sde_mm), species, k = 5, bs = 'fs') +
          # how each species reacts to temperature:precip interaction
          ti(temp_c, sqrt(tp_mm), species, bs = c('tp', 'tp', 're')) +
          # how each species reacts to temperature:sde interaction
          ti(temp_c, sqrt(sde_mm), species, bs = c('tp', 'tp', 're')) +
          # how each species reacts to temperature:precip interaction
          ti(sqrt(tp_mm), sqrt(sde_mm), species, bs = c('tp', 'tp', 're')),
        family = Gamma(link = 'log'), # can use Gamma because no zeros
        data = d,
        subset = moving, # only model animals that are moving
        method = 'fREML', # fast REML
        discrete = TRUE,  # discretize the posterior for faster computation
        knots = list(time_of_day = c(0, 1), doy = c(0.5, 366.5)),
        control = gam.control(nthreads = 4, # excessive threads slows it down 
                              trace = TRUE))
  
  saveRDS(m_2, paste0('models/gamma-gam-', Sys.Date(), '.rds'))
  
  m_2_ls <-
    gam(list(
      speed_est ~
        # to account for changes in behavior within days
        s(time_of_day, species, k = 10, bs = 'fs', xt = list(bs = 'cc')) +
        # to account for changes in behavior within years
        s(doy, species, k = 10, bs = 'fs', xt = list(bs = 'cc')) +
        # no common effect of temperature, precipitation, or snow depth
        # species-level effect of temperature
        s(temp_c, species, k = 10, bs = 'fs') +
        # species-level effect of precipitation
        s(sqrt(tp_mm), species, k = 5, bs = 'fs') +
        # species-level effect of snow depth
        s(sqrt(sde_mm), species, k = 5, bs = 'fs') +
        # how each species reacts to temperature:precip interaction
        ti(temp_c, sqrt(tp_mm), species, bs = c('tp', 'tp', 're')) +
        # how each species reacts to temperature:sde interaction
        ti(temp_c, sqrt(sde_mm), species, bs = c('tp', 'tp', 're')) +
        # how each species reacts to temperature:precip interaction
        ti(sqrt(tp_mm), sqrt(sde_mm), species, bs = c('tp', 'tp', 're')),
      # scale term
      ~ s(species, bs = 're') +
        # to account for changes in behavior within years
        s(doy, species, k = 10, bs = 'fs', xt = list(bs = 'cc'))),
      
      family = gammals(), # location-scale Gamma
      data = d,
      subset = moving, # only model animals that are moving
      method = 'REML', # fast REML
      knots = list(time_of_day = c(0, 1), doy = c(0.5, 366.5)),
      control = gam.control(nthreads = 4, trace = TRUE))
  
  saveRDS(m_2_ls, paste0('models/gammals-gam-', Sys.Date(), '.rds'))
} else {
  m_2 <- readRDS('models/gamma-gam-2023-02-27.rds')
  m_2_ls <- readRDS('models/gammals-gam-2023-03-03.rds')
}

# check predicted vs fitted
tibble(speed = m_2$model$speed_est,
       mu = m_2$fitted.values[, 1],
       species = m_2$model$species) %>%
  group_by(species) %>%
  summarise(speed = mean(speed), mu = mean(mu)) %>%
  ggplot() +
  facet_wrap(~ species) +
  geom_point(aes(speed, mu, col = species), d_2, alpha = 0.5) +
  geom_point(aes(speed, mu), size = 17) +
  geom_point(aes(speed, mu, col = species), size = 15) +
  scale_color_bright(name = 'Species') +
  geom_abline(slope = 1, intercept = 0) +
  lims(x = c(0, 0.5), y = c(0, 0.5)) +
  labs(x = 'Observed speeds', y = 'Estimated speeds')

#' *plot the sum of X random points against X times the mean to check for long-term prediction accuracy*

#' check `time_of_day` term
bind_cols(newd_tod,
          predict(m_2, newdata = newd_tod, terms = c('s(time_of_day,species)'),
                  type = 'link', se.fit = TRUE) %>%
            as.data.frame() %>%
            rename(fit = fit.1, se.fit = se.fit.1)) %>%
  mutate(mu = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(time_of_day, ymin = lwr, ymax = upr, fill = species),
              alpha = 0.3) +
  geom_line(aes(time_of_day, mu, color = species), linewidth =  1) +
  scale_color_bright(name = 'Species') +
  scale_fill_bright(name = 'Species') +
  ylab('Speed | Moving')

#' check `doy` term
bind_cols(newd_doy,
          predict(m_2, newdata = newd_doy, terms = c('s(doy,species)'),
                  type = 'link', se.fit = TRUE) %>%
            as.data.frame() %>%
            rename(fit = fit.1, se.fit = se.fit.1)) %>%
  mutate(mu = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(doy, ymin = lwr, ymax = upr, fill = species), alpha = 0.3) +
  geom_line(aes(doy, mu, color = species)) +
  scale_color_bright(name = 'Species') +
  scale_fill_bright(name = 'Species') +
  ylim(c(0, NA)) +
  ylab('Speed | Moving')

#' check `sde_mm` term
bind_cols(newd_sde,
          predict(m_2, newdata = newd_sde, terms = c('s(sqrt(sde_mm),species)'),
                  type = 'link', se.fit = TRUE) %>%
            as.data.frame() %>%
            rename(fit = fit.1, se.fit = se.fit.1)) %>%
  mutate(mu = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit)) %>%
  ggplot() +
  geom_ribbon(aes(sde_mm, ymin = lwr, ymax = upr, fill = species), alpha = 0.3) +
  geom_line(aes(sde_mm, mu, color = species), linewidth = 1) +
  scale_color_bright(name = 'Species') +
  scale_fill_bright(name = 'Species') +
  ylab('Speed | Moving')

plot(m_2, pages = 1, scheme = 1) # cannot drop s(precip, species) bc of ti()

layout(matrix(1:4, ncol = 2))
gam.check(m_2)
layout(1)

summary(m_2)

# predictions
preds_m2 <- bind_cols(newd,
                      predict(m_2, newdata = newd, se.fit = TRUE)) %>%
  mutate(mu = m_2$family$linkinv(fit),
         lwr = m_2$family$linkinv(fit - 1.96 * se.fit),
         upr = m_2$family$linkinv(fit + 1.96 * se.fit),
         time_of_day = paste0(time_of_day * 24, ':00') %>%
           as.POSIXct(format = '%H:%M') %>%
           format('%H:%M'),
         tp_mm = case_when(tp_mm == precips[1] ~ 'No precip.',
                           tp_mm == precips[2] ~ 'Average precip.',
                           tp_mm == precips[3] ~ 'Heavy precip.') %>%
           factor(levels = c('No precip.', 'Average precip.', 'Heavy precip.')),
         species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'))

# grizzly only ----
preds_m2 %>%
  filter(species == 'Grizzly bear') %>%
  ggplot() +
  facet_grid(tp_mm ~ time_of_day) +
  geom_line(aes(temp_c, mu), linewidth = 1, color = pal[3]) +
  labs(x = 'Temperature (\U00B0\U0043)', y = 'Estimated speed (m/s)') +
  ylim(c(0, NA))

ggsave(filename = 'figures/bcparf-2022-nanaimo/speeds-grizzly.png',
       width = FIG_W, height = FIG_H, dpi = 300, bg = 'transparent',
       scale = 0.75)

# all species ----
ggplot(preds_m2) +
  facet_grid(tp_mm ~ time_of_day) +
  geom_line(aes(temp_c, mu, color = species), linewidth = 1) +
  scale_color_manual('Species', values = pal) +
  labs(x = 'Temperature (\U00B0\U0043)', y = 'Estimated speed (m/s)') +
  ylim(c(0, NA))

ggsave(filename = 'figures/bcparf-2022-nanaimo/speeds-all.png',
       width = FIG_W, height = FIG_H, dpi = 300, bg = 'transparent',
       scale = 0.75)

# fit a location-scale Gamma GAM ----
# m <- gam(list(
#   # predictor for the mean
#   speed_est ~
#     s(temp_c) +
#     s(sqrt(tp_mm)) +
#     ti(temp_c, sqrt(tp_mm)) +
#     s(temp_c, species, bs = 'fs') +
#     s(sqrt(tp_mm), species, bs = 'fs') +
#     ti(temp_c, sqrt(tp_mm), species, bs = c('tp', 'tp', 're')) +
#     s(temp_c, animal, bs = 'fs') +
#     s(sqrt(tp_mm), animal, bs = 'fs'),
#   # predictor for the scale parameter
#   ~
#     s(temp_c) +
#     s(sqrt(tp_mm)) +
#     s(temp_c, species, bs = 'fs') +
#     s(sqrt(tp_mm), species, bs = 'fs')),
#   family = Gamma(link = 'log'),
#   data = d,
#   method = 'REML',
#   control = gam.control(nthreads = 8)) # do not hyper-thread!
