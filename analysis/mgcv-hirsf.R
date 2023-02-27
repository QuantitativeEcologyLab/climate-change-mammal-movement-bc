library('tidyr')   # for data wrangling
library('raster')  # for raster data
library('dplyr')   # for data wrangling
library('mgcv')    # for GAMs
library('ggplot2') # for fancy plots
theme_set(theme_bw()) # change ggplot theme
source('functions/seq_range.R') # custom version of seq()

inv_logit <- function(x) brms::inv_logit_scaled(x, lb = 0, ub = 1)

#' *do we need the scaling constant?*
#' *ALL ELK LOCATIONS ARE NA*
K <- 1e-6 # scaling constant for weights

# import observed locations ----
ones <-
  readRDS('data/standardized-speeds.rds') %>%
  mutate(detected = 1,
         #' *!!!*
         weight = 1) # add AKDE weights in speeds weather file # # # # # # #

# rasters of resources
forest_r <- raster('data/forest.tif')
water_r <- raster('data/distance-from-water.tif')
dem <- raster('data/area-dem-high-res.tif')

# add resources to each observed location
ones_sp <- SpatialPoints(select(ones, longitude, latitude))
ones <- mutate(ones,
               forest_perc = extract(forest_r, ones_sp),
               dist_w_m = extract(water_r, ones_sp),
               elev_m = extract(forest_r, ones_sp))

# use all raster cells as unobserved locations ----
zeros <-
  as.data.frame(forest_r, xy = TRUE) %>%
  rename(forest_perc = consensus_full_class_1) %>%
  left_join(as.data.frame(water_r, xy = TRUE) %>%
              rename(dist_w_m = consensus_full_class_12),
            by = c('x', 'y')) %>%
  # project to lowest resolution for consistency
  left_join(projectRaster(dem, forest_r) %>%
              as.data.frame(xy = TRUE) %>%
              rename(elev_m = value),
            by = c('x', 'y')) %>%
  nest(nested = -c()) %>%
  expand_grid(species = unique(ones$species)) %>%
  unnest(nested)

# merge ones and zeros to create the full dataset
d <- bind_rows(ones, zeros)

# test data ----------------------------------------------------------------
set.seed(1)
d <- tibble(species = rep(1:2, rep(500, 2)),
            forest = rbeta(length(species), shape1 = 5, shape2 = 5) * 100,
            dist_w_m = rexp(length(species), rate = 2),
            elevation = rgamma(length(species), shape = 100, scale = 10),
            temp_c = rnorm(length(species), mean = 0, sd = 7.5),
            tp_mm = rexp(length(species), rate = 1),
            snow_d_mm = rexp(length(species), rate = 7)) %>%
  group_by(species) %>%
  mutate(p = inv_logit(0.25 * species +  4e-2 * forest - 2 * dist_w_m -
                         3e-6 * (elevation - 1e2)^2 - 3),
         detected = rbinom(n = length(species), size = 1, prob = p),
         weight = if_else(detected == 1, sum(detected) * 0.7 * K, 1),
         species = factor(species)) %>%
  ungroup()

# exploratory diagnostic plots ----
# plot the data for a quick check
tidyr::pivot_longer(d, -c(detected, species)) %>%
  ggplot() +
  facet_wrap(~ name, scales = 'free') +
  geom_density(aes(value, color = factor(species)), fill = NA,
               linewidth = 1, adjust = 2) +
  khroma::scale_color_bright()

#' `sqrt()` the data with heavy skews?
d %>%
  mutate(dist_w_m = sqrt(dist_w_m),
         snow_d_mm = sqrt(snow_d_mm),
         tp_mm = sqrt(tp_mm)) %>%
  tidyr::pivot_longer(-c(detected, species)) %>%
  ggplot() +
  facet_wrap(~ name, scales = 'free') +
  geom_density(aes(value, color = factor(species)), fill = NA,
               linewidth = 1, adjust = 2) +
  khroma::scale_color_bright()

# visualize the relationships
d %>%
  mutate(dist_w_m = sqrt(dist_w_m),
         snow_d_mm = sqrt(snow_d_mm),
         tp_mm = sqrt(tp_mm)) %>%
  tidyr::pivot_longer(-c(detected, species, weight, p)) %>%
  ggplot(aes(value, detected)) +
  facet_wrap(~ name, scales = 'free') +
  geom_point(alpha = 0.1) +
  geom_smooth(aes(color = species, fill = species), method = 'gam',
              formula = y ~ s(x, k = 5),
              method.args = list(family = binomial(link = 'logit'))) +
  khroma::scale_color_bright() +
  khroma::scale_fill_bright()

# fit the Hierarchical integrated Resource Selection Function (HiRSF) ----
ctrl <- gam.control(nthreads = 1, #parallel::detectCores(logical = FALSE),
                    trace = TRUE)

m <-
  bam(
    detected / K ~ # 1s become 1e6, 0s stay 0
      
      # global marginal effects of resources
      # s(forest, k = 5, bs = 'tp') +
      # s(sqrt(dist_w_m), k = 5, bs = 'tp') +
      # s(elevation, k = 5, bs = 'tp') +
      
      # species-level marginal effects of resources
      #' *NOTE:* marginals of temp, precip, and elev don't affect `detected` 
      s(forest, species, k = 5, bs = 'fs', xt = list(bs = 'tp')) +
      s(sqrt(dist_w_m), species, k = 5, bs = 'fs', xt = list(bs = 'tp')) +
      s(elevation, species, k = 5, bs = 'fs', xt = list(bs = 'tp')),# +
      
      # interaction effects of resources and weather
      # to check if temperature affects resource preferences
      # ti(forest, species, temp_c, k = 5, bs = c('tp', 're', 'tp')) +
      # ti(sqrt(dist_w_m), species, temp_c, k = 5, bs = c('tp', 're', 'tp')) +
      # ti(elevation, species, temp_c, k = 5, bs = c('tp', 're', 'tp')) +
      # # to check if precipitation affects resource preferences
      # ti(forest, species, sqrt(tp_mm), k = 5, bs = c('tp', 're', 'tp')) +
      # ti(sqrt(dist_w_m), species, sqrt(tp_mm), k = 5, bs = c('tp', 're', 'tp')) +
      # ti(elevation, species, sqrt(tp_mm), k = 5, bs = c('tp', 're', 'tp')) +
      # # to check if snow depth affects resource preferences
      # ti(forest, species, sqrt(snow_d_mm), k = 5, bs = c('tp', 're', 'tp')) +
      # ti(sqrt(dist_w_m), species, sqrt(snow_d_mm), k = 5, bs = c('tp', 're', 'tp')) +
      # ti(elevation, species, sqrt(snow_d_mm), k = 5, bs = c('tp', 're', 'tp')),
    
    family = poisson(link = 'log'),
    data = d,
    weights = weight, # downscale 1s based on sample size and K
    method = 'fREML',
    discrete = TRUE,
    control = ctrl,
    select = FALSE) # allow for model selection

layout(matrix(1:4, ncol = 2))
gam.check(m)
layout(1)

summary(m)
plot(m, pages = 1, scheme = 0, scale = 0)

# make predictions
newd <- tidyr::expand_grid(forest = seq(0, 100, length.out = 100),
                           dist_w_m = seq_range(d$dist_w_m, n = 100),
                           elevation = seq_range(d$elevation, n = 100),
                           temp_c = 20, # c(-20, -10, 0, 10, 20),
                           tp_mm = 0, # seq(0, max(d$tp_mm), n = 3),
                           snow_d_mm = 0, # seq(0, max(d$snow_d_mm), n = 3),
                           species = unique(d$species)) %>%
  slice(1:1e4)
  # group_by(species) %>%
  # filter(exclude.too.far(...))

preds <- bind_cols(newd,
                   mu = predict(m, newdata = newd, type = 'link',
                                se.fit = TRUE) %>%
                     data.frame()) %>%
  mutate(mu = exp(fit),
         lwr = exp(fit - 1.96 * se.fit),
         upr = exp(fit + 1.96 * se.fit))

ggplot(preds, aes(elevation, dist_w_m, fill = mu)) +
  facet_wrap(~ species) +
  geom_raster() +
  khroma::scale_fill_acton(name = expression(lambda), reverse = TRUE) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
