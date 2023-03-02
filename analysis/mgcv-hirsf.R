library('tidyr')   # for data wrangling
library('raster')  # for raster data
library('dplyr')   # for data wrangling
library('mgcv')    # for GAMs
library('ggplot2') # for fancy plots
theme_set(theme_bw()) # change ggplot theme
source('functions/seq_range.R') # custom version of seq()

inv_logit <- function(x) brms::inv_logit_scaled(x, lb = 0, ub = 1)

#' *do we need the scaling constant?*
K <- 1e-6 # scaling constant for weights

# import observed locations ----
all_ones <- mutate(readRDS('data/standardized-speeds.rds'),
                   detected = 1)

# rasters of resources
forest_r <- raster('data/forest.tif')
water_r <- raster('data/distance-from-water.tif')
dem <- raster('data/area-dem-high-res.tif')

# add resources to each observed location
all_ones_sp <- SpatialPoints(select(all_ones, longitude, latitude))
all_ones <- mutate(all_ones,
                   forest_perc = extract(forest_r, all_ones_sp),
                   dist_w_m = extract(water_r, all_ones_sp),
                   elev_m = extract(forest_r, all_ones_sp))

# use all raster cells as unobserved locations ----
all_zeros <-
  as.data.frame(forest_r, xy = TRUE) %>%
  rename(forest_perc = consensus_full_class_1) %>%
  # add water by coordinates
  left_join(as.data.frame(water_r, xy = TRUE) %>%
              rename(dist_w_m = consensus_full_class_12),
            by = c('x', 'y')) %>%
  # project to lowest resolution for consistency
  left_join(projectRaster(dem, forest_r) %>%
              as.data.frame(xy = TRUE) %>%
              rename(elev_m = value),
            by = c('x', 'y')) %>%
  # collapse all data to a single row
  nest(nested = -c())

# too much data to run a single HIRSF for all the species at once (~165 GB)
sp <- all_ones$species[1]
for(sp in unique(all_ones$species)) {
  # subset the telemetry data to only one species
  ones <- filter(all_ones, species == sp) %>%
    mutate(animal = factor(animal))
  
  #' subset the zeros based on the location of the `ones`
  zeros <- filter(all_zeros,
                  longitude > min(ones$longitude) - 1,
                  longitude < max(ones$longitude) + 1,
                  latitude < min(ones$latitude) - 1,
                  longitude > max(ones$longitude) + 1) %>%
    # add all combinations of weather values based on the data subset
    expand_grid(
      animal = unique(ones$animal),
      temp_c = quantile(ones$temp_c, seq(0, 1, by = .1), na.rm = TRUE),
      tp_mm = quantile(ones$tp_mm, seq(0, 1, by = .1), na.rm = TRUE),
      sde_mm = quantile(ones$sde_mm, seq(0, 1, by = .1), na.rm = TRUE)) %>%
    # expand the zeros back to a single tibble
    unnest(nested)
  
  # merge ones and zeros to create the full dataset
  d <- ones %>%
    select(animal, species, timestamp, longitude, latitude, temp_c, tp_mm,
           sde_mm, weight, detected, forest_perc, dist_w_m, elev_m) %>%
    bind_rows(zeros)
  
  # test data ----------------------------------------------------------------
  if(FALSE) {
    set.seed(1)
    d <- tibble(species = rep(1:2, rep(500, 2)),
                forest = rbeta(length(species), shape1 = 5, shape2 = 5) * 100,
                dist_w_m = rexp(length(species), rate = 2),
                elevation = rgamma(length(species), shape = 100, scale = 10),
                temp_c = rnorm(length(species), mean = 0, sd = 7.5),
                tp_mm = rexp(length(species), rate = 1),
                sde_mm = rexp(length(species), rate = 7)) %>%
      group_by(species) %>%
      mutate(p = inv_logit(0.25 * species +  4e-2 * forest - 2 * sqrt(dist_w_m) -
                             3e-6 * (elevation - 1e2)^2 - 3),
             detected = rbinom(n = length(species), size = 1, prob = p),
             weight = if_else(detected == 1, sum(detected) * 0.7 * K, 1),
             species = factor(species)) %>%
      ungroup()
    
    # plot effects of environmental variables
    p0 <-
      d %>%
      select(species, forest_perc, dist_w_m, elev_m, p) %>%
      pivot_longer(-c(species, p)) %>%
      ggplot(aes(value, p)) +
      facet_grid(species ~ name, scales = 'free_x') +
      geom_point(alpha = 0.2) +
      geom_smooth(method = 'gam', formula = y ~ s(x),
                  method.args = list(family = betar())); p0
  }
  
  # exploratory diagnostic plots ----
  # plot the data for a quick check
  d %>%
    select(temp_c, tp_mm, sde_mm, detected, forest_perc, dist_w_m, elev_m) %>%
    tidyr::pivot_longer(- detected) %>%
    ggplot() +
    facet_wrap(~ name, scales = 'free') +
    geom_density(aes(value), fill = NA, linewidth = 1, adjust = 2) +
    khroma::scale_color_bright() +
    theme(legend.position = c(0.8, 0.2)) +
    ggtitle(unique(d$species))
  
  #' `sqrt()` the predictors with long tails
  #' *check which predictors need to be transformed*
  #' *sometimes transformations increase kurtosis*
  d %>%
    select(dist_w_m, sde_mm, tp_mm, detected) %>%
    mutate(.sqrt_dist_w_m = sqrt(dist_w_m),
           .sqrt_sde_mm = sqrt(sde_mm),
           .sqrt_tp_mm = sqrt(tp_mm)) %>%
    tidyr::pivot_longer(-c(detected)) %>%
    ggplot() +
    facet_wrap(~ name, scales = 'free') +
    geom_density(aes(value), fill = NA, linewidth = 1, adjust = 2) +
    khroma::scale_color_bright() +
    theme(legend.position = 'top') +
    ggtitle(unique(d$species))
  
  # visualize the relationships
  d %>%
    select(temp_c, tp_mm, sde_mm, detected, forest_perc, dist_w_m, elev_m,
           weight) %>%
    mutate(dist_w_m = sqrt(dist_w_m),
           sde_mm = sqrt(sde_mm),
           tp_mm = sqrt(tp_mm)) %>%
    tidyr::pivot_longer(-c(detected, weight)) %>%
    ggplot(aes(value, detected)) +
    facet_wrap(~ name, scales = 'free') +
    geom_point(alpha = 0.1) +
    geom_smooth(method = 'gam', formula = y ~ s(x, k = 5),
                method.args = list(family = binomial(link = 'logit'))) +
    khroma::scale_color_bright() +
    khroma::scale_fill_bright()
  
  # fit the Hierarchical integrated Resource Selection Function (HiRSF) ----
  ctrl <- gam.control(nthreads = parallel::detectCores(logical = FALSE) / 2,
                      trace = TRUE)
  
  m <-
    bam(
      detected / K ~ # 1s become 1e6, 0s stay 0
        
        # global marginal effects of resources
        s(forest_perc, k = 5, bs = 'tp') +
        s(sqrt(dist_w_m), k = 5, bs = 'tp') +
        s(elev_m, k = 5, bs = 'tp') +
        
        # species-level marginal effects of resources
        #' *NOTE:* marginals of weather variables don't affect `detected` 
        s(forest_perc, animal, k = 5, bs = 'fs', xt = list(bs = 'tp')) +
        s(sqrt(dist_w_m), animal, k = 5, bs = 'fs', xt = list(bs = 'tp')) +
        s(elev_m, animal, k = 5, bs = 'fs', xt = list(bs = 'tp')) +
        
        # interaction effects of resources and weather
        # to check if temperature affects resource preferences
        ti(forest_perc, temp_c, k = 5, bs = 'tp') +
        ti(sqrt(dist_w_m), temp_c, k = 5, bs = 'tp') +
        ti(elev_m, temp_c, k = 5, bs = 'tp') +
        # to check if precipitation affects resource preferences
        ti(forest_perc, sqrt(tp_mm), k = 5, bs = 'tp') +
        ti(sqrt(dist_w_m), sqrt(tp_mm), k = 5, bs = 'tp') +
        ti(elev_m, sqrt(tp_mm), k = 5, bs = 'tp') +
        # to check if snow depth affects resource preferences
        ti(forest_perc, sqrt(sde_mm), k = 5, bs = 'tp') +
        ti(sqrt(dist_w_m), sqrt(sde_mm), k = 5, bs = 'tp') +
        ti(elev_m, sqrt(sde_mm), k = 5, bs = 'tp'),
      
      family = poisson(link = 'log'),
      data = d,
      weights = weight, # downscale 1s based on sample size and K
      method = 'fREML',
      discrete = TRUE,
      control = ctrl,
      #' *turn shrinkage on?*
      select = FALSE) # allow for model selection
  
  gam.check(m)
  plot(m, pages = 1, scheme = 2, scale = 0)
  summary(m)
  
  tolower(sp) %>%
    stringr::str_replace_all(pattern = '_', replacement = '-') %>%
    stringr::str_replace_all(pattern = ' ', replacement = '-') %>%
    paste0('models/hirsf-', ., '-', Sys.Date(), '.rds') %>%
    saveRDS(m, file = .)
  
  #' *move predictions to a new file (generalized for each model and species)*
  #' *write function to plot the effects of a GAM*
  # make predictions
  newd <- tidyr::expand_grid(forest = seq(0, 100, length.out = 100),
                             dist_w_m = seq_range(d$dist_w_m, n = 100),
                             elevation = seq_range(d$elevation, n = 100),
                             temp_c = 20, # c(-20, -10, 0, 10, 20),
                             tp_mm = 0, # seq(0, max(d$tp_mm), n = 3),
                             sde_mm = 0, # seq(0, max(d$sde_mm), n = 3),
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
}
