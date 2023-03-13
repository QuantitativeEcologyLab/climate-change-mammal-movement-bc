library('tidyr')   # for data wrangling
library('sf')      # for spatial features (e.g. polygons)
library('raster')  # for raster data
library('ctmm')    # for movement modeling
library('dplyr')   # for data wrangling
library('mgcv')    # for GAMs
library('ggplot2') # for fancy plots
theme_set(theme_bw()) # change ggplot theme
source('functions/seq_range.R') # custom version of seq()
sf_use_s2(use_s2 = FALSE) # because of overlapping borders in some polygons

K <- 1e-6 # scaling constant for weights

for(SPECIES in c('Oreamnos_americanus', 'Puma_concolor', 'Rangifer_tarandus',
                 'Ursus_arctos_horribilis', 'Cervus elaphus')[3:5]) {
  
  cat(paste0('Fitting model for species ', SPECIES, '...\n'))
  
  # import observed locations ----
  ones <-
    readRDS('data/standardized-speeds.rds') %>%
    filter(species == SPECIES) %>%
    mutate(detected = 1)
  
  #' elk have a sample size 133 times greater than other species and an
  #' effective sample size 90 times greater than other species, but this results
  #' in an excessively large number of zeros needed to fit the model. Therefore
  #' we can drop some of the ones to ensure the number of zeros is large enough.
  #' Since the movement models were fit with constant weights (i.e., assuming
  #' constant sampling frequency), the weight is simply `1/n * n_eff`. Thus,
  #' we can re-scale the weights as `w_1 = w_0 * (n_0/n_1)` since keeping the
  #' effective sample size constant implies `n_eff = w_1 * n_1 = w_0 * n_0`.
  #' But note that the AKDEs used to determine the area are the ones fit to the
  #' full dataset.
  if(SPECIES == 'Cervus elaphus') {
    ones <- ones %>%
      # drop observations per each animal to ensure the new weights are correct
      group_by(animal) %>%
      mutate(n_0 = n()) %>% # find current sample size
      slice(seq(1, n(), by = 4)) %>% # take one row every 4
      mutate(n_1 = n(), # find new sample size
             c = n_0 / n_1, # find scaling constant based on change in n
             weight_1 = weight * c, # scale new weights accordingly
             check = weight_1 / weight) %>% # ensure the new weights are right
      # drop variables used to check accuracy
      select(-c(n_0, n_1, c, weight, check)) %>%
      # rename the weight column
      rename(weight = weight_1) %>%
      # remove grouping by animal
      ungroup()
  }
  
  # check effective sample size
  sum(ones$weight) / K
  
  # import AKDEs to filter the spatial area
  akdes <- readRDS('models/movement-models.rds') %>%
    filter(species == SPECIES) %>%
    pull(ud) %>%
    purrr::map(\(.ud) {
      SpatialPolygonsDataFrame.UD(.ud, level.UD = 0.9995, level = 0.9995) %>% 
        st_as_sf() %>%
        st_transform(crs = '+proj=longlat')
    }) %>%
    bind_rows() %>%
    summarise_all(.funs = identity) %>%
    #' to fix `Evaluation error: TopologyException: side location conflict`
    st_make_valid() %>%
    st_union() %>%
    st_buffer(dist = 0.25) %>% # units are degrees; accuracy isn't crucial here
    st_as_sf()
  
  plot(akdes)
  
  # rasters of resources, cropped to the area in which the animals are
  forest_r <- raster('data/forest.tif') %>%
    crop(akdes) %>%
    mask(akdes)
  water_r <- raster('data/distance-from-water.tif') %>%
    crop(akdes) %>%
    mask(akdes)
  dem <- raster('data/area-dem-high-res.tif') %>%
    crop(akdes) %>%
    mask(akdes)
  raster::plot(forest_r, main = SPECIES)
  
  # add resources to each observed location
  ones_sp <- SpatialPoints(select(ones, longitude, latitude))
  sp::plot(ones_sp, add = TRUE)
  ones <- mutate(ones,
                 forest_perc = extract(forest_r, ones_sp),
                 dist_w_m = extract(water_r, ones_sp),
                 elev_m = extract(dem, ones_sp))
  
  if(any(is.na(ones$forest_perc), is.na(ones$dist_w_m), is.na(ones$elev_m))) {
    stop('Some locations are outside the cropped resource rasters.')
  }
  
  # use all raster cells as unobserved locations ----
  d <-
    as.data.frame(forest_r, xy = TRUE) %>%
    rename(forest_perc = consensus_full_class_1) %>%
    left_join(as.data.frame(water_r, xy = TRUE) %>%
                rename(dist_w_m = consensus_full_class_12),
              by = c('x', 'y')) %>%
    # project to lowest resolution for consistency
    left_join(projectRaster(dem, forest_r) %>%
                as.data.frame(xy = TRUE) %>%
                rename(elev_m = layer) %>%
                filter(! is.na(elev_m)), # drop NAs on the border
              by = c('x', 'y')) %>%
    filter(! is.na(forest_perc)) %>%
    #' keep location column names consistent with `ones`
    rename(longitude = x, latitude = y)
  
  # add rows if tibble is too small
  if(nrow(d) / nrow(ones) < 2e3) {
    MULT <- round((2e3 * nrow(ones) / nrow(d))^(1/3))
    
    warning(paste0('Added ', (MULT - 1) * nrow(d),
                   ' rows because nrow(d) / nrow(ones) < 2e3.'))
    
    # values for weather variables in quadrature points
    STEPS <- seq(0.1, 0.9, length.out = MULT)
    TEMPS <- quantile(ones$temp_c, STEPS, na.rm = TRUE)
    TPS <- quantile(ones$tp_mm, STEPS, na.rm = TRUE)
    SDES <- quantile(ones$sde_mm, STEPS, na.rm = TRUE)
    
    d <- d %>%
      nest(dat = -c()) %>%
      expand_grid(species = SPECIES,
                  temp_c = TEMPS,
                  tp_mm = TPS,
                  sde_mm = SDES) %>%
      unnest(dat)
  } else { # otherwise add 10 values for each weather variable
    # values for weather variables in quadrature points
    STEPS <- seq(0.1, 0.9, length.out = 10)
    TEMPS <- quantile(ones$temp_c, STEPS, na.rm = TRUE)
    TPS <- quantile(ones$tp_mm, STEPS, na.rm = TRUE)
    SDES <- quantile(ones$sde_mm, STEPS, na.rm = TRUE)
    
    d <- mutate(d,
                species = SPECIES,
                temp_c = sample(x = TEMPS, size = n(), replace = TRUE),
                tp_mm = sample(x = TPS, size = n(), replace = TRUE),
                sde_mm = sample(x = SDES, size = n(), replace = TRUE))
  }
  
  d <- d %>%
    mutate(detected = 0,
           weight = 1) %>%
    # merge ones and zeros to create the full dataset
    #' *NOTE:* `bind_rows()` pairs columns appropriately, `bind.rows()` does not
    bind_rows(select(ones, -c(animal, dataset_name, timestamp,
                              speed_low, speed_est, speed_high)))
  rm(ones)
  
  # exploratory diagnostic plots ----
  if(FALSE) {
    # plot the data for a quick check
    tidyr::pivot_longer(d,
                        c(forest_perc, dist_w_m, elev_m, temp_c, tp_mm, sde_mm)) %>%
      ggplot() +
      facet_wrap(~ detected + name, scales = 'free') +
      geom_density(aes(value, color = factor(species)), fill = NA,
                   linewidth = 1, adjust = 3) +
      khroma::scale_color_bright()
    
    #' `sqrt()` the data with heavy skews
    d %>%
      select(detected, species, dist_w_m, sde_mm, tp_mm) %>%
      mutate(.sqrt_dist_w_m = sqrt(dist_w_m),
             .sqrt_sde_mm = sqrt(sde_mm),
             .sqrt_tp_mm = sqrt(tp_mm)) %>%
      tidyr::pivot_longer(- c(species, detected)) %>%
      ggplot() +
      facet_wrap(~ detected + name, scales = 'free', nrow = 2) +
      geom_density(aes(value, color = factor(species)), fill = NA,
                   linewidth = 1, adjust = 2) +
      khroma::scale_color_bright()
    
    # visualize the relationships
    d %>%
      mutate(dist_w_m = sqrt(dist_w_m),
             sde_mm = sqrt(sde_mm),
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
  }
  
  # fit the Hierarchical Resource Selection Function (HIRSF) ----
  m <-
    bam(
      detected / K ~ # 1s become 1e6, 0s stay 0
        
        # global marginal effects of resources
        #' *NOTE:* marginals of temp, precip, and elev don't affect `detected` 
        s(forest_perc, k = 5, bs = 'tp') +
        s(sqrt(dist_w_m), k = 5, bs = 'tp') +
        s(elev_m, k = 5, bs = 'tp') +
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
      control = gam.control(nthreads = 10, trace = TRUE))
  
  if(FALSE) {
    # manual model checks
    layout(matrix(1:4, ncol = 2))
    gam.check(m)
    layout(1)
    
    summary(m)
    plot(m, pages = 1, scheme = 2, scale = 0)
  }
  
  # save the model so it can be used later without re-fitting it
  stringr::str_replace_all(SPECIES, '_', '-') %>%
    stringr::str_replace_all(' ', '-') %>%
    paste0('models/hrsf-', ., '-', Sys.Date(), '-quarter-of-dataset.rds') %>%
    saveRDS(m, .)
  rm(m, d)
}
