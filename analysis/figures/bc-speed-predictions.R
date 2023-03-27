library('dplyr')     # for data wrangling
library('tidyr')     # for data wrangling
library('purrr')     # for functional programming
library('mgcv')      # for generalized additive models
library('ggplot2')   # for fancy figures
library('khroma')    # for colorblind-friendly palettes
library('stringr')   # for working with strings
library('rphylopic') # for animal silhouettes to plots
library('cowplot')   # to add phylopic once (not in each facet)
source('analysis/figures/default-ggplot-theme.R') # for consistent theme
source('analysis/figures/bc-shapefile.R') # import shapefile of bc

#' *NEED TO USE BC PROJECTION*

# import climate data ----
d <- bind_rows(
  # 2020 historical climate data (monthly averages)
  readr::read_csv('climate-na/bc-dem/bc-dem_2020.csv') %>%
    mutate(scenario = '2020'),
  # 2100 climate projections
  readr::read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp126@2100.csv') %>%
    mutate(scenario = 'SSP 1-2.6'),
  readr::read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp245@2100.csv') %>%
    mutate(scenario = 'SSP 2-4.5'),
  readr::read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp370@2100.csv') %>%
    mutate(scenario = 'SSP 3-7.0'),
  readr::read_csv('climate-na/bc-dem/8GCMs_ensemble_ssp585@2100.csv') %>%
    mutate(scenario = 'SSP 5-8.5')) %>%
  select(scenario, Longitude, Latitude, Tave01:Tave12, PPT01:PPT12) %>%
  pivot_longer(cols = c(Tave01:Tave12, PPT01:PPT12)) %>%
  mutate(month = substr(name, nchar(name) - 1, nchar(name)) %>%
           as.numeric(),
         variable = case_when(str_detect(name, pattern = 'Tave') ~ 'temp_c',
                              str_detect(name, pattern = 'PPT') ~ 'tp_mm'),
         n_days = lubridate::days_in_month(month),
         doy = if_else(grepl('2020', scenario), '2020-', '2100-') %>%
           paste0(if_else(month < 10, paste0('0', month), as.character(month)),
                  '-', as.character(round(n_days / 2))) %>%
           as.Date() %>%
           lubridate::yday()) %>%
  select(- c(name)) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  transmute(scenario,
            month,
            doy,
            n_days,
            longitude = Longitude,
            latitude = Latitude,
            temp_c = if_else(temp_c == -9999, NA_real_, as.numeric(temp_c)),
            tp_mm = if_else(tp_mm == -9999, NA_real_, as.numeric(tp_mm)),
            # convert from mm/month to mm/hour
            tp_mm = tp_mm / (n_days * 24),
            sde_mm = 0)

# remove NAs
d <- filter(d, ! is.na(temp_c), ! is.na(tp_mm))

# check data
filter(d, scenario == '2020') %>%
  ggplot() +
  facet_wrap(~ scenario + month) +
  geom_raster(aes(longitude, latitude, fill = temp_c)) +
  scale_fill_sunset(name = paste0('Temperature \U00B0', 'C')) +
  theme(legend.position = c(0.85, 0.2), axis.title = element_blank())

filter(d, scenario == '2020') %>%
  group_by(longitude, latitude) %>%
  summarize(tp_mm = mean(tp_mm)) %>%
  mutate(tp_mm = tp_mm * 24 * 365) %>%
  ggplot() +
  geom_raster(aes(longitude, latitude, fill = tp_mm)) +
  scale_fill_continuous('Yearly precipitation (mm, estimate)') +
  theme(legend.position = c(0.85, 0.2), axis.title = element_blank())

# import models ----
m_1 <- readRDS('models/binomial-gam-2023-03-03.rds')
m_2 <- readRDS('models/gamma-gam-2023-02-27.rds')

# predict speeds ----
# create new data for predictions
newd <- tibble(time_of_day = 0.5,
               species = unique(m_1$model$species)) %>%
  mutate(data = list(d)) %>%
  unnest(data)

preds <-
  mutate(newd,
         p_moving =
           predict(m_1, newdata = newd, type = 'response',
                   exclude = c('s(time_of_day,species)',
                               's(sqrt(sde_mm),species)',
                               'ti(temp_c,sqrt(sde_mm),species)',
                               'ti(sqrt(tp_mm),sqrt(sde_mm),species)')),
         speed =
           predict(m_2, newdata = newd, type = 'response',
                   exclude = c('s(time_of_day,species)',
                               's(sqrt(sde_mm),species)',
                               'ti(temp_c,sqrt(sde_mm),species)',
                               'ti(sqrt(tp_mm),sqrt(sde_mm),species)')),
         # convert to overall average speed (accounting for P(moving))
         displ = p_moving * speed, # m/s
         species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'),
         displ = speed / 1e3 * n_days * 24 * 60 * 60) %>% # monthly displacement
  group_by(species, scenario, longitude, latitude) %>%
  summarise(p_moving = sum(p_moving * n_days) / 365, # no units
            speed = sum(speed * n_days) / 365, # m/s
            displ = sum(displ) / 365, # km/day
            .groups = 'drop')

# check overall trends
preds %>%
  select(species, scenario, longitude, latitude, displ) %>%
  pivot_wider(names_from = scenario, values_from = displ) %>%
  pivot_longer(cols = -c(species, longitude, latitude, `2020`),
               names_to = 'scenario', values_to = 'displ') %>%
  mutate(change = displ / `2020`) %>%
  group_by(species) %>%
  summarise(mean_change = mean(change),
            sd_change = sd(change))

# check changes with density functions
preds %>%
  select(species, scenario, longitude, latitude, displ) %>%
  pivot_wider(names_from = scenario, values_from = displ) %>%
  pivot_longer(cols = -c(species, longitude, latitude, `2020`),
               names_to = 'scenario', values_to = 'displ') %>%
  mutate(change = displ / `2020`) %>%
  ggplot() +
  facet_wrap(~ species, scales = 'free_y') +
  geom_vline(xintercept = 1, color = 'grey') +
  geom_density(aes(change, fill = scenario, color = scenario), alpha = 0.3) +
  scale_color_manual('Scenario', values = khroma::color('sunset')(4),
                     aesthetics = c('color', 'fill')) +
  xlab('Change in speed relative to 2020')

# figure of estimated speeds for each species ----
for(SPECIES in unique(preds$species)) {
  cat('Making figure for ', SPECIES, '...\n', sep = '')
  
  # get phylopic for the species
  pic <- case_when(
    SPECIES == 'Elk' ~ 'cc03f5c2-933f-4c40-9c64-7f8727556fdb',
    SPECIES == 'Mountain goat' ~ 'd6b7ba72-a51b-47ce-a1c9-d8489483ea4c',
    SPECIES == 'Cougar' ~ 'ba8012a3-cfc9-4403-b4bb-1959988766ca',
    SPECIES == 'Caribou' ~ 'e6e864fd-8e3d-435f-9db3-dc6869c589f1',
    SPECIES == 'Grizzly bear' ~ '0cd82109-bb1c-4e08-ab11-c845d8a82eba') %>%
    get_phylopic()
  
  pic <- ggplot() +
    add_phylopic(pic) +
    theme(panel.border = element_blank())
  
  preds_sp <-
    preds %>%
    filter(species == SPECIES)
  # # cap displacements at 97.5% quantile
  #   mutate(displ = if_else(displ > round(quantile(displ, 0.975), 4),
  #                          round(quantile(displ, 0.975), 4), displ)) %>%
  #   # cap displacements at 2.5% quantile
  #   mutate(displ = if_else(displ < round(quantile(displ, 0.025), 4),
  #                          round(quantile(displ, 0.025), 4), displ))
  
  
  plt_0 <-
    ggplot() +
    facet_wrap(~ scenario) +
    geom_sf(data = st_transform(bc_shp, crs = '+proj=latlong')) +
    geom_raster(aes(longitude, latitude, fill = displ), preds_sp) +
    scale_fill_iridescent(name = 'Distance travelled (km/day)') +
    labs(x = NULL, y = NULL) +
    coord_sf(xlim = c(-138.5, NA)) +
    theme(legend.position = 'none')
  
  if(SPECIES == 'Caribou') {
    bounds <-
      read_sf('data/species-ranges-shapefiles/caribou/Herd_Boundaries.shp') %>%
      st_geometry() %>%
      st_transform(crs = '+proj=longlat') %>%
      st_as_sf() %>%
      st_make_valid()
    
      plt_0 <- plt_0 + geom_sf(data = bounds, color = 'black', fill = NA) +
        coord_sf(xlim = c(-138.5, NA))
  }
  
  plt <-
    ggdraw(plt_0) +
    # add the phylopic
    draw_plot(pic,
              x = case_when(SPECIES == 'Cougar' ~ 0.8,
                            SPECIES == 'Grizzly bear' ~ 0.8,
                            SPECIES == 'Mountain goat' ~ 0.8,
                            TRUE ~ 0.75),
              y = 0.05,
              width = if_else(SPECIES %in% c('Elk', 'Caribou'), 0.3, 0.2),
              height = if_else(SPECIES %in% c('Elk', 'Caribou'), 0.3, 0.2)) +
    # add the legend over the phylopic
    draw_grob(get_legend(plt_0 + theme(legend.position = 'right',
                                       legend.background = element_blank())),
              x = 0.35, y = -0.15)
  
  #' edit `SPECIES` appropriately for a file name
  SPECIES <- tolower(SPECIES) %>%
    str_replace_all(pattern = ' ', replacement = '-')
  
  ggsave(paste0('figures/bc-speeds-', SPECIES, '.png'),
         plot = plt, scale = 0.75, width = 13, height = 8, dpi = 300,
         bg = 'white', device = 'png', type = 'cairo')
}
