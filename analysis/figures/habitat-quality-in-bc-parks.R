library('canadianmaps') # for shapefile of BC
library('sf')           # for spatial features data
library('dplyr')        # for data wrangling (e.g., mutate, %>%, ...)
library('tidyr')        # for data wrangling (e.g., pivot_***(), ...)
library('purrr')        # for functional programming
library('ggplot2')      # for fancy plots
library('khroma')       # for colorblind-friendly figures
library('terra')        # for raster data
source('analysis/figures/default-ggplot-theme.R')
sf_use_s2(FALSE)

# shapefile of British Columbia
bc_shp <-
  st_as_sf(PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  st_geometry() %>% # extract boundaries only
  st_as_sf() # back to sf format for cropping rasters

parks <- read_sf('data/bc-parks-shapefile/TA_PEP_SVW_polygon.shp') %>%
  st_geometry() %>%
  st_as_sf()

plot(bc_shp)
plot(parks, col = '#228b2250', border = 'forestgreen', add = TRUE)

d <- tibble(species = c('Oreamnos_americanus', 'Puma_concolor',
                        'Rangifer_tarandus', 'Ursus_arctos_horribilis',
                        'Cervus elaphus'),
            hq = map(c('data/habitat-quality-oreamnos-americanus.rds',
                       'data/habitat-quality-puma-concolor.rds',
                       'data/habitat-quality-rangifer-tarandus.rds',
                       'data/habitat-quality-ursus-arctos-horribilis.rds',
                       'data/habitat-quality-cervus-elaphus.rds'),
                     \(.fn) select(readRDS(.fn), scenario, x, y, mu) %>%
                       pivot_wider(names_from = scenario, values_from = mu)),
            `2020` = map(hq, \(.d) .d %>%
                           select(x, y, `2020`) %>%
                           rast() %>%
                           mask(parks) %>%
                           as.data.frame()),
            `2100 SSP 1-2.6` = map(hq, \(.d) .d %>%
                                     select(x, y, `2100 SSP 1-2.6`) %>%
                                     rast() %>%
                                     mask(parks) %>% 
                                     as.data.frame()),
            `2100 SSP 2-4.5` = map(hq, \(.d) .d %>%
                                     select(x, y, `2100 SSP 2-4.5`) %>%
                                     rast() %>%
                                     mask(parks) %>% 
                                     as.data.frame()),
            `2100 SSP 3-7.0` = map(hq, \(.d) .d %>%
                                     select(x, y, `2100 SSP 3-7.0`) %>%
                                     rast() %>%
                                     mask(parks) %>% 
                                     as.data.frame()),
            `2100 SSP 5-8.5` = map(hq, \(.d) .d %>%
                                     select(x, y, `2100 SSP 5-8.5`) %>%
                                     rast() %>%
                                     mask(parks) %>% 
                                     as.data.frame())) %>%
  select(-hq) %>% # drop habitat quality tibble
  unnest(c(`2020`:`2100 SSP 5-8.5`)) %>%
  pivot_longer(- species, values_to = 'mu', names_to = 'scenario') %>%
  filter(! is.na(mu)) %>%
  mutate(species = stringr::str_replace_all(species, '_', ' '))

ggplot(d) +
  facet_wrap(. ~ species, scales = 'free_y') +
  geom_density(aes(mu, color = scenario), adjust = 2, linewidth = 1) +
  scale_x_continuous(expression(bold(Habitat~quality~(log[2]))),
                     limits = c(0.25, 4), trans = 'log2') +
  ylab('Density') +
  scale_color_manual(
    'Scenario', values = c('black', '#0571b0', '#92c5de', '#f4a582', '#ca0020'),
    aesthetics = c('color', 'fill')) +
  theme(legend.position = c(0.85, 0.2))

ggsave('figures/habitat-quality-densities.png',
       width = 10, height = 5, dpi = 600, bg = 'transparent', scale = 0.75)
