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

# habitat quality within BC parks
d <- tibble(species = c('Oreamnos_americanus', 'Puma_concolor',
                        'Rangifer_tarandus', 'Ursus_arctos_horribilis',
                        'Cervus elaphus'),
            hq = map(c('data/habitat-quality-change-oreamnos-americanus.rds',
                       'data/habitat-quality-change-puma-concolor.rds',
                       'data/habitat-quality-change-rangifer-tarandus.rds',
                       'data/habitat-quality-change-ursus-arctos-horribilis.rds',
                       'data/habitat-quality-change-cervus-elaphus.rds'),
                     \(.fn) select(readRDS(.fn), scenario, x, y, mu) %>%
                       pivot_wider(names_from = scenario, values_from = mu)),
            # `2020` = map(hq, \(.d) .d %>%
            #                select(x, y, `2020`) %>%
            #                rast() %>%
            #                mask(parks) %>%
            #                as.data.frame()),
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
  unnest(c(`2100 SSP 1-2.6`:`2100 SSP 5-8.5`)) %>%
  pivot_longer(- species, values_to = 'mu', names_to = 'scenario') %>%
  filter(! is.na(mu)) %>%
  mutate(species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'))

d %>%
  mutate(mu = if_else(mu > 4, 4, mu),
         mu = if_else(mu < 0.25, 0.25, mu)) %>%
  ggplot() +
  facet_wrap(. ~ species, scales = 'fixed') +
  geom_vline(xintercept = 1, color = 'grey') +
  geom_density(aes(mu, color = scenario, fill = scenario), alpha = 0.1,
               linewidth = 1, na.rm = TRUE, bw = 0.25) +
  scale_x_continuous(
    expression(bold(Change~'in'~habitat~quality~within~BC~parks~(log[2]))),
    limits = c(0.25, 4), trans = 'log2') +
  scale_y_continuous('Density') +
  scale_color_manual(
    name = 'Scenario', aesthetics = c('color', 'fill'),
    values = c('black', '#0571b0', '#92c5de', '#f4a582', '#ca0020')[-1]) +
  theme(legend.position = c(0.85, 0.2))

# d %>%
#   group_by(species, scenario) %>%
#   summarise(good = mean(mu >= 1),
#             .groups = 'drop') %>%
#   group_by(species) %>%
#   mutate(good = good / first(good)) %>%
#   ggplot() +
#   facet_wrap(. ~ species, scales = 'fixed') +
#   geom_hline(yintercept = 1, color = 'grey') +
#   geom_segment(aes(scenario, xend = scenario, y = 1, yend = good,
#                    color = scenario)) +
#   geom_point(aes(scenario, y = good, color = scenario, fill = scenario)) +
#   scale_x_discrete(NULL, breaks = NULL) +
#   scale_y_continuous('Relative change in good habitat') +
#   scale_color_manual(
#     name = 'Scenario', aesthetics = c('color', 'fill'),
#     values = c('black', '#0571b0', '#92c5de', '#f4a582', '#ca0020')) +
#     theme(legend.position = c(0.85, 0.2))

ggsave('figures/habitat-quality-densities-within-parks.png',
       width = 10, height = 5, dpi = 600, bg = 'transparent')
