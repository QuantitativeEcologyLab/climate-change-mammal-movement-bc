library('ctmm')    # for movement modeling
library('sf')      # for spatial data
library('dplyr')   # for data wrangling
library('ggplot2') # for fancy plots
library('khroma')  # for colorblind-friendly palettes
source('analysis/figures/default-ggplot-theme.R') # for consistent theme
source('analysis/figures/bc-shapefile.R') # import BC shapefile

# create a color palette
muted <- color('muted')
plot_scheme(muted(5), colours = TRUE, names = TRUE, size = 0.9)

akdes <- readRDS('models/movement-models.rds') %>%
  pull(ud) %>%
  purrr::map(\(.ud) {
    SpatialPolygonsDataFrame.UD(.ud, level.UD = 0.95, level = 0) %>% 
      st_as_sf() %>%
      st_transform(crs = st_crs(bc_shp))
  }) %>%
  bind_rows() %>%
  filter(grepl('95% est', name)) %>%
  bind_cols(species = readRDS('models/movement-models.rds')$species) %>%
  mutate(species =
           case_when(species == 'Oreamnos_americanus' ~ 'Mountain goat',
                     species == 'Puma_concolor' ~ 'Cougar',
                     species == 'Rangifer_tarandus' ~ 'Caribou',
                     species == 'Ursus_arctos_horribilis' ~ 'Grizzly bear',
                     species == 'Cervus elaphus' ~ 'Elk'))

ggplot() +
  geom_sf(data = bc_shp) +
  geom_sf(aes(color = species, fill = species), akdes, alpha = 0.3) +
  scale_color_muted(name = 'Species') +
  scale_fill_muted(name = 'Species') +
  theme(legend.position = c(0.8, 0.8))

ggsave('figures/bc-95-uds.png', width = 6, height = 6, dpi = 600, bg = 'white',
       device = 'png', type = 'cairo') # so degree symbols show up correctly
