library('sp')      # for working with spatial data
library('sf')      # for working with spatial data
library('raster')  # for working with raster data (tifs)
library('dplyr')   # for data wrangling (e.g., mutate, %>%, ...)
library('ggplot2') # for fancy plots
source('analysis/figures/default-ggplot-theme.R')

theme_set(theme_get() +
            theme(axis.text = element_blank(),
                  axis.ticks = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank(),
                  legend.position = c(0.9, 0.7)))

# import shapefile of bc
# download a shapefile of British Columbia
bc_shp <-
  sf::st_as_sf(canadianmaps::PROV) %>% # convert to spatial features (sf) object
  filter(PRENAME == 'British Columbia') %>% # filter to BC only
  sf::st_geometry() # extract boundaries only

# create custom color palettes for each predictor
forest_pal <- colorRampPalette(c('#C2B280', 'darkgreen'))(100)
elevation_pal <- colorRampPalette(c('white', 'brown4'))(100)
water_pal <- c('transparent', 'blue')

f <- raster('data/bc-forest.tif') %>%
  as.data.frame(xy = TRUE) %>%
  filter(! is.na(layer))
e <- raster('data/bc-dem.tif') %>%
  as.data.frame(xy = TRUE) %>%
  filter(layer > 0)
w <- raster('data/bc-distance-from-water-coarse.tif') %>%
  as.data.frame(xy = TRUE) %>%
  rename(layer = consensus_full_class_12) %>%
  filter(! is.na(layer))

p <-
  cowplot::plot_grid(
    ggplot() +
      geom_raster(aes(x, y, fill = layer), f) +
      geom_sf(data = bc_shp, fill = NA, color = 'black') +
      scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
      scale_y_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
      scale_fill_gradient('Tree cover (%)', low = 'white', na.value = NA,
                          high = 'darkgreen', breaks = c(0, 100)),
    ggplot() +
      geom_raster(aes(x, y, fill = layer), e) +
      geom_sf(data = bc_shp, fill = NA, color = 'black') +
      scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
      scale_y_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
      scale_fill_distiller('Elevation (m)', palette = 6, direction = 1,
                           breaks = round(range(e$layer, na.rm = TRUE)),
                           labels = round(range(e$layer, na.rm = TRUE), -2),
                           limits = round(range(e$layer, na.rm = TRUE))),
    ggplot() +
      geom_raster(aes(x, y, fill = layer / 1e3), w) +
      geom_sf(data = bc_shp, fill = NA, color = 'black') +
      scale_x_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
      scale_y_continuous(NULL, breaks = NULL, expand = c(0, 0)) +
      scale_fill_distiller(expression(bold(atop(Distance~from,
                                                water~(km)~phantom(om)))),
                           na.value = NA, values = c(0, 0.05, 1),
                           breaks = range(w$layer) / 1e3,
                           labels = round(range(w$layer) / 1e3)),
    nrow = 1, align = 'h')

ggsave('figures/bcparf-2022-nanaimo/bc-resources.png', plot = p,
       width = 16, height = 7, dpi = 300, bg = 'transparent')
