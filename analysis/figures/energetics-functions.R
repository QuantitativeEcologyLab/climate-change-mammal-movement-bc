library('dplyr')     # for data wrangling
library('ggplot2')   # for fancy plots
library('rphylopic') # for animal silhouettes
source('analysis/figures/default-ggplot-theme.R') # bold text and no grids

theme_set(theme_get() +
            theme(legend.position = 'none',
                  # axis.ticks = element_blank(),
                  # axis.text = element_blank(),
                  text = element_text(face = 'bold', size = 16)))

save_plt <- function(file_name, plot = last_plot(), width = 7, height = 7) {
  if(! grepl('climate-change-animal-movement', getwd())) {
    setwd('H:/GitHub/climate-change-animal-movement')
  }
  ggsave(filename = file_name,
         plot = plot,
         path = 'figures/bcparf-2022-nanaimo',
         scale = 0.75,
         width = width,
         height = height,
         dpi = 300,
         bg = 'transparent')
}

# pics and masses of 3 animals ----
hare <- image_data('f69eb95b-3d0d-491d-9a7f-acddd419afed', size = '512')[[1]]
cervus <- image_data('dbf886d8-42f3-4d7b-8a93-45d4b3e72f9c', size = '512')[[1]]
bear <- image_data('5a5dafa2-6388-43b8-a15a-4fd21cd17594', size = '512')[[1]]

masses <- c(4, 70, 220)

#  metabolic rate as a function of mass and temperature -----
# Brown et al. 2004 ( https://doi.org/10.1890/03-9000)
mr_m_t <- function(M, temperature) {
  I_0 <- 1
  E <- 5e-25
  k <- 1.380649e-23
  I_0 * M^(3/4) * exp(-E/k * temperature)
}

d1 <- expand.grid(mass = masses,
                  temp = seq(-30, 40, by = 0.5) + 273) %>%
  mutate(metabolic_rate = mr_m_t(mass, temp))

ggplot(d1) +
  geom_line(aes(temp, metabolic_rate, group = mass),
            linewidth = 2, lineend = 'round') +
  add_phylopic(img = bear, alpha = 1, ysize = 7, col = 'black',
               x = min(d1$temp) + 10,
               y = quantile(d1$metabolic_rate, 0.993)) +
  add_phylopic(img = cervus, alpha = 1, ysize = 10, col = 'black',
               x = min(d1$temp) + 1,
               y = quantile(d1$metabolic_rate, 0.915)) +
  add_phylopic(img = hare, alpha = 1, ysize = 5, col = 'black',
               x = min(d1$temp),
               y = quantile(d1$metabolic_rate, 0.55)) +
  scale_x_continuous('Temperature', breaks = range(d1$temp),
                     labels = c('Low', 'High')) +
  scale_y_continuous('Energetic cost', breaks = range(d1$metabolic_rate),
                     labels = c('Low', 'High'))

save_plt('metabolic-rate-mass-temperature.png')

# cost of movement ----
# Taylor et al. 1982 (https://doi.org/10.1242/jeb.97.1.1)
mov_cost <- function(M, speed) {
  (10.7 * M^(-0.316) * speed + 6.03 * M^(-0.303)) * M
}

d2 <- expand.grid(mass = masses, speed = seq(0.1, 10, by = 0.05)) %>%
  mutate(movement_cost = mov_cost(mass, speed),
         speed_l = if_else(speed == 0.1, 'Slow', 'Fast'))

ggplot(d2) +
  geom_line(aes(speed, movement_cost, group = mass),
            linewidth = 2, lineend = 'round') +
  add_phylopic(img = bear, alpha = 1, ysize = 500, col = 'black',
               x = quantile(d2$speed, 0.77),
               y = quantile(d2$movement_cost, 0.975)) +
  add_phylopic(img = cervus, alpha = 1, ysize = 730, col = 'black',
               x = quantile(d2$speed, 0.9),
               y = quantile(d2$movement_cost, 0.83)) +
  add_phylopic(img = hare, alpha = 1, ysize = 365, col = 'black',
               x = quantile(d2$speed, 0.95),
               y = quantile(d2$movement_cost, 0.45)) +
  scale_x_continuous('Movement speed', breaks = range(d2$speed),
                     labels = c('Low', 'High')) +
  scale_y_continuous('Energetic cost', breaks = range(d2$movement_cost),
                     labels = c('Low', 'High'),
                     limits = c(NA, max(d2$movement_cost)) * 1.05)

save_plt('movement-cost-speed.png')

# https://doi.org/10.1038/s41559-017-0241-4 ----
v_max_m <- function(M) { # mass in kg
  25.5 * M^0.26 * (1 - exp(-22*(M)^(-0.66)))
}

d3 <- tibble(mass = 10^seq(-8, 4, length.out = 250),
            v_max = v_max_m(mass))

ggplot() +
  geom_line(aes(log10(mass), log10(v_max)), d3, linewidth = 2) +
  geom_point(aes(log10(masses), log10(v_max_m(masses))), color = 'red',
             size = 3) +
  add_phylopic(img = bear, alpha = 1, ysize = 1, col = 'black',
               x = log10(masses[3]) + 1,
               y = max(log10(d3$v_max))) +
  add_phylopic(img = cervus, alpha = 1, ysize = 1.46, col = 'black',
               x = log10(masses[2]),
               y = max(log10(d3$v_max)) * 1.11) +
  add_phylopic(img = hare, alpha = 1, ysize = 0.73, col = 'black',
               x = log10(masses[1]) - 0.2,
               y = quantile(log10(d3$v_max), 0.9)) +
  scale_x_continuous(expression(log[10](body~mass))) +
  scale_y_continuous(expression(log[10](max~speed)),
                     limits = c(min(log10(d3$v_max)),
                                max(log10(d3$v_max)) * 1.15))

save_plt('v_max-mass.png')

# empty figure w objective of the study ----
ggplot() +
  geom_text(aes(0, 0, label = '?'), size = 100) +
  labs(x = 'Temperature', y = 'Movement speed')

save_plt('objective-plot.png')
