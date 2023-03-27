library('tidyverse')
library('mgcv')

models <-
  tibble(species = c('O. americanus', 'P. concolor',
                     'R. tarandus', 'U. arctos horribilis',
                     'C. elaphus'),
         file_name = c('models/hrsf-Oreamnos-americanus-2023-03-07.rds',
                       'models/hrsf-Puma-concolor-2023-03-07.rds',
                       'models/hrsf-Rangifer-tarandus-2023-03-07.rds',
                       'models/hrsf-Ursus-arctos-horribilis-2023-03-08.rds',
                       'models/hrsf-Cervus-elaphus-2023-03-14-tenth-of-dataset.rds'),
         model = map(file_name, readRDS))

models <- models %>%
  mutate(intercept = map_dbl(model, \(.m) coef(.m)['(Intercept)']),
         ones = map_dbl(model, \(.m) filter(.m$model, `detected/K` > 0) %>%
                          nrow()),
         ones_w = map_dbl(model, \(.m) filter(.m$model, `detected/K` > 0) %>%
                            pull(`(weights)`) %>%
                            sum()),
         ones_w = ones_w / 1e-6, # remove scaling constant
         zeros = map_dbl(model, \(.m) filter(.m$model, `detected/K` == 0) %>%
                           nrow()),
         ratio = zeros/ones,
         ratio_w = zeros/ones_w)

models %>%
  select(species, ones, ones_w, zeros, ratio, ratio_w) %>%
  arrange(species)
  
# all models have a range of 0-100% forest cover
models %>%
  transmute(forest_range = map_chr(model,
                                   \(.m) filter(.m$model, `detected/K` > 0) %>%
                                     pull(forest_perc) %>%
                                     range() %>%
                                     paste0(collapse = ' - ')),
            w_dist_range = map_chr(model,
                                   \(.m) filter(.m$model, `detected/K` > 0) %>%
                                     pull(`sqrt(dist_w_m)`) %>%
                                     range() %>%
                                     paste0(collapse = ' - ')),
            elev_m = map_chr(model,
                             \(.m) filter(.m$model, `detected/K` > 0) %>%
                               pull(`elev_m`) %>%
                               range() %>%
                               paste0(collapse = ' - ')))

# check temporal ranges
readRDS('data/tracking-data/full-dataset.rds') %>%
  mutate(tel = map(tel, data.frame)) %>%
  unnest(tel) %>%
  mutate(day = as.Date(timestamp)) %>%
  group_by(species) %>%
  summarise(t_range = round(max(timestamp) - min(timestamp)),
            days = n_distinct(day))
