library('dplyr') # for data wrangling (mutate(), %>%, etc.)
library('tidyr') # for data wrangling (nest(), unnest(), pivot_*, etc.)
library('purrr') # for functional programming (map_***(), etc.)
library('ctmm')  # for movement models
source('functions/import_rda.R') # to convert Rda files to rds files

# bind all the Rda objects into a single tibble
d <-
  bind_rows(
    import_rda('data/tracking-data/Oreamnos_americanus.Rda') %>%
      as_tibble() %>% # to print only first 10 rows by default
      select(-X) %>% # remove row name
      mutate(animal = individual.local.identifier, # to add a column of ID
             species = 'Oreamnos_americanus',
             dataset_name = 'Oreamnos_americanus') %>% # some have > 1 data set
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    import_rda('data/tracking-data/Puma_concolor_2.Rda') %>%
      as_tibble() %>%
      arrange(AnimalID, timestamp) %>% # some data out of order
      mutate(animal = as.character(AnimalID),
             species = 'Puma_concolor',
             dataset_name = 'Puma_concolor_2') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    import_rda('data/tracking-data/Puma_concolor_4.Rda') %>%
      as_tibble() %>%
      mutate(animal = individual.local.identifier,
             species = 'Puma_concolor',
             dataset_name = 'Puma_concolor_4') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    import_rda('data/tracking-data/Rangifer_tarandus.Rda',
               object_name = 'data') %>%
      as_tibble() %>%
      select(- 'Validated') %>% # causes: error in `[<-`:! subscript out of bounds
      mutate(animal = AnimalID,
             species = 'Rangifer_tarandus',
             dataset_name = 'Rangifer_tarandus') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, \(x) as.telemetry(x, keep = TRUE))),
    import_rda('data/tracking-data/Ursus_arctos_horribilis.Rda') %>%
      as_tibble() %>%
      mutate(animal = as.character(AnimalID),
             species = 'Ursus_arctos_horribilis',
             dataset_name = 'Ursus_arctos_horribilis') %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)),
    # Elk data: doi:10.5441/001/1.j484vk24
    read.csv('data/tracking-data/Elk in southwestern Alberta.csv') %>%
      rename(species = individual.taxon.canonical.name,
             dataset_name = study.name) %>%
      mutate(animal = as.character(individual.local.identifier)) %>%
      # remove duplicates
      group_by(animal) %>%
      filter(! (duplicated(paste(animal, timestamp)))) %>%
      ungroup() %>%
      nest(tel = -c(animal, species, dataset_name)) %>%
      mutate(tel = map(tel, as.telemetry)))

saveRDS(d, 'data/tracking-data/full-dataset.rds')
