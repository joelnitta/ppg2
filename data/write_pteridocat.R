library(pteridocat)
library(tidyverse)
library(assertr)
library(dwctaxon)

# Write out original data
data(pteridocat)

# Some new names lacked `genericName`, but this is needed for joining higher
# taxonomy.
# Add parsed genus names for those that were missing
parsed_names <-
  pteridocat %>%
  filter(is.na(genericName)) %>%
  pull(scientificName) %>%
  unique() %>%
  taxastand::ts_parse_names(docker = TRUE) %>%
  select(
    scientificName = name,
    genericName = genus_name,
    specificEpithet = specific_epithet,
    infraspecificEpithet = infraspecific_epithet
  )

fixed_names <-
  pteridocat %>%
  filter(is.na(genericName)) %>%
  select(-c(genericName, specificEpithet, infraspecificEpithet)) %>%
  left_join(parsed_names, by = "scientificName") %>%
  select(all_of(colnames(pteridocat))) %>%
  assert(not_na, genericName)

pteridocat::pteridocat %>%
  anti_join(fixed_names, by = "taxonID") %>%
  bind_rows(fixed_names) %>%
  arrange(scientificName) %>%
  assert(not_na, genericName) %>%
  dct_validate(check_taxonomic_status = FALSE) %>%
  write.csv(here::here("data/pteridocat.csv"), row.names = FALSE)
