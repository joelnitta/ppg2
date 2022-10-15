
# Load data ----

pteridocat <- read_csv(
  here::here("data_prep/pteridocat.csv"),
  col_types = cols(.default = col_character()))

ppgi <- read_csv(
  here::here("data_prep/ppgi_taxonomy_mod.csv"),
  col_types = cols(.default = col_character())) %>%
  select(-notes)

# Wrangle data ----

# Many old synonyms in pteridocat have no entries for that genus (old genera)
# instead, grab the higher level taxonomy **for the accepted name**
# Output is df with taxonID and higher level taxonomy
higher_tax_for_syns <-
  pteridocat %>%
  filter(!is.na(acceptedNameUsageID)) %>%
  select(taxonID_orig = taxonID, acceptedNameUsageID) %>%
  left_join(
    pteridocat,
    by = c(acceptedNameUsageID = "taxonID")) %>%
  assert(not_na, genericName) %>%
  select(taxonID = taxonID_orig, genericName) %>%
  left_join(ppgi, by = c(genericName = "genus")) %>%
  assert(is_uniq, taxonID) %>%
  assert(not_na, class)

# do the same for accepted names
higher_tax_for_acc <-
  pteridocat %>%
  filter(is.na(acceptedNameUsageID)) %>%
  select(taxonID, genericName) %>%
  left_join(ppgi, by = c(genericName = "genus")) %>%
  assert(is_uniq, taxonID) %>%
  assert(not_na, class)

# combine them
higher_tax <- bind_rows(
  higher_tax_for_syns,
  higher_tax_for_acc
) %>%
  # 'genus' is PPGII genus (of accepted name),
  # genericName is original genus
  rename(genus = genericName) %>%
  assert(is_uniq, taxonID) %>%
  assert(not_na, class)

# Join higher-level taxonomy to pteridocat
pteridocat_tax <-
  pteridocat %>%
  assert(not_na, genericName) %>%
  left_join(higher_tax, by = "taxonID") %>%
  assert(is_uniq, taxonID)
