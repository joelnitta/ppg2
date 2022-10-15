library(assertr)

# Load data ----

# write_pteridocat.R needs to be run once first to output pteridocat
# to data/

pteridocat <- readr::read_csv(
  here::here("data/pteridocat.csv"),
  col_types = readr::cols(.default = readr::col_character()))

ppgi <- readr::read_csv(
  here::here("data/ppgi_taxonomy_mod.csv"),
  col_types = readr::cols(.default = readr::col_character())) |>
  dplyr::select(-notes)

# Wrangle data ----

# Many old synonyms in pteridocat have no entries for that genus (old genera)
# instead, grab the higher level taxonomy **for the accepted name**
# Output is df with taxonID and higher level taxonomy
higher_tax_for_syns <-
  pteridocat |>
  dplyr::filter(!is.na(acceptedNameUsageID)) |>
  dplyr::select(taxonID_orig = taxonID, acceptedNameUsageID) |>
  dplyr::left_join(
    pteridocat,
    by = c(acceptedNameUsageID = "taxonID")) |>
  assert(not_na, genericName) |>
  dplyr::select(taxonID = taxonID_orig, genericName) |>
  dplyr::left_join(ppgi, by = c(genericName = "genus")) |>
  assert(is_uniq, taxonID) |>
  assert(not_na, class)

# do the same for accepted names
higher_tax_for_acc <-
  pteridocat |>
  dplyr::filter(is.na(acceptedNameUsageID)) |>
  dplyr::select(taxonID, genericName) |>
  dplyr::left_join(ppgi, by = c(genericName = "genus")) |>
  assert(is_uniq, taxonID) |>
  assert(not_na, class)

# combine them
higher_tax <- dplyr::bind_rows(
  higher_tax_for_syns,
  higher_tax_for_acc
) |>
  # 'genus' is PPGII genus (of accepted name),
  # genericName is original genus
  dplyr::rename(genus = genericName) |>
  assert(is_uniq, taxonID) |>
  assert(not_na, class)

# Join higher-level taxonomy to pteridocat
pteridocat_tax <-
  pteridocat |>
  assert(not_na, genericName) |>
  dplyr::left_join(higher_tax, by = "taxonID") |>
  assert(is_uniq, taxonID)

saveRDS(pteridocat_tax, here::here("app/pteridocat_tax.RDS"))
