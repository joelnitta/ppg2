# Setup ----

library(tidyverse)
library(pteridocat)
library(assertr)
library(dwctaxon)

# Set dwctaxon options
dct_options(
  check_sci_name = FALSE,
  valid_tax_status = "accepted, synonym, ambiguous synonym, variant")

# Define functions
digest_any <- function(...) {
  digest::digest(c(...))
}

make_taxon_id <- function(df, ...) {
  df %>%
    dplyr::select(...) %>%
    purrr::pmap_chr(digest_any) %>%
    substr(1, 12)
}

end_of_next_val <- function(x) {
  c(x[c(2:length(x))] - 1, Inf)
}

genus_from_sp <- function(x, sep = " ") {
  stringr::str_split(x, sep) %>%
    purrr::map_chr(1)
}

epithet_from_sp_single <- function(x, sep = " ") {
  res <- stringr::str_split(x, sep) %>%
    purrr::map_chr(2)
  if (nchar(res) == 1) {
    res <- stringr::str_split(x, sep) %>%
    purrr::map_chr(3)
  }
  res
}

epithet_from_sp <- function(x, sep = " ") {
  purrr::map2_chr(x, sep, ~epithet_from_sp_single(x = .x, sep = .y))
}

# Load raw data ----
wf_raw <- read_delim("working/001.csv", delim = "|") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols")

# Initial cleaning ----

# Initial World Ferns data includes synonyms in a column; all rows are accepted
# names
wf_with_syn <-
  wf_raw %>%
  select(-photo, -orientation, -author) %>%
  # convert taxonRank to full name
  mutate(taxon = case_when(
    taxon == "O" ~ "order",
    taxon == "F" ~ "family",
    taxon == "SF" ~ "subfamily",
    taxon == "T" ~ "tribe",
    taxon == "G" ~ "genus",
    taxon == "S" ~ "species",
    taxon == "FM" ~ "form",
    taxon == "SS" ~ "subspecies",
    taxon == "V" ~ "variety",
    TRUE ~ NA_character_
  )) %>%
  assert(not_na, taxon) %>%
  # fill in "number" for species and below, to enable mapping to genus
  # ! requires same row order as in raw file
  fill(number, .direction = "down") %>%
  rename(
    scientificName = name,
    vernacularName = trivial_name,
    taxonRank = taxon,
    namePublishedIn = literature,
    taxonRemarks = remarks
    ) %>%
  mutate(taxonID = make_taxon_id(., scientificName, namePublishedIn)) %>%
    # all sci names are unique
  assert(is_uniq, scientificName, taxonID)

# Split out synonyms into dataframe with one row per name
wf_syns <-
  wf_with_syn %>%
  filter(!is.na(synonyms)) %>%
  select(taxonRank, number, acceptedNameUsageID = taxonID, synonyms) %>%
  separate_rows(synonyms, sep = "=") %>%
  filter(!synonyms == "") %>%
  mutate(synonyms = str_squish(synonyms)) %>%
  separate(
    synonyms,
    c("scientificName", "namePublishedIn"),
    sep = "\\[",
    remove = TRUE,
    extra = "merge",
    fill = "right") %>%
  mutate(
    scientificName = str_squish(scientificName),
    namePublishedIn = str_remove_all(namePublishedIn, "\\]$"),
  ) %>%
  unique() %>%
  mutate(
    taxonID = make_taxon_id(
      ., acceptedNameUsageID, scientificName, namePublishedIn)
  ) %>%
  add_count(scientificName) %>%
  mutate(
    taxonomicStatus = case_when(
      n == 1 ~ "synonym",
      n > 1 ~ "ambiguous synonym"
    )
  ) %>%
  select(-n) %>%
  assert(is_uniq, taxonID)

# Split out non-DWC columns
# include taxonID for joining later
wf_non_dwc <-
wf_with_syn %>%
  select(taxonID, number, distribution, conservation_status)

# Combine accepted and synonyms
# - doesn't incldue parentage mapping yet
wf_dwc_no_parentage <-
wf_with_syn %>%
  select(-synonyms) %>%
  select(any_of(dct_terms$term)) %>%
  mutate(taxonomicStatus = "accepted") %>%
  bind_rows(wf_syns) %>%
  select(any_of(dct_terms$term)) %>%
  dct_validate()

# Account for child-parent taxon relationships ----

# * All higher-level taxa (genus and up) are accepted
rank_by_num <-
wf_non_dwc %>%
  select(number, taxonID) %>%
  left_join(
    select(wf_dwc_no_parentage, taxonID, scientificName, taxonRank),
    by = "taxonID"
  ) %>%
  tidyr::extract(number, "major_num", "(\\d{3})\\.", remove = FALSE) %>%
  mutate(major_num = parse_number(major_num)) %>%
  tidyr::extract(number, "minor_num", "\\.(\\d+)", remove = FALSE) %>%
  mutate(
    minor_num = parse_number(minor_num),
    taxonomicStatus = "accepted"
  )

# Split out each high-level taxon and parse the number system
order_by_num <- filter(rank_by_num, taxonRank == "order") %>%
  arrange(number) %>%
  mutate(
    start = major_num,
    end = end_of_next_val(major_num)
  )

family_by_num <- filter(rank_by_num, taxonRank == "family") %>%
  arrange(number) %>%
  mutate(
    start = minor_num,
    end = minor_num + 999
  )

subfamily_by_num <- filter(rank_by_num, taxonRank == "subfamily") %>%
  arrange(number) %>%
  mutate(
    start = minor_num,
    end = minor_num + 999
  )

tribe_by_num <- filter(rank_by_num, taxonRank == "tribe") %>%
  arrange(number) %>%
  mutate(
    start = minor_num,
    end = minor_num + 99
  )

genus_by_num <- filter(rank_by_num, taxonRank == "genus") %>%
  arrange(number) %>%
  mutate(
    start = minor_num,
    end = minor_num + 999
  )

# Map lower to higher taxa
# (family to order, subfamily to family, genus to subfamily or family)
family_mapped <-
family_by_num %>%
  left_join(
    select(
      order_by_num,
      parentNameUsageID = taxonID,
      parentNameUsage = scientificName,
      start,
      end
    ),
    by = join_by(
      major_num >= start, major_num <= end
    )
  ) %>%
  select(any_of(dct_terms$term)) %>%
  assert(not_na, parentNameUsageID) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

subfamily_mapped <-
subfamily_by_num %>%
  left_join(
    select(
      family_by_num,
      parentNameUsageID = taxonID,
      parentNameUsage = scientificName,
      major_num
    ),
    by = "major_num"
  ) %>%
  select(any_of(dct_terms$term)) %>%
  assert(not_na, parentNameUsageID) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

genus_mapped_to_subfamily <-
genus_by_num %>%
  left_join(
    select(
      subfamily_by_num,
      parentNameUsageID = taxonID,
      parentNameUsage = scientificName,
      major_num,
      start,
      end
    ),
    by = join_by(
      major_num, minor_num >= start, minor_num < end
    )
  ) %>%
  select(any_of(dct_terms$term)) %>%
  filter(!is.na(parentNameUsageID)) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

genus_mapped_to_family <-
genus_by_num %>%
  anti_join(genus_mapped_to_subfamily, by = "taxonID") %>%
  left_join(
    select(
      family_by_num,
      parentNameUsageID = taxonID,
      parentNameUsage = scientificName,
      major_num,
      start,
      end
    ),
    by = join_by(
      major_num, minor_num >= start, minor_num < end
    )
  ) %>%
  select(any_of(dct_terms$term)) %>%
  assert(not_na, parentNameUsageID) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

genus_mapped_to_tribe <-
genus_by_num %>%
  anti_join(genus_mapped_to_subfamily, by = "taxonID") %>%
    anti_join(genus_mapped_to_family, by = "taxonID") %>%
  left_join(
    select(
      tribe_by_num,
      parentNameUsageID = taxonID,
      parentNameUsage = scientificName,
      major_num,
      start,
      end
    ),
    by = join_by(
      major_num, minor_num >= start, minor_num < end
    )
  ) %>%
  select(any_of(dct_terms$term)) %>%
  filter(!is.na(parentNameUsageID)) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

species_mapped_to_genus <-
  rank_by_num %>%
  filter(taxonRank == "species") %>%
  left_join(
    select(
      genus_by_num,
      parentNameUsageID = taxonID,
      parentNameUsage = scientificName,
      number,
    ),
    by = join_by(number)
  ) %>%
  select(any_of(dct_terms$term)) %>%
  assert(not_na, parentNameUsageID) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

infra_mapped_to_species <-
rank_by_num %>%
  filter(taxonRank %in% c("subspecies", "form", "variety")) %>%
  mutate(
    genericName = genus_from_sp(scientificName),
    specificEpithet = epithet_from_sp(scientificName)
    ) %>%
  left_join(
    transmute(
      species_mapped_to_genus,
      parentNameUsageID = taxonID,
      genericName = genus_from_sp(scientificName),
      specificEpithet = epithet_from_sp(scientificName)
    ),
    by = c("genericName", "specificEpithet")
  ) %>%
  select(any_of(dct_terms$term)) %>%
  # TODO not all infraspecific taxa have autonyms, e.g., Spinulum annotinum
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

# Combine the mapped higher taxa
parent_mapping <-
  order_by_num %>%
  select(any_of(dct_terms$term)) %>%
  bind_rows(family_mapped) %>%
  bind_rows(subfamily_mapped) %>%
  bind_rows(genus_mapped_to_subfamily) %>%
  bind_rows(genus_mapped_to_family) %>%
  bind_rows(genus_mapped_to_tribe) %>%
  bind_rows(species_mapped_to_genus) %>%
  bind_rows(infra_mapped_to_species) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE) %>%
  select(taxonID, parentNameUsageID)

wf_dwc <-
  wf_dwc_no_parentage %>%
  left_join(parent_mapping, by = "taxonID")  %>%
  dct_validate()

# Save final data in DWC format ----
write_csv(wf_dwc, "working/wf_dwc.csv")
