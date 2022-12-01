# Convert World Ferns CSV file to Darwin Core CSV file

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

is_uniq_2 <- function(...) {
  is_uniq(..., allow.na = TRUE)
}

# Load raw data ----
wf_raw <- read_delim("data_raw/001.csv", delim = "|") %>%
  janitor::clean_names() %>%
  janitor::remove_empty("cols") %>%
  # Remove two duplicated names
  filter(
    !name %in% c(
      "Tectaria fungii S.Y.Dong",
      "Davallia seramensis (M. Kato) comb. ined.")
  )

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

# Split out non-DWC columns (only includes accepted names)
# include taxonID for joining later
wf_non_dwc <-
wf_with_syn %>%
  select(taxonID, number, distribution, conservation_status)

# Save non-DWC data -----
# (distribution, conservation status)
write_csv(wf_non_dwc, "data/wf_non_dwc.csv")

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

# tribe maps to subfamily
# (only have 5 tribes, all in Microsoroideae)
tribe_mapped <-
  tribe_by_num %>%
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
  assert(not_na, parentNameUsageID) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

genus_mapped_to_tribe <-
genus_by_num %>%
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

genus_mapped_to_subfamily <-
  genus_by_num %>%
  # tribe is immediately above genus, so exclude those with tribe
  anti_join(genus_mapped_to_tribe, by = "taxonID") %>%
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
  anti_join(genus_mapped_to_tribe, by = "taxonID") %>%
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

species_mapped <-
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

# Make tibble of accepted species for looking up parent taxon of
# infrasp taxa
acc_sp <-
  rank_by_num %>%
  filter(taxonRank == "species") %>%
  mutate(
    genericName = genus_from_sp(scientificName),
    specificEpithet = epithet_from_sp(scientificName)
    ) %>%
  # need to match on genus + epithet, so make sure that combination is unique
  assert_rows(col_concat, is_uniq, genericName, specificEpithet)

infrasp_mapped <-
rank_by_num %>%
  filter(taxonRank %in% c("subspecies", "form", "variety")) %>%
  mutate(
    genericName = genus_from_sp(scientificName),
    specificEpithet = epithet_from_sp(scientificName)
    ) %>%
  left_join(
    transmute(
      acc_sp,
      parentNameUsageID = taxonID,
      genericName,
      specificEpithet
    ),
    by = c("genericName", "specificEpithet")
  ) %>%
  select(any_of(dct_terms$term)) %>%
  filter(!is.na(parentNameUsageID)) %>%
  # TODO not all infraspecific taxa have autonyms, e.g., Spinulum annotinum
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE)

# Combine the mapped higher taxa
parent_mapping <-
  order_by_num %>%
  select(any_of(dct_terms$term)) %>%
  bind_rows(family_mapped) %>%
  bind_rows(subfamily_mapped) %>%
  bind_rows(tribe_mapped) %>%
  bind_rows(genus_mapped_to_subfamily) %>%
  bind_rows(genus_mapped_to_family) %>%
  bind_rows(genus_mapped_to_tribe) %>%
  bind_rows(species_mapped) %>%
  bind_rows(infrasp_mapped) %>%
  dct_validate(check_mapping = FALSE, check_mapping_strict = FALSE) %>%
  select(taxonID, parentNameUsageID) %>%
  filter(!is.na(parentNameUsageID))

#
wf_dwc_no_higher_tax <-
  wf_dwc_no_parentage %>%
  # add parent usage for accepted names
  left_join(
    rename(parent_mapping, parentNameUsageID_1 = parentNameUsageID),
    by = "taxonID") %>%
  # add parent usage for synonyms (where parent is that of the accepted name)
  left_join(
    rename(parent_mapping, parentNameUsageID_2 = parentNameUsageID),
    by = c(acceptedNameUsageID = "taxonID")
  ) %>%
  mutate(
    parentNameUsageID = coalesce(parentNameUsageID_1, parentNameUsageID_2)) %>%
  select(-parentNameUsageID_1, -parentNameUsageID_2) %>%
  dct_validate()

wf_dwc_acc_only_no_higher_tax <-
  wf_dwc_no_higher_tax %>%
  filter(taxonomicStatus == "accepted")

# Add higher level taxa columns
higher_taxa <-
wf_dwc_acc_only_no_higher_tax %>%
  filter(taxonRank == "order") %>%
  select(order = scientificName, order_id = taxonID) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "family") %>%
    select(
      family_id = taxonID,
      order_id = parentNameUsageID,
      family = scientificName),
    by = "order_id",
    multiple = "all") %>%
  assert(is_uniq_2, family) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "subfamily") %>%
    select(
      subfamily_id = taxonID,
      family_id = parentNameUsageID,
      subfamily = scientificName),
    by = "family_id",
    multiple = "all") %>%
  assert(is_uniq_2, subfamily) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "tribe") %>%
    select(
      tribe_id = taxonID,
      subfamily_id = parentNameUsageID,
      tribe = scientificName),
    by = "subfamily_id",
    multiple = "all") %>%
  assert(is_uniq_2, tribe) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "genus") %>%
    select(
      genus_id_1 = taxonID,
      tribe_id = parentNameUsageID,
      genus_1 = scientificName),
    by = "tribe_id",
    multiple = "all") %>%
  assert(is_uniq_2, genus_1) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "genus") %>%
    select(
      genus_id_2 = taxonID,
      subfamily_id = parentNameUsageID,
      genus_2 = scientificName),
    by = "subfamily_id",
    multiple = "all") %>%
  assert(is_uniq_2, genus_2) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "genus") %>%
    select(
      genus_id_3 = taxonID,
      family_id = parentNameUsageID,
      genus_3 = scientificName),
    by = "family_id",
    multiple = "all") %>%
  assert(is_uniq_2, genus_3) %>%
  mutate(
    genus = coalesce(genus_1, genus_2, genus_3),
    genus_id = coalesce(genus_id_1, genus_id_2, genus_id_3)
  ) %>%
  assert(is_uniq_2, genus) %>%
  select(
    -genus_1, -genus_2, -genus_3,
    -genus_id_1, -genus_id_2, -genus_id_3
  ) %>%
  left_join(
    filter(wf_dwc_acc_only_no_higher_tax, taxonRank == "species") %>%
    select(
      taxonID,
      genus_id = parentNameUsageID
    ),
    by = "genus_id",
    multiple = "all") %>%
  assert(is_uniq_2, taxonID) %>%
  select(taxonID, genus, tribe, subfamily, family, order) %>%
  mutate(across(genus:order, ~str_split(.x, " ") %>% purrr::map_chr(1))) %>%
  unique()

# Add higher taxa
wf_dwc <-
wf_dwc_no_higher_tax %>%
  left_join(higher_taxa, by = "taxonID") %>%
  left_join(
    higher_taxa,
    by = c(acceptedNameUsageID = "taxonID"), na_matches = "never") %>%
  mutate(
    genus = coalesce(genus.x, genus.y),
    tribe = coalesce(tribe.x, tribe.y),
    subfamily = coalesce(subfamily.x, subfamily.y),
    family = coalesce(family.x, family.y),
    order = coalesce(order.x, order.y)
  ) %>%
  select(-matches("\\.x|\\.y"))

# Save final data in DWC format ----
write_csv(wf_dwc, "data/wf_dwc.csv")
