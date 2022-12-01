# Generate custom error message for assertr
# https://github.com/ropensci/assertr/issues/70
err_msg <- function(msg) stop(msg, call. = FALSE)

# Modified version of `%in%` that returns FALSE for elements
# of input that are NA
`%in_f_na%` <- function(x, y) {
  res <- x %in% y
  if (all(!is.na(x))) return(res)
  res[is.na(x)] <- FALSE
  res
}

# Run checks on uploaded data
# if all checks pass, return data
verify_ul <- function(pteridocat, data_ul, valid_col_names) {

  data_ul |>
    # Check col names
    assertr::verify(
      all(
        colnames(data_ul) %in% valid_col_names) &&
        all(valid_col_names %in% colnames(data_ul)
      ),
      error_fun = err_msg(
        glue::glue("Column names must include only {paste0(valid_col_names, collapse = ', ')}") # nolint
      )
    ) |>
    # Check non-missing cols
    assertr::assert(
      assertr::not_na,
      taxonID, taxonomicStatus, taxonRank, scientificName, new) |>
    # Check unique cols
    assertr::assert(assertr::is_uniq, taxonID) |>
    # Check `new` column
    assertr::assert(
      assertr::in_set(c("0", "1"), allow.na = FALSE), new,
      success_fun = assertr::success_logical)

  # - make sure new and old IDs are assigned correctly
  new_ids <- data_ul |>
    dplyr::filter(new == "1") |>
    dplyr::pull(taxonID)

  old_ids <- data_ul |>
    dplyr::filter(new == "0") |>
    dplyr::pull(taxonID)

  bad_new_ids <- new_ids[new_ids %in% pteridocat$taxonID]

  bad_old_ids <- old_ids[!old_ids %in% pteridocat$taxonID]

  assertthat::assert_that(
    length(bad_new_ids) == 0,
    msg = glue::glue(
      "These new taxonIDs are already in the existing data: {paste0(bad_new_ids, collapse = ', ')}" # nolint
    )
  )

  assertthat::assert_that(
    length(bad_old_ids) == 0,
    msg = glue::glue(
      "These old taxonIDs are not in the existing data: {paste0(bad_old_ids, collapse = ', ')}" # nolint
    )
  )

  data_ul
}

make_new_data <- function(new_dat_raw, pteridocat) {
  # Manipulate user-provided data ----
  # - Add parsed scientific names
  # (doesn't include infragenericEpithet, will break if this is provided)
  sci_names <- new_dat_raw |>
    dplyr::pull(scientificName) |>
    unique() |>
    taxastand::ts_parse_names(
      docker = FALSE
    ) |>
    dplyr::select(
      scientificName = name,
      genericName = genus_name,
      specificEpithet = specific_epithet,
      infraspecificEpithet = infraspecific_epithet
    )

  new_dat <- dplyr::left_join(
    new_dat_raw, sci_names, by = "scientificName") |>
    dplyr::select(-new) |>
    dplyr::select(dplyr::any_of(colnames(pteridocat)))

  # Add new data ----
  dplyr::anti_join(pteridocat, new_dat, by = "taxonID") |>
    dplyr::bind_rows(new_dat) |>
    dwctaxon::dct_validate(
      check_sci_name = FALSE,
      check_status_diff = FALSE,
      on_fail = "summary") |>
    dplyr::arrange(scientificName)
}

push_pterido <- function(pteridocat, user, email, changes_summary, pat) {
  # Set PAT
  Sys.setenv(GITHUB_PAT = pat)
  # set path to pp2 repo clone
  ppg2_repo <- fs::path(tempdir(), "ppg2")
  if (fs::dir_exists(ppg2_repo)) {
    fs::dir_delete(ppg2_repo)
  }
  # Clone the ppg2 repo
  gert::git_clone(
    url = "https://github.com/joelnitta/ppg2",
    path = ppg2_repo,
    branch = "main")
  # Make a new branch, named after hash of pteriocat
  # git config --global --add safe.directory /tmp/Rtmp3Zrmyz/ppg2
  br_name <- digest::digest(pteridocat)
  gert::git_branch_create(br_name, repo = ppg2_repo)
  gert::git_branch_checkout(br_name, repo = ppg2_repo)
  # Write out updated pteridocat db
  write.csv(
    pteridocat,
    fs::path(ppg2_repo, "data/pteridocat.csv"),
    row.names = FALSE)
  # Commit change
  gert::git_add(
    files = "data/pteridocat.csv",
    repo = ppg2_repo)
  gert::git_commit(
    "Update pteridocat",
    repo = ppg2_repo,
    author = gert::git_signature(user, email))
  # Push change
  gert::git_push(remote = "origin", repo = ppg2_repo)
  # Create PR
  gh_pr_create(
    title = "Update to PPG2",
    body = glue::glue(
      "Submitted by: {user}
      Summary of changes: {changes_summary}
      "),
    branch = br_name,
    repo = ppg2_repo,
    pat = pat
  )
}

# Create a pull request. Requires github CLI to be installed
gh_pr_create <- function(title, body, branch, base = "main", repo, pat) {
  res <- processx::run(
    command = "gh",
    args = c(
      "pr", "create",
      "--title", title,
      "--body", body,
      "--head", branch,
      "--base", base),
    wd = repo,
    env = c("current", GH_TOKEN = pat)
  )
  # Return URL of pull request
  gsub("\n", "", res$stdout, fixed = TRUE)
}