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
