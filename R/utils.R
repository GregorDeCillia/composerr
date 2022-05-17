#' Stringify vector
#'
#' Turn a vector (e.g. `c(1, 2, 3)`) into a string (e.g. `"'1', '2', '3'"`).
#' This function is particularly useful when creating error messages,
#' describing problematic values.
#' @param x An object, which should be turned into a string. Usually an atomic
#'   object (a vector).
#' @param quote A string, holding  the symbol which should be used for quoting
#'   every entry of `x`. Default is `'` which gives the result `'entry1'`.
#' @param collapse A string, used for separating each vector entry.
#' @param before A string placed before each vector entry.
#' @param after A string placed after each vector entry.
#' @param n_max A positive integer or `Inf`, defining the maximum number of
#'   displayed vector elements. All further elements are displayed with the
#'   text defined in `further`.
#' @param further A string indicating further (after `n_max` displayed elements)
#'   vector elements.
#' @return A string showing the entries of the vector.
stringify <- function(
  x,
  before = NULL,
  after = NULL,
  collapse = ", ",
  quote = "'",
  n_max = 10,
  further = "..."
) {
  err_h <- function(msg)
    stop(paste("Error while calling `stringify()`:", msg), call. = FALSE)
  if (!is.null(collapse) && (
    !is.character(collapse) || length(collapse) != 1 || is.na(collapse)
  ))
    err_h("Argument `collapse` must be a non-missing string value or `NULL`.")
  if (!is.null(quote) && (
    !is.character(quote) || length(quote) != 1 || is.na(quote)
  ))
    err_h("Argument `quote` must be a non-missing string value or `NULL`.")
  if (!is.null(before) && (
    !is.character(before) || length(before) != 1 || is.na(before)
  ))
    err_h("Argument `before` must be a non-missing string value or `NULL`.")
  if (!is.null(after) && (
    !is.character(after) || length(after) != 1 || is.na(after)
  ))
    err_h("Argument `after` must be a non-missing string value or `NULL`.")
  if (!is.numeric(n_max) || length(n_max) != 1 || is.na(n_max) || n_max < 1 ||
      (is.finite(n_max) && as.integer(n_max) != n_max)
  )
    err_h("Argument `n_max` be a positive integer value or `Inf`.")
  if (!is.character(further) || length(further) != 1 || is.na(further))
    err_h("Argument `further` must be a non-missing string value.")
  # stringify
  x <- tryCatch(
    as.character(x),
    error = function(e) err_h(paste0(
      "Argument `x` could not be converted into a character vector:\n", e))
  )
  add_dots <- length(x) > n_max
  if (length(x) > n_max)
    x <- x[1:n_max]
  if (!is.null(quote))
    x <- c(
      paste0(quote, x, quote),
      if (add_dots) further else NULL
    )
  paste(paste0(before, x, after), collapse = collapse)
}
