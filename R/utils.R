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
#' @param new_line A logical. If set to `TRUE`, then a new line command is
#'    placed at the end of the resulting string.
#' @return A string showing the entries of the vector.
stringify <- function(
  x,
  before = NULL,
  after = NULL,
  collapse = ", ",
  quote = "'",
  new_line = FALSE
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
  if (!is.logical(new_line) || length(new_line) != 1 || is.na(new_line))
    err_h("Argument `new_line` must be a non-missing logical value.")
  # stringify
  x <- tryCatch(
    as.character(x),
    error = function(e) err_h(paste0(
      "Argument `x` could not be converted into a character vector:\n", e))
  )
  if (!is.null(quote))
    x <- paste0(quote, x, quote)
  x <- paste(paste0(before, x, after), collapse = collapse)
  if (isTRUE(new_line))
    x <- paste0(x, "\n")
  x
}
