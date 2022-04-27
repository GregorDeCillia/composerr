#' Stringify vector
#'
#' Turn a vector (e.g. `c(1, 2, 3)`) into a string (e.g. `"'1', '2', '3'"`).
#' This function is particularly useful when creating error messages,
#' describing problematic values.
#' @param x An object, which should be turned into a string. Usually an atomic
#'   object (a vector).
#' @param str_quote A string, holding  the symbol which should be used for quoting
#'   every entry of `x`. Default is `'` which gives the result `'entry1'`.
#' @param str_collapse A string, used for separating each vector entry.
#' @param str_before A string placed before each vector entry.
#' @param str_after A string placed after each vector entry.
#' @param new_line A logical. If set to `TRUE`, then a new line command is
#'    placed at the end of the resulting string.
#' @return A string showing the entries of the vector.
stringify <- function(
  x,
  str_collapse = ", ",
  str_quote = "'",
  str_before = "",
  str_after = "",
  new_line = FALSE
) {
  err_h <- function(msg)
    stop(paste("Error while calling `stringify()`:", msg), call. = FALSE)
  if (!is.null(str_collapse) && (
    !is.character(str_collapse) || length(str_collapse) > 1 || is.na(str_collapse)
  ))
    err_h("Argument `str_collapse` must be a non-missing string value or `NULL`.")
  if (!is.null(str_quote) && (
    !is.character(str_quote) || length(str_quote) > 1 || is.na(str_quote)
  ))
    err_h("Argument `str_quote` must be a non-missing string value or `NULL`.")
  if (!is.null(str_before) && (
    !is.character(str_before) || length(str_before) > 1 || is.na(str_before)
  ))
    err_h("Argument `str_before` must be a non-missing string value or `NULL`.")
  if (!is.null(str_after) && (
    !is.character(str_after) || length(str_after) > 1 || is.na(str_after)
  ))
    err_h("Argument `str_after` must be a non-missing string value or `NULL`.")
  if (!is.logical(new_line) || length(new_line) > 1 || is.na(new_line))
    err_h("Argument `new_line` must be a non-missing logical value.")
  # stringify
  x <- tryCatch(
    as.character(x),
    error = function(e) err_h(paste0(
      "Argument `x` could not be converted into a character vector:\n", e))
  )
  if (!is.null(str_quote))
    x <- paste0(str_quote, x, str_quote)
  x <- paste(paste0(str_before, x, str_after), collapse = str_collapse)
  if (isTRUE(new_line))
    x <- paste0(x, "\n")
  x
}
