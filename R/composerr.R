#' @include closure.R
NULL

#' Compose error handlers (concatenate error messages)
#'
#' The following functions allow an advanced error handling, including the
#' incremental creation of complex error messages and optional accumulation of
#' multiple error messages (in the following `...` stands for
#' the arguments `text_*`, `sep_*` or `multiflush_*`):
#' - `composerr(...)`: Create a new customized error handling function.
#' - `composerr(..., err_h = err_h)`: Create a new customized error handling
#'   function, which enhances the existing error handling function `err_h()`.
#'   This can be useful in order to extend the error message parts in `err_h()`
#'   by additional message parts (defined in `...`).
#' - `composerr_modify(..., err_h = err_h)`:
#'   Does not create a new error handling function, but modifies an
#'   existing error handler (`err_h()`). With this function you can add new
#'   message parts to an existing error handler or change its default behavior
#'   (e.g. throw an error instead of a warning).
#'   **CAUTION: This function does return a function
#' - `composerr_halt(err_h)`: Set the error handling function `err_h()` to
#'   **non-flushing mode**, which means that each time `err_h(msg)` is called,
#'   the error message (`msg`) gets accumulated in an internal error stack,
#'   but no error is thrown.
#' - `composerr_flush(err_h)`: If the internal error stack is non-empty, then create
#'   the full error message from the internal error stack and throw an error.
#'   If the internal error stack is empty, then do not throw an error.
#' - `composerr_counterr(err_h)`: Counts the number of times `err_h()` was
#'   called, since `composerr_halt()`.
#' - `composerr_get_internal_handler(err_h)`: Returns the `internal_handler` used in
#'   `err_h()`. This can be useful, when the internal processing of the
#'   error message should be adapted (e.g. instead of writing it to a file,
#'   you want to write it to the file **and** do something else).
#' - `composerr_validate(err_h)`: Check if the object `err_h` is a
#'   `composerr` class object (was created with `composerr()`).
#' 
#' Further more, it is possible to change the error handling behavior to
#' throwing a warning (or some other customized behavior) instead of throwing
#' an error by calling `err_h(msg, handler = warning)`
#' @param text_1 A character string, which will be appended
#'   at the beginning of the error message. The argument `sep_1` will be used
#'   as text separator.
#' @param err_h Optional error handling function
#'   to which the message part should be appended.
#' @param text_2 A character string, which will be appended
#'   at the end of the error message. The argument `sep_2` will be used
#'   as text separator.
#' @param sep_1 A character string that is used as separator for the
#'   concatenation of `text_1` at the beginning of the error message.
#' @param sep_2 A character string that is used as separator for the
#'   concatenation of `text_2` at the end of the error message.
#' @param multiflush_start An optional string which will be added before
#'   a multi-line error stack section, when calling `composerr_flush()`.
#'   This argument only takes effect, if `composerr_halt(err_h)` was called
#'   once, then at least two times `err_h()` and then `composerr_flush(err_h)`.
#' @param multiflush_prefix An optional string which will be added before
#'   each line of a multi-line error stack section, when calling `composerr_flush()`.
#'   This argument only takes effect, if `composerr_halt(err_h)` was called
#'   once, then at least two times `err_h()` and then `composerr_flush(err_h)`.
#' @param multiflush_end An optional string which will be added after
#'   a multi-line error stack section, when calling `composerr_flush()`.
#'   This argument only takes effect, if `composerr_halt(err_h)` was called
#'   once, then at least two times `err_h()` and then `composerr_flush(err_h)`.
#' @param internal_handler An optional error handling function used
#'   as default value for `internal_handler` when calling `err_h(msg, internal_handler)`.
#'   This argument can be used, in order to change the default behavior of the
#'   error handling to throwing a warning (`internal_handler = warning`) or
#'   some other customized behavior instead of throwing an error, like
#'   writing the error message to a text file
#'   (`internal_handler = function(text) cat(text, FILENAME)`).
#' @return A new error handling function that has an extended error message.
#' @rdname composerr
#' @export
#' @examples 
#' \dontrun{
#' # -- composerr: create new error handler form old one --
#' # check if variable 'obj' exists and holds value TRUE
#' # original error handler
#' err_h <- composerr("Something went wrong")
#' if (!exists("obj"))
#'   err_h("`obj` does not exist.")
#' # Error: Something went wrong: `obj` does not exist.
#' obj <- FALSE
#' err_h2 <- composerr("`obj` has the wrong value", err_h = err_h)
#' if (!obj)
#'   err_h2("Value is FALSE.", handler = warning)
#' # Warning: Something went wrong: `obj` has the wrong value: Value is FALSE.
#' err_h("Old handler is still the same.")
#' # Error: Something went wrong: Old handler is still the same.
#' 
#' # -- composerr: update existing error handler --
#' # check if variable 'obj' exists and holds value TRUE
#' # original error handler
#' err_h <- composerr("Something went wrong")
#' if (!exists("obj"))
#'   err_h("`obj` does not exist.")
#' # Error: Something went wrong: `obj` does not exist.
#' obj <- FALSE
#' composerr_modify("`obj` has the wrong value", err_h = err_h)
#' if (!obj)
#'   err_h("Value is FALSE.", handler = warning)
#' # Warning: Something went wrong: `obj` has the wrong value: Value is FALSE.
#' 
#' # -- accumulate multiple errors --
#' err_h <- composerr(text_1 = "Something went wrong", sep_2 = "", text_2 = "Try again!")
#' err_h1 <- composerr("Error part", err_h = err_h)
#' composerr_halt(err_h1)
#' err_h1("First error.")
#' err_h1("Second error.")
#' composerr_flush(err_h1)
#' # Error: Something went wrong:
#' #   - Error part: First error.
#' #   - Error part: Second error.
#' # Try again!
#' }
composerr <- function(
  text_1 = NULL,
  err_h = NULL,
  text_2 = NULL,
  sep_1 = ": ",
  sep_2 = NULL,
  multiflush_start = "",
  multiflush_prefix = "\n  - ",
  multiflush_end = "\n",
  internal_handler = NULL
) {
  composerr_helper(
    text_1 = text_1,
    err_h = err_h,
    text_2 = text_2,
    sep_1 = sep_1,
    sep_2 = sep_2,
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler,
    new_handler = TRUE,
    err_h_usage = function(msg)
      stop(paste("Error while calling `composerr()`:", msg), call. = FALSE)
  )
}

#' @rdname composerr
#' @export
composerr_modify <- function(
  err_h,
  text_1 = NULL,
  text_2 = NULL,
  sep_1 = NULL,
  sep_2 = NULL,
  multiflush_start = NULL,
  multiflush_prefix = NULL,
  multiflush_end = NULL,
  internal_handler = NULL
) {
  composerr_helper(
    text_1 = text_1,
    err_h = err_h,
    text_2 = text_2,
    sep_1 = sep_1,
    sep_2 = sep_2,
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler,
    new_handler = FALSE,
    err_h_usage = function(msg)
      stop(paste("Error while calling `composerr_modify()`:", msg), call. = FALSE)
  )
}

#' Helper function for [composerr()] and [composerr_modify()]
#' 
#' @inheritParams composerr
#' @param new_handler A logical value. If set to `TRUE`, then a new error
#'   handling function is created, enhancing the existing error handling
#'   function `err_h()`. If `new_handler` set to `FALSE`, then the existing
#'   error handling function `err_h()` will be modified. 
#' @param err_h_usage An error handling function for catching
#'   errors because of improper usage of `composerr_helper()`.
composerr_helper <- function(
  text_1,
  err_h,
  text_2,
  sep_1,
  sep_2,
  multiflush_start,
  multiflush_prefix,
  multiflush_end,
  internal_handler,
  new_handler,
  err_h_usage
) {
  if (!is.function(err_h_usage) || length(rlang::fn_fmls(err_h_usage)) == 0)
    stop(
      paste(
        "Error while calling `composerr_helper()`:",
        "Argument `err_h_usage` must be a function that has at least one argument."
      ),
      call. = FALSE
    )
  if (!is.logical(new_handler) || length(new_handler) != 1 || is.na(new_handler))
    err_h_usage("Argument `new_handler` must be a logical value.")
  composerr_validate(err_h, allow_null = new_handler, err_h = err_h_usage)
  if (!is.null(text_1)) {
    if (!is.character(text_1))
      err_h_usage("Argument `text_1` must be a character vector")
    if (!is.null(sep_1) && (!is.character(sep_1) || length(sep_1) != 1))
      err_h_usage("Argument `sep_1` must be a string or `NULL`.")
  }
  if (!is.null(text_2)) {
    if (!is.character(text_2))
      err_h_usage("Argument `text_2` must be a character vector")
    if (!is.null(sep_2) && (!is.character(sep_2) || length(sep_2) != 1))
      err_h_usage("Argument `sep_2` must be a string or `NULL`.")
  }
  if (!is.null(multiflush_start) && (
    !is.character(multiflush_start) || length(multiflush_start) != 1
  ))
    err_h_usage("Argument `multiflush_start` must be a string.")
  if (!is.null(multiflush_prefix) && (
    !is.character(multiflush_prefix) || length(multiflush_prefix) != 1
  ))
    err_h_usage("Argument `multiflush_prefix` must be a string.")
  if (!is.null(multiflush_end) && (
    !is.character(multiflush_end) || length(multiflush_end) != 1
  ))
    err_h_usage("Argument `multiflush_end` must be a string.")
  if (!is.null(internal_handler) && (
    !is.function(internal_handler) || length(rlang::fn_fmls(internal_handler)) == 0
  ))
    err_h_usage("Argument `internal_handler` must be a function that has at least one argument.")
  # composerr_helper
  if (isTRUE(new_handler) || is.null(new_handler)) {
    if (is.null(err_h)) {
      if (is.null(internal_handler))
        internal_handler <- stop
      err_h <- restrict_fn_env(
        fn = function(msg, internal_handler = internal_handler) {
          if (!is.function(internal_handler) || length(rlang::fn_fmls(internal_handler)) == 0)
            stop(
              paste(
                "Error while executing the error handling function:",
                "Argument `internal_handler` must be a function with at leas one argument."
              ),
              call. = FALSE
            )
          if (identical(internal_handler, stop) || identical(internal_handler, warning)) {
            internal_handler(msg, call. = FALSE)
          } else {
            internal_handler(msg)
          }
        },
        vars = "internal_handler"
      )
    }
    err_h <- eval_closure(
      expr = {
        assigned_internal_handler <- NULL
        assigned_multiflush_start <- NULL
        assigned_multiflush_prefix <- NULL
        assigned_multiflush_end <- NULL
        text_before <- NULL
        text_after <- NULL
        err_stack <- NULL
        flush_halted <- FALSE
        modify <- function(
          text_1 = NULL,
          text_2 = NULL,
          sep_1 = NULL,
          sep_2 = NULL,
          multiflush_start = NULL,
          multiflush_prefix = NULL,
          multiflush_end = NULL,
          internal_handler = NULL
        ) {
          if (!is.null(text_1))
            text_before <<- paste(c(text_before, text_1, sep_1), collapse = "")
          if (!is.null(text_2))
            text_after <<- paste(c(sep_2, text_2, text_after), collapse = "")
          if (!is.null(multiflush_start))
            assigned_multiflush_start <<- multiflush_start
          if (!is.null(multiflush_prefix))
            assigned_multiflush_prefix <<- multiflush_prefix
          if (!is.null(multiflush_end))
            assigned_multiflush_end <<- multiflush_end
          if (!is.null(internal_handler))
            assigned_internal_handler <<- internal_handler
          invisible(NULL)
        }
        halt <- function(
          multiflush_start = NULL,
          multiflush_prefix = NULL,
          multiflush_end = NULL,
          internal_handler = NULL
        ) {
          if (!is.null(multiflush_start))
            assigned_multiflush_start <<- multiflush_start
          if (!is.null(multiflush_prefix))
            assigned_multiflush_prefix <<- multiflush_prefix
          if (!is.null(multiflush_end))
            assigned_multiflush_end <<- multiflush_end
          if (!is.null(internal_handler))
            assigned_internal_handler <<- internal_handler
          flush_halted <<- TRUE
          invisible(NULL)
        }
        flush <- function(
          multiflush_start = NULL,
          multiflush_prefix = NULL,
          multiflush_end = NULL,
          internal_handler = NULL
        ) {
          if (is.null(multiflush_start))
            multiflush_start <- assigned_multiflush_start
          if (is.null(multiflush_prefix))
            multiflush_prefix <- assigned_multiflush_prefix
          if (is.null(multiflush_end))
            multiflush_end <- assigned_multiflush_end
          if (is.null(internal_handler))
            internal_handler <- assigned_internal_handler
          flush_halted <<- FALSE
          if (is.null(err_stack))
            return(invisible(0L))
          num_err <- counterr()
          if (length(err_stack) > 1) {
            full_msg <- paste(
              c(
                multiflush_start,
                paste0(multiflush_prefix, err_stack),
                multiflush_end
              ),
              collapse = ""
            )
          } else {
            full_msg <- err_stack
          }
          err_stack <<- NULL
          err_h(full_msg, internal_handler = internal_handler)
          invisible(num_err)
        }
        counterr <- function() {
          invisible(length(err_stack))
        }
        get_internal_handler <- function() {
          invisible(assigned_internal_handler)
        }
        function(msg, internal_handler = NULL) {
          err_stack <<- c(
            err_stack,
            paste(c(text_before, msg, text_after), collapse = "")
          )
          if (isFALSE(flush_halted)) {
            flush(internal_handler = internal_handler)
          }
        }
      },
      vars = "err_h"
    )
    class(err_h) <- "composerr"
  }
  # update (old or new) handler
  get("modify", envir = environment(err_h))(
    text_1 = text_1,
    sep_1 = sep_1,
    text_2 = text_2,
    sep_2 = sep_2,
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler
  )
  invisible(err_h)
}

#' @param err_h An existing error handling function created with `composerr()`.
#' @export
#' @rdname composerr
composerr_halt <- function(
  err_h,
  multiflush_start = NULL,
  multiflush_prefix = NULL,
  multiflush_end = NULL,
  internal_handler = NULL
) {
  err_h_usage <- function(msg)
    stop(paste("Error while calling `composerr_halt()`:", msg), call. = FALSE)
  composerr_validate(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  if (!is.null(multiflush_start) && (
    !is.character(multiflush_start) || length(multiflush_start) != 1
  ))
    err_h_usage("Argument `multiflush_start` must be a string.")
  if (!is.null(multiflush_prefix) && (
    !is.character(multiflush_prefix) || length(multiflush_prefix) != 1
  ))
    err_h_usage("Argument `multiflush_prefix` must be a string.")
  if (!is.null(multiflush_end) && (
    !is.character(multiflush_end) || length(multiflush_end) != 1
  ))
    err_h_usage("Argument `multiflush_end` must be a string.")
  if (!is.null(internal_handler) && (
    !is.function(internal_handler) || length(rlang::fn_fmls(internal_handler)) == 0
  ))
    err_h_usage("Argument `internal_handler` must be a function that has at least one argument.")
  # composerr_halt
  get("halt", envir = environment(err_h))(
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler
  )
}

#' @export
#' @rdname composerr
composerr_flush <- function(
  err_h,
  multiflush_start = NULL,
  multiflush_prefix = NULL,
  multiflush_end = NULL,
  internal_handler = NULL
) {
  err_h_usage <- function(msg)
    stop(paste("Error while calling `composerr_flush()`:", msg), call. = FALSE)
  composerr_validate(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  if (!is.null(multiflush_start) && (
    !is.character(multiflush_start) || length(multiflush_start) != 1
  ))
    err_h_usage("Argument `multiflush_start` must be a string.")
  if (!is.null(multiflush_prefix) && (
    !is.character(multiflush_prefix) || length(multiflush_prefix) != 1
  ))
    err_h_usage("Argument `multiflush_prefix` must be a string.")
  if (!is.null(multiflush_end) && (
    !is.character(multiflush_end) || length(multiflush_end) != 1
  ))
    err_h_usage("Argument `multiflush_end` must be a string.")
  if (!is.null(internal_handler) && (
    !is.function(internal_handler) || length(rlang::fn_fmls(internal_handler)) == 0
  ))
    err_h_usage("Argument `internal_handler` must be a function that has at least one argument.")
  # composerr_flush
  get("flush", envir = environment(err_h))(
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler
  )
}

#' @export
#' @rdname composerr
composerr_counterr <- function(
  err_h
) {
  err_h_usage <- function(msg)
    stop(paste("Error while calling `composerr_counterr()`:", msg), call. = FALSE)
  composerr_validate(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  # composerr_counterr
  get("counterr", envir = environment(err_h))()
}

#' @export
#' @rdname composerr
composerr_get_internal_handler <- function(
  err_h
) {
  err_h_usage <- function(msg)
    stop(paste("Error while calling `composerr_get_internal_handler()`:", msg), call. = FALSE)
  composerr_validate(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  # composerr_counterr
  get("get_internal_handler", envir = environment(err_h))()
}

#' @param obj An object that should be an error handling
#'   function created by `composerr()`
#' @param allow_null A logical flag, defining if `obj = NULL` should be allowed.
#' @param obj_name An optional string, defining the variable name of
#'   `obj`. This string is used for creating a meaningful error message,
#'   in case the validation failed. If `obj_name` is omitted, then `obj_name`
#'   is calculated by using **non-standard-evaluation** on `obj`.
#' @export
#' @rdname composerr
composerr_validate <- function(
  obj,
  err_h,
  allow_null = TRUE,
  obj_name = NULL
) {
  err_h_usage <- function(msg)
    stop(paste("Error while calling `composerr_validate()`:", msg), call. = FALSE)
  if (is.null(obj_name))
    obj_name <- deparse(substitute(obj))
  if (!is.function(err_h) || length(rlang::fn_fmls(err_h)) == 0)
    err_h_usage("Argument `err_h` must be a function that has at least one argument.")
  if (!is.logical(allow_null) || length(allow_null) != 1 || is.na(allow_null))
    err_h_usage("Argument `allow_null` must be a logical value.")
  if (!is.character(obj_name) || length(obj_name) != 1 || is.na(obj_name))
    err_h_usage("Argument `obj_name` must be a string or omitted.")
  # composerr_validate
  if ((is.null(obj) && isFALSE(allow_null)) || (!is.null(obj) && (
    !is.function(obj) || !"composerr" %in% class(obj)
  )))
    err_h(paste0(
      "`", obj_name,
      "` is not an error handler created by `composerr()`."
    ))
  invisible(obj)
}
