#' @include closure.R
NULL

#' Compose error handlers (concatenate error messages)
#'
#' The following functions allow the creation/modification of
#' **error handlers** and incremental creation of complex error messages
#' holding precise information **where and why** the problem emerged
#' (in the following `...` stands for
#' the arguments `text_*`, `sep_*`, `multiflush_*` or `internal_handler`):
#' - `composerr(...)`: Create a new error handling function from scratch.
#' - `composerr(..., err_h = err_h_old)`: Create a new error handling
#'   function, which enhances the existing error handler `err_h_old`.
#'   This can be useful in order to extend the error message parts in `err_h_old`
#'   by adding additional message parts (by the text parts
#'   `text_1`, `sep_1`, `text_2`, `sep_2`, `multiflush_prefix`,
#'   `multiflush_start` and `multiflush_end` defined in `...`).
#' - `composerr_modify(..., err_h = err_h_old)`:
#'   Does not create a new error handling function, but modifies an
#'   existing error handler `err_h_old`. With this function you can add new
#'   message parts to an existing error handler or change its default behavior
#'   (like throwing a warning instead of an error etc.).
#'   **CAUTION**: This function modifies the **original** error handling object
#'   `err_h_old` (**by reference**).
#' @section Cascading error handlers:
#' If `err_h1 <- composerr(...)` was created without passing another
#' error handler to the optional argument `err_h`, then
#' `err_h1` is a newly created error handler with the following structure
#' `err_h1 = function(msg = NULL, internal_handler = NULL, ...)`.
#' Similarly, if `err_h1 <- composerr(err_h = err_h_old, ...)` was created
#' by passing `err_h = err_h_old`, then `err_h1` is also
#' a new error handler, which also has the structure
#' `err_h1 = function(msg = NULL, internal_handler = NULL, ...)`, but
#' now it was created from another error handler `err_h_old`.
#' 
#' We now consider the latter case
#' `err_h1 <- composerr(text_1, err_h_old, text_2, sep_1, sep_2, internal_handler = internal_handler_1)`.
#' If the error handler gets executed with the following call
#' `err_h1(msg, internal_handler = internal_handler_2, ...)`,
#' then the following things happen:
#' - `err_h1` first creates a more detailed message
#'   `msg_new = text_1+sep_1+msg+sep_2+text_2` (here `+` stands for string concatenation.)
#' - if no argument `internal_handler` was passed to `err_h1`,
#'   then `internal_handler` is set to the default handler
#'   (in this example `internal_handler_1`) previously assigned to `err_h1`.
#'   If no default handler was previously assigned to `err_1`,
#'   then `internal_handler <- NULL`.
#' - `err_h1` calls `err_h_old(msg_new, internal_handler = internal_handler, ...)`.
#' 
#' Now `err_h_old` is called and is running through the same process.
#' We therefore have a **cascade of repeated error handler calls**, passing
#' down
#' - a successively growing error message `msg` (incrementally adding text parts
#'   in front and at the end of the message).
#' - the argument `internal_handler`, which can
#'   either be `NULL` or a function used for final processing of the created
#'   error message (throw an error or a warning or do something else).
#'   If the passed down `internal_handler` argument is `NULL` and at some
#'   point in the error handling cascade there is a non-null
#'   default internal handler that was assigned to some error handler
#'   in the error handler cascade, then this default internal handler is 
#'   passed down as `internal_handler` instead. Once the `internal_handler`
#'   is non-null, it will remain unchanged, no matter if there were other
#'   default handlers assigned to error handler inside the error handler
#'   cascade.
#' - additional arguments `...` passed to `err_h1` are directly passed down
#'   the error handler cascade and get directly passed to the ultimate call
#'   of the internal error processing function `internal_handler(msg_final, ...)`.
#' 
#' After cascading backwards the error handlers, we ultimately reach the
#' primal error handler, which was created first
#' (from scratch) and not from another error handler.
#' Let us call it `err_h_primal`.
#' This primal error handler does the following things:
#' - As usual `err_h_primal` concatenates its additional text parts to the
#'   passed in message. This gives us at last the final version of the error
#'   message. Let us call it `msg_final`.
#' - As usual `err_h_primal` checks if `internal_handler` is `NULL`.
#'   If so, `internal_handler` is set to the default handler that was assigned
#'   to `err_h_primal`.
#' - If the `internal_handler` is still `NULL`, then `internal_handler` is
#'   replaced by the [stop()] function.
#' - Ultimately `err_h_primal` calls `internal_handler(msg_final, ...)`.
#' 
#' The result of the final call `internal_handler(msg_final, ...)` is silently
#' returned by `err_h1`.
#' @section Stacked error messages:
#' Let us assume `composerr_halt(err_h1)` was called before calling
#' `err_h1(msg, ...)`.
#' In this case the error message will not be cascaded down and `internal_handler` 
#' will not get called.
#' Instead `err_h1` will do the following things:
#' - As usual `err_h1` will append the additional message parts:
#'   `msg_new <- text_1+sep_1+msg+sep_2+text_2`
#' - `err_h1` will add the resulting error message `msg_new` to an internal
#'   error stack of `err_h1`.
#' - `err_h1` will return the value `NULL`
#' 
#' If later on, the function
#' `composerr_flush(err_h1, internal_handler = internal_handler4, ...)` is
#' called, then the error handler cascade will be executed as usual, but first
#' the error messages stored in the error stack of `err_h1` will be collapsed
#' into a single error message and this summed up error message will be passed
#' down the error handler cascade as usual.
#' In short, the call
#' `composerr_flush(err_h1, internal_handler = internal_handler4, ...)`
#' will do the following things:
#' - All error messages stored in the internal error stack of `err_h1` will
#'   be concatenated into a single string by adding the strings `multiflush_prefix`
#'   in front of each stack entry and then collapsing the message stack into a
#'   single string and then
#'   adding `multiflush_start` in front and `multiflush_end` at the end this
#'   message string:
#'   `msg_new <- multiflush_start+collapse(multiflush_prefix+stack)+multiflush_end`
#' - As usual, if `internal_handler` is `NULL`, then it will be replaced by 
#'   the default internal handler assigned to `err_h1`.
#' - `err_h_old(msg_new, internal_handler = internal_handler)` will be called and
#'   the error handler cascade will continue as usual.
#'   
#' It is also possible to halt the error execution in the
#' **middle of the error handler cascade**, by calling
#' `composerr_halt(err_h_middle)`,
#' where `err_h_middle` is an error handler, that has at least one child 
#' error handler `err_h1` (an error handler that was created from `err_h_middle`).
#' If `err_h1` or a child error handler of `err_h1` gets called,
#' then the error handling will be cascaded down as usual until it reaches
#' `err_h_middle`. There it will halted and the created message will be stacked
#' in the internal error message stack of `err_h_middle`.
#' Only by calling `composerr_flush(err_h_middle)` the
#' error handling cascade can be continued.
#' 
#' It is also possible to halt the error execution at
#' **multiple points of the error handler cascade**. In order to cascade
#' the errors down till the end, each halted error handler must be flushed with
#' `composerr_flush()`.
#' @param text_1 Optional string that will be appended
#'   at the beginning of the error message. The argument `sep_1` will be used
#'   as text separator.
#' @param err_h Optional error handling function
#'   to which the message part should be appended.
#' @param text_2 Optional string that will be appended
#'   at the end of the error message. The argument `sep_2` will be used
#'   as text separator.
#' @param sep_1 Optional string that is used as separator for the
#'   concatenation of `text_1` at the beginning of the error message.
#' @param sep_2 Optional string that is used as separator for the
#'   concatenation of `text_2` at the end of the error message.
#' @param multiflush_start Optional string which will be added before
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
#' @param internal_handler Optional error handling function used
#'   as default value for `internal_handler`
#'   when calling `err_h(msg, internal_handler = NULL)`.
#'   This function is passed down the error handler cascade until it
#'   finally gets called in order to process the incrementally extended error
#'   message (e.g. throw an error or a warning or do something else with the
#'   created error message).
#'   The argument `internal_handler` can be used, in order to change the
#'   default behavior of the error processing to throwing a warning
#'   (`internal_handler = warning`) or
#'   some other customized behavior like
#'   writing the error message to a text file
#'   (`internal_handler = function(text) cat(text, file = FILENAME)`).
#' @return The call `composerr_modify(err_h_old)` silently returns
#'   the modified error handler `err_h_old`, but since
#'   `composerr_modify(err_h_old)`
#'   does not create a new error handler, but merely modifies `err_h_old`,
#'   there is no need to reassign `err_h_old <- composerr_modify(err_h_old)`.
#'   
#'   The call `composerr(...)` always returns a new error handler, either
#'   created from scratch or enhancing another error handler `err_h_old`.
#'   The resulting error handler is a function with the following arguments:
#'   - `msg`: An optional string holding the error message. This error
#'     message will be passed down the error handler cascade and get recursively
#'     concatenated with additional message parts.
#'   - `internal_handler`: An optional error processing function
#'     (`function(msg = NULL, ...)`), which
#'     will be passed down the error handler cascade and ultimately be executed.
#'   - `...`: Additional arguments passed down the error handler cascade and
#'     ultimately passed to the called `internal_handler`.
#' @rdname composerr
#' @seealso [composerr_flush()], [composerr_halt()], [composerr_counterr()],
#'   [composerr_get_internal_handler()] and [validate_composerr()]
#' @export
#' @examples 
#' \dontrun{
#' #' ##### Example-1: Create child error handlers and modify existing handlers #####
#' # create new error handler `err_h1()` from scratch
#' err_h1 <- composerr(text_1 = "T1", sep_1 = "_", text_2 = "T2", sep_2 = "_")
#' 
#' # `err_h1` throws errors by default
#' err_h1("MSG")
#' # Error: T1_MSG_T2
#' 
#' # With `internal_handler = warning` a warning will be sent instead
#' err_h1("MSG", internal_handler = warning)
#' # Warning: T1_MSG_T2
#' 
#' # create a new error handler `err_h2` based on `err_h1`
#' err_h2 <- composerr(
#'   err_h = err_h1, text_1 = "X1", sep_1 = "#", text_2 = "X2", sep_2 = "#",
#'   internal_handler = message
#' )
#' 
#' # Because of `internal_handler = message`, the handler `err_h2` will print
#' # plain messages by default
#' err_h2("MSG")
#' # T1_X1#MSG#X2_T2
#' 
#' # The original error handler `err_h1` remained unchanged
#' err_h1("MSG")
#' # Error: T1_MSG_T2
#' 
#' # Modify an existing error handler without creating a new one
#' composerr_modify(
#'   err_h = err_h1, text_1 = "Y1", sep_1 = "-", text_2 = "Y2", sep_2 = "-",
#'   internal_handler = warning
#' )
#' 
#' # The old error handler `err_h1` has now a different default behavior
#' err_h1("MSG")
#' # Warning: T1_Y1-MSG-Y2_T2
#' 
#' # The error handler `err_h2` enhances the now
#' # modified `err_h1` and has hence also changed
#' err_h2("MSG")
#' # T1_Y1-X1#MSG#X2-Y2_T2
#' 
#' ##### Example-2: Halt/Flush errors #####
#' # create general error handler
#' err_h1 <- composerr(text_1 = "There are problems")
#' 
#' # create a more precise error handler listing the problems
#' err_h_detail <- composerr("problem", sep_1 = "-", err_h = err_h1)
#' 
#' # halt `err_h_detail()` processing in order to collect
#' # multiple errors
#' composerr_halt(err_h_detail)
#' err_h_detail("1")
#' err_h_detail("2")
#' err_h_detail("3")
#' 
#' composerr_flush(err_h_detail)
#' # Error: There are problems:
#' #   - problem-1
#' #   - problem-2
#' #   - problem-3
#' 
#' ##### Example-3: Error/Warning handling in functions #####
#' my_vec_mult <- function(x, y) {
#'   # create your error handlers
#'   err_h <- composerr("In `my_vec_mult()`")
#'   err_h_x <- composerr("Invalid argument `x`", err_h)
#'   err_h_y <- composerr("Invalid argument `y`", err_h)
#'   if (!is.numeric(x))
#'     err_h_x("Not a number.")
#'   if (any(is.na(x)))
#'     err_h_x("Has missings.", internal_handler = warning)
#'   if (!is.numeric(y))
#'     err_h_y("Not a number.")
#'   if (any(is.na(y)))
#'     err_h_y("Has missings.", internal_handler = warning)
#'   if (length(x) != length(y))
#'     err_h("Vectors `x` and `y` have different length.")
#'   sum(x*y, na.rm = TRUE)
#' }
#' 
#' my_vec_mult("a", 1:2)
#' # Error: In `my_vec_mult()`: Invalid argument `x`: Not a number.
#' 
#' my_vec_mult(c(1, NA), 1:2)
#' # 1
#' # Warning: In `my_vec_mult()`: Invalid argument `x`: Has missings.
#' 
#' my_vec_mult(1:2, "b")
#' # Error: In `my_vec_mult()`: Invalid argument `y`: Not a number.
#' 
#' my_vec_mult(1:2, c(1, NA))
#' # 1
#' # Warning: In `my_vec_mult()`: Invalid argument `y`: Has non-finite values.
#' 
#' my_vec_mult(1:2, 1:3)
#' # Error: In `my_vec_mult()`: Vectors `x` and `y` have different length.
#' 
#' my_vec_mult(1:2, 1:2)
#' # 5
#' 
#' ##### Example-4: Error handling in sub routines #####
#' validate_numeric_vec <- function(obj, err_h) {
#'   obj_name <- deparse(substitute(obj))
#'   err_h <- composerr(paste0("Invalid argument `", obj_name, "`"), err_h)
#'   if (!is.numeric(obj))
#'     err_h("Not a number.")
#'   err_h <- composerr(err_h = err_h)
#'   composerr_halt(err_h)
#'   for (i in seq_along(obj)) {
#'     err_h_item <- composerr(text_1 = paste0("Item-", i), err_h, sep_1 = " is ")
#'     if (is.na(obj[i]) && !is.nan(obj[i]))
#'       err_h_item("NA.")
#'     if (is.nan(obj[i]))
#'       err_h_item("NaN.")
#'     if (is.infinite(obj[i]))
#'       err_h_item("infinite.")
#'   }
#'   composerr_flush(err_h)
#'   invisible(obj)
#' }
#' 
#' my_vec_mult2 <- function(x, y) {
#'   err_h <- composerr("In `my_vec_mult2()`")
#'   validate_numeric_vec(x, err_h)
#'   validate_numeric_vec(y, err_h)
#'   if (length(x) != length(y))
#'     err_h("Vectors `x` and `y` have different length.")
#'   sum(x*y)
#' }
#' 
#' my_vec_mult2("a", 1:4)
#' # Error: In `my_vec_mult2()`: Invalid argument `x`: Not a number.
#' 
#' my_vec_mult2(c(1, NA, NaN, Inf, 5), 1:5)
#' # Error: In `my_vec_mult2()`: Invalid argument `x`:
#' #   - Item-2 is NA.
#' #   - Item-3 is NaN.
#' #   - Item-4 is infinite.
#' 
#' my_vec_mult2(1:5, c(NaN, 2, 3, NA, Inf))
#' # Error: In `my_vec_mult2()`: Invalid argument `y`:
#' #   - Item-1 is NA.
#' #   - Item-4 is NaN.
#' #   - Item-5 is infinite.
#' 
#' my_vec_mult2(1:5, 1:4)
#' # Error: In `my_vec_mult2()`: Vectors `x` and `y` have different length.
#' 
#' my_vec_mult2(1:5, 1:5)
#' # 55
#' 
#' ##### Example-5: Write errors to log file #####
#' logfile = tempfile()
#' err_h <- composerr(
#'   "There was a problem",
#'   internal_handler = function(msg) {
#'     cat(msg, file = logfile, append = TRUE, fill = TRUE)
#'   }
#' )
#' err_h("MSG")
#' 
#' ##### Example-6: Optionally suppressing warnings #####
#' my_sum <- function(x, suppress_warnings = FALSE) {
#'   if (isFALSE(suppress_warnings)) {
#'     err_h <- composerr("Problem in `my_sum()`", internal_handler = warning)
#'   } else {
#'     # internal handler does nothing
#'     err_h <- composerr(internal_handler = function(...) {})
#'   }
#'   if (any(is.na(x)))
#'     err_h("`x` has missing values.")
#'   sum(x, na.rm = TRUE)
#' }
#' 
#' my_sum(c(1, 2, NA))
#' # 3
#' # Warning: Problem in `my_sum()`: `x` has missing values.
#' 
#' my_sum(c(1, 2, NA), suppress_warnings = TRUE)
#' # 3
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
    err_h_usage = function(msg = NULL)
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
    err_h_usage = function(msg = NULL)
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
  validate_composerr(err_h, allow_null = new_handler, err_h = err_h_usage)
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
  if (is.null(err_h) && is.null(internal_handler))
    internal_handler <- stop
  if(identical(internal_handler, stop) || identical(internal_handler, warning))
    internal_handler <- restrict_fn_env(
      fn = function(msg, ...) {
        do.call(
          internal_handler,
          append(
            list(msg), # TODO: continue
            plyr::defaults(
              list(...),
              list(call. = FALSE)
            )
          )
        )
      },
      vars = "internal_handler"
    )
  if (isTRUE(new_handler)) {
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
          internal_handler = NULL,
          ...
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
            return(invisible(NULL))
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
          if (!is.null(err_h)) {
            invisible(err_h(full_msg, internal_handler = internal_handler, ...))
          } else {
            if (is.null(internal_handler))
              internal_handler <- stop
            if(identical(internal_handler, stop) || identical(internal_handler, warning))
              internal_handler <- restrict_fn_env(
                fn = function(msg, ...) {
                  do.call(
                    internal_handler,
                    append(
                      list(msg), # TODO: continue
                      plyr::defaults(
                        list(...),
                        list(call. = FALSE)
                      )
                    )
                  )
                },
                vars = "internal_handler"
              )
            internal_handler(full_msg, ...)
          }
        }
        counterr <- function() {
          invisible(length(err_stack))
        }
        get_internal_handler <- function() {
          if (is.null(assigned_internal_handler)) {
            get("get_internal_handler", envir = environment(err_h))()
          } else {
            invisible(assigned_internal_handler)
          }
        }
        function(msg = NULL, internal_handler = NULL, ...) {
          err_stack <<- c(
            err_stack,
            paste(c(text_before, msg, text_after), collapse = "")
          )
          if (isFALSE(flush_halted)) {
            flush(internal_handler = internal_handler, ...)
          } else {
            invisible(NULL)
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

#' Halting and flushing errors
#' 
#' The following functions allow the accumulation of multiple errors
#' in an internal error stack and flushing them all at once later on:
#' - `composerr_halt(err_h1)`: Set the error handling function `err_h1` to
#'   **non-flushing mode**, which means that each time `err_h1(msg)` is called,
#'   the error message (`msg`) gets accumulated in an internal error stack,
#'   but no error is thrown.
#' - `composerr_flush(err_h1)`: If the internal error stack is non-empty,
#'   then create the full error message from the internal error stack and
#'   throw an error.
#'   If the internal error stack is empty, then do nothing.
#' - `composerr_counterr(err_h1)`: Counts the number of times `err_h1` was
#'   called, since `composerr_halt(err_h1)` was called.
#' @inheritSection composerr Cascading error handlers
#' @inheritSection composerr Stacked error messages
#' @param err_h An existing error handling function created with `composerr()`.
#' @inheritParams composerr
#' @seealso [composerr()], [composerr_modify()],
#'   [composerr_get_internal_handler()] and [validate_composerr()]
#' @examples 
#' \dontrun{
#' ##### Example-1 #####
#' # create general error handler
#' err_h1 <- composerr(text_1 = "There are problems")
#' 
#' # create a more precise error handler listing the problems
#' err_h_detail <- composerr("problem", sep_1 = "-", err_h = err_h1)
#' 
#' # halt `err_h_detail()` processing in order to collect
#' # multiple errors
#' composerr_halt(err_h_detail)
#' err_h_detail("1")
#' err_h_detail("2")
#' err_h_detail("3")
#' composerr_flush(err_h_detail)
#' # Error: There are problems:
#' #   - problem-1
#' #   - problem-2
#' #   - problem-3
#' 
#' ##### Example-2 #####
#' # Advanced implementation of vector multiplication
#' # using a validation routine with **advanced error handling**:
#' validate_numeric_vec <- function(obj, err_h) {
#'   obj_name <- deparse(substitute(obj))
#'   err_h <- composerr(paste0("Invalid argument `", obj_name, "`"), err_h)
#'   if (!is.numeric(obj))
#'     err_h("Not a number.")
#'   err_h <- composerr(err_h = err_h)
#'   composerr_halt(err_h)
#'   for (i in seq_along(obj)) {
#'     err_h_item <- composerr(text_1 = paste0("Item-", i), err_h, sep_1 = " is ")
#'     if (is.na(obj[i]) && !is.nan(obj[i]))
#'       err_h_item("NA.")
#'     if (is.nan(obj[i]))
#'       err_h_item("NaN.")
#'     if (is.infinite(obj[i]))
#'       err_h_item("infinite.")
#'   }
#'   composerr_flush(err_h)
#'   invisible(obj)
#' }
#' my_vec_mult2 <- function(x, y) {
#'   err_h <- composerr("In `my_vec_mult2()`")
#'   validate_numeric_vec(x, err_h)
#'   validate_numeric_vec(y, err_h)
#'   if (length(x) != length(y))
#'     err_h("Vectors `x` and `y` have different length.")
#'   sum(x*y)
#' }
#' my_vec_mult2("a", 1:4)
#' # Error: In `my_vec_mult2()`: Invalid argument `x`: Not a number.
#' my_vec_mult2(c(1, NA, NaN, Inf, 5), 1:5)
#' # Error: In `my_vec_mult2()`: Invalid argument `x`:
#' #   - Item-2 is NA.
#' #   - Item-3 is NaN.
#' #   - Item-4 is infinite.
#' my_vec_mult2(1:5, c(NaN, 2, 3, NA, Inf))
#' # Error: In `my_vec_mult2()`: Invalid argument `y`:
#' #   - Item-1 is NA.
#' #   - Item-4 is NaN.
#' #   - Item-5 is infinite.
#' my_vec_mult2(1:5, 1:4)
#' # Error: In `my_vec_mult2()`: Vectors `x` and `y` have different length.
#' my_vec_mult2(1:5, 1:5)
#' 55
#' }
#' @export
#' @rdname composerr_flush
composerr_halt <- function(
  err_h,
  multiflush_start = NULL,
  multiflush_prefix = NULL,
  multiflush_end = NULL,
  internal_handler = NULL
) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_halt()`:", msg), call. = FALSE)
  validate_composerr(
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
    err_h_usage("Argument `internal_handler` must be a function, which has at least one argument.")
  # composerr_halt
  get("halt", envir = environment(err_h))(
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler
  )
}

#' @export
#' @rdname composerr_flush
composerr_flush <- function(
  err_h,
  multiflush_start = NULL,
  multiflush_prefix = NULL,
  multiflush_end = NULL,
  internal_handler = NULL
) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_flush()`:", msg), call. = FALSE)
  validate_composerr(
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
    err_h_usage("Argument `internal_handler` must be a function, which has at least one argument.")
  # composerr_flush
  get("flush", envir = environment(err_h))(
    multiflush_start = multiflush_start,
    multiflush_prefix = multiflush_prefix,
    multiflush_end = multiflush_end,
    internal_handler = internal_handler
  )
}

#' @export
#' @rdname composerr_flush
composerr_counterr <- function(
  err_h
) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_counterr()`:", msg), call. = FALSE)
  validate_composerr(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  # composerr_counterr
  get("counterr", envir = environment(err_h))()
}

#' Retrieve the internal error processing function of an [error handler][composerr()]
#' 
#' @param err_h An error handler created with [composerr()].
#' @return The internal error processing function
#'   `internal_handler()` assigned to `err_h()`.
#'   If `err_h()` was created from another error handling function `err_h_old()`
#'   and `err_h()` has been created without assigning a new `internal_handler`,
#'   then the `internal_handler()` assigned to `err_h_old()` is returned when
#'   calling `composerr_get_handler(err_h)`. This process is repeated
#'   recursively until the first ancestor of `err_h()` which has assigned an
#'   `internal_handler()`.
#' @export
#' @seealso [composerr()], [composerr_modify()], [composerr_flush()], [composerr_halt()]
#'    and [validate_composerr()]
composerr_get_internal_handler <- function(err_h) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_get_internal_handler()`:", msg), call. = FALSE)
  validate_composerr(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  # composerr_counterr
  get("get_internal_handler", envir = environment(err_h))()
}

#' Validate an [error handler][composerr()]
#' 
#' Check that `obj` is really an error handler created with [composerr()].
#' @param obj An error handling function created by `composerr()`.
#' @param err_h An [error handler][composerr()] called when
#'   `obj` is not a valid error handling function created by [composerr()].
#' @param allow_null A logical flag, defining if `obj = NULL` should be allowed.
#' @param obj_name An optional string, defining the variable name of
#'   `obj`. This string is used for creating a meaningful error message,
#'   in case the validation failed. If `obj_name` is omitted, then `obj_name`
#'   is calculated by using **non-standard-evaluation** on `obj`.
#' @export
#' @seealso [composerr()], [composerr_modify()], [composerr_flush()],
#'   [composerr_halt()], [composerr_get_internal_handler()] and
#'   [validate_composerr()]
validate_composerr <- function(
  obj,
  err_h,
  allow_null = TRUE,
  obj_name = NULL
) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `validate_composerr()`:", msg), call. = FALSE)
  if (is.null(obj_name))
    obj_name <- deparse(substitute(obj))
  if (!is.function(err_h) || length(rlang::fn_fmls(err_h)) == 0)
    err_h_usage("Argument `err_h` must be a function, which has at least one argument.")
  if (!is.logical(allow_null) || length(allow_null) != 1 || is.na(allow_null))
    err_h_usage("Argument `allow_null` must be a logical value.")
  if (!is.character(obj_name) || length(obj_name) != 1 || is.na(obj_name))
    err_h_usage("Argument `obj_name` must be a string or omitted.")
  # validate_composerr
  if ((is.null(obj) && isFALSE(allow_null)) || (!is.null(obj) && (
    !is.function(obj) || !"composerr" %in% class(obj)
  )))
    err_h(paste0(
      "`", obj_name,
      "` is not an error handler created by `composerr()`."
    ))
  invisible(obj)
}
