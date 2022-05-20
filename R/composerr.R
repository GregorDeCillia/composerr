#' Compose error handlers (concatenate error messages)
#'
#' `composerr()` is used in order to create so called **error or warning handlers**.
#' These are mere functions, which have a character argument `msg` and maybe
#' additional arguments `...` and which process this
#' arguments (e.g. call `stop(msg)`). 
#' Often error handling and especially the creation of meaningful error
#' messages takes a lot of time and often leads to a great amount of duplicate
#' code, which is often not easy to maintain.
#' `composerr()` offers an easy way out, since it simplifies the creation
#' of error handlers with meaningful error messages, by incrementally concatenating
#' more detailed message parts to your error handlers.
#' The call of `composerr()` does two different things, depending on whether
#' the argument `err_h` is used or not (in the following `...` stands for
#' the arguments `before`, `after`, `collapse`, and `action`):
#' - `composerr(...)`: Create a new error handling function from scratch.
#'   This error handling function is returned by the call `composerr(...)`.
#' - `composerr(..., err_h = err_h_parent)`: Create a new (**child**) error handling
#'   function from another (**parent**) error handling function `err_h_parent`.
#'   This new error handling function enhances `err_h_parent` by adding
#'   additional message parts to the message parts defined in `err_h_parent`.
#' The function `composerr` returns special **composerr class objects**,
#' which are functions with the following structure
#' `function(msg, action = NULL, ...)`. The following arguments are available:
#' - `msg`: A character vector passed to the error handler, holding information
#'   about one or multiple errors.
#' - `action`: A so called **ultimate error handler**. This is a 
#'   function with the following structure `function(msg, ...)`. 
#'   The ultimate error handler processes the error message after it was
#'   fully enriched by all message
#'   parts defined in the so called **error handler cascade**. 
#' - `...`: Additional arguments passed down the error handler cascade and then
#'   passed to the ultimate error handler defined in `action`.
#' @section Cascading error handlers:
#' If `err_h_new <- composerr(...)` was created without passing a parent
#' error handler to the optional argument `err_h`, then
#' `err_h_new` is a newly created error handler with the following structure
#' `err_h_new = function(msg = NULL, action = NULL, ...)`.
#' Similarly, if `err_h_new <- composerr(err_h = err_h_parent, ...)` was created
#' by passing a parent error handler `err_h = err_h_parent`,
#' then `err_h_new` is also a new error handler, which also has the structure
#' `err_h_new = function(msg = NULL, action = NULL, ...)`, but
#' now it was created from a parent error handler `err_h_parent`.
#' 
#' We now consider the latter case
#' `err_h_new <- composerr(before, err_h_parent, after, action = action_default)`.
#' If the error handler gets executed with the following call
#' `err_h_new(msg, action = action_1, ...)`,
#' then the following things will happen:
#' - if the argument `collapse` in `composerr(...)` was set to a string, then
#'   `err_h_new` will first create a more detailed message, by first adding the
#'   text parts defined in the arguments `before` and `after` and then
#'   collapsing the created character vector using the string in `collapse`
#'   as separator
#'   `msg_new <- paste(paste0(before, msg, after), collapse = collapse)`
#' - if the argument `collapse` in `composerr(...)` was omitted or set to `NULL`,
#'   then `err_h_new` will first create a more detailed message, by first adding the
#'   text parts defined in the arguments `before` and `after`, but without
#'   collapsing the resulting vector:
#'   `msg_new <- paste0(before, msg, after)`
#' - if no argument `action` is passed to `err_h_new`
#'   (in this example `action_1 = NULL`),
#'   then `action` will be set to the default ultimate handler
#'   (in this example `action_default`) previously assigned to `err_h_new`
#'   with the argument `action`.
#'   If no default ultimate handler was previously assigned to `err_h_new`,
#'   then `action` will be set to `NULL`.
#' - Finally, `err_h_new` will call `err_h_parent(msg_new, action = action, ...)`
#'
#' Since `err_h_parent` is called at the end and `err_h_parent` will run through
#' the same process, maybe calling its parent error handler, we have a
#' so called **cascade of successive error handler calls**.
#' The following arguments are passed down this error handler cascade:
#' - a successively growing error message `msg` (incrementally adding text parts
#'   in front and at the end of the message).
#' - the argument `action`, which can
#'   either be `NULL` or an ultimate error handling function
#'   used for ultimate processing of the created
#'   error message (throw an error or a warning or do something else).
#'   If the passed down `action` argument is `NULL` and at some
#'   point in the error handling cascade there was a non-null
#'   default ultimate error handler assigned to some error handler
#'   in the error handler cascade, then this default ultimate error handler is
#'   passed on instead as argument `action`. Once the ultimate error handler
#'   `action` is not `NULL` any more, it will remain unchanged, no matter if
#'   there were other default ultimate error handlers assigned to other error
#'   handlers in the error handler cascade. This means the default ultimate
#'   error handlers of the child error handlers overwrite the default ultimate
#'   error handlers of its ancestor error handlers.
#' - additional arguments `...` passed to the call
#'   `err_h_new(msg, action, ...)` will be directly passed down
#'   the error handler cascade and directly passed to the ultimate call
#'   of the ultimate error handler `action(msg_complete, ...)`.
#' 
#' After cascading backwards the ancestry of `err_h_new`, we ultimately reach the
#' primal error handler, which was created first
#' (from scratch) and has no further parent error handler.
#' Let us call it `err_h_primal`.
#' This primal error handler will do the following things:
#' - As usual `err_h_primal` will concatenate its additional text parts
#'   `before` and `after` to the
#'   passed in message. Also it will maybe collapse the resulting character
#'   vector, depending if `collapse` was set to `NULL` or not.
#'   This will give us at last the ultimate version of the error
#'   message. Let us call it `msg_complete`.
#' - As usual `err_h_primal` checks if `action` is `NULL`.
#'   If so, `action` is set to the default handler that was assigned
#'   to `err_h_primal`.
#' - If the `action` is still `NULL`, then `action` is
#'   replaced by the [stop()] function.
#' - Ultimately `err_h_primal` calls `action(msg_complete, ...)`.
#' 
#' The result of the final call `action(msg_complete, ...)` is silently
#' returned by `err_h_new`.
#' @section Stacked error messages:
#' Let us assume `composerr_halt(err_h_new)` was called before calling
#' `err_h_new(msg, ...)`.
#' In this case the error message will not be cascaded down and `action` 
#' will not be called.
#' Instead `err_h_new` will do the following things:
#' - As usual `err_h_new` will concatenate the additional message parts:
#'   `msg_new <- paste0(before, msg, after)`
#' - `err_h_new` will append the resulting error message `msg_new` to an internal
#'   error stack of `err_h_new`. `err_stack <- c(err_stack, msg_new)`
#' - `err_h_new` will return the value `NULL`
#' 
#' If later on, the function
#' `composerr_flush(err_h_new, action = action4, ...)` is
#' called, then the error handler cascade will be executed as usual, but first
#' `err_h_new` will do the following things:
#' - If `collapse != NULL` was assigned to `err_h_new`, then the entire
#'   internal error stack (a character vector holding the stored error messages),
#'   will be collapsed to a single string:
#'   `msg_new <- paste(err_stack, collapse = collapse)`.
#' - If `collapse == NULL` was omitted when calling `err_h_new <- composerr(...)`,
#'   then the internal error stack (a character vector holding the stored error
#'   messages) of `err_h_new` will be used as `msg_new`,
#'   without collapsing it first. 
#' - As usual, if the argument `action = action4` is `NULL`,
#'   then it will be replaced by 
#'   the default ultimate handler assigned to `err_h_new` (in this example
#'   `action_default`).
#' - `err_h_parent(msg_new, action = action)` will be called and
#'   the error handler cascade will continue as usual.
#'   
#' It is also possible to halt the error execution in the
#' **middle of the error handler cascade**, by calling
#' `composerr_halt(err_h_middle)`,
#' where `err_h_middle` is an error handler, that has at least one child 
#' error handler `err_h_new`.
#' If `err_h_new` or another offspring error handler of `err_h_new` gets called,
#' then the error handling will be cascaded down as usual until it reaches
#' `err_h_middle`. There it will be halted and the created message will appended
#' to the internal error message stack of `err_h_middle`.
#' Only by calling `composerr_flush(err_h_middle)` the
#' error handling cascade will be continued.
#' 
#' It is also possible to halt the error execution at
#' **multiple points of the error handler cascade**. In order to cascade
#' the errors down till the end, each halted error handler must be flushed with
#' `composerr_flush()`.
#' @param before Optional string that will be prepended
#'   to the error message `msg`, when the resulting error handler is called.
#' @param err_h Optional **parent error handler**. It must either be `NULL`
#'   or a **composerr class object** created with the function [composerr()].
#'   If `err_h` is omitted, then `composerr()` creates a new error handler from
#'   scratch. Otherwise, the error handler passed to `err_h` is used as
#'   parent error handler for the creation of the new error handler.
#' @param after Optional string that will be appended
#'   to the error message `msg`, when the resulting error handler is called.
#' @param collapse Optional string that will be used for collapsing the
#'   resulting error message `paste0(before, msg, after)`, when
#'   the resulting error handler is called. If `collapse` is set to
#'   `NULL`, then the resulting error message will not be collapsed, but
#'   directly be passed on.
#' @param action Optional **default ultimate error handler**. This argument
#'   can either be `NULL` (no default handler defined) or a function
#'   `function(msg, ...)`. In the latter case this defines a default
#'   function that will be used as ultimate error handler if the current
#'   error handler is called without passing an `action` argument.
#'   The used ultimate error handler is passed down the error handler cascade
#'   until it ultimately will be called in order to process the successively
#'   extended error message (e.g. throw an error or a warning or do something
#'   else with the created error message).
#'   The argument `action` can be used, in order to change the
#'   default behavior of the error processing to throwing a warning
#'   (`action = warning`) or
#'   some other customized behavior like
#'   writing the error message to a text file
#'   (`action = function(msg, ...) cat(msg, file = FILENAME, fill = TRUE)`).
#' @return The call `composerr(...)` always returns a new error handler, either
#'   created from scratch or enhancing a parent error handler `err_h_parent`.
#'   The resulting error handler is a function with the following arguments:
#'   - `msg`: An optional string holding the error message. This error
#'     message will be passed down the error handler cascade and get recursively
#'     concatenated with additional message parts.
#'   - `action`: An optional ultimate error processing function
#'     (`function(msg = NULL, ...)`), which
#'     will be passed down the error handler cascade and ultimately be executed.
#'   - `...`: Additional argument, which will be passed down the error handler
#'     cascade and ultimately passed to `action(msg_full, ...)`.
#' @seealso [composerr_flush()], [composerr_halt()], [composerr_counterr()],
#'   [composerr_get_action()] and [validate_composerr()]
#' @export
#' @examples 
#' \dontrun{
#' #' ##### Example-1: Create error handlers #####
#' # create new error handler `err_h1()` from scratch
#' err_h1 <- composerr(before = "B1", after = "A1")
#' 
#' # `err_h1` throws errors by default
#' err_h1("XXX")
#' # Error: B1XXXA1
#' 
#' # also a vector can be used as message
#' err_h1(c("XXX", "YYY"))
#' # Error: B1XXXA1
#' # B1YYYA1
#' 
#' # With `action = warning` a warning will be sent instead
#' err_h1("XXX", action = warning)
#' # Warning: B1XXXA1
#' 
#' # create a new error handler `err_h2` based on `err_h1`
#' err_h2 <- composerr(
#'   err_h = err_h1,
#'   before = "B2",
#'   after = "A2",
#'   action = message
#' )
#' 
#' # Because of `action = message`, the handler `err_h2` will print
#' # plain messages by default
#' err_h2("XXX")
#' # B1B2XXXA2A1
#' 
#' # The original error handler `err_h1` remained unchanged
#' err_h1("XXX")
#' # Error: B1XXXA1
#' 
#' ##### Example-2: Halt/Flush errors #####
#' # create general error handler
#' err_h_parent <- composerr(before = "There are problems:\n")
#' 
#' # create a more precise error handler listing the problems
#' err_h_child <- composerr("  - problem-", err_h = err_h_parent)
#' 
#' # halt `err_h_child` processing in order to collect multiple errors
#' composerr_halt(err_h_child)
#' err_h_detail(1)
#' err_h_detail(2:3)
#' err_h_detail(4)
#' 
#' composerr_flush(err_h_child)
#' # Error: There are problems:
#' #   - problem-1
#' #   - problem-2
#' #   - problem-3
#' #   - problem-4
#' 
#' ##### Example-3: Error/Warning handling in functions #####
#' my_vec_mult <- function(x, y) {
#'   # create your error handlers
#'   err_h <- composerr("In `my_vec_mult()`: ")
#'   err_h_x <- composerr("Invalid argument `x`: ", err_h)
#'   err_h_y <- composerr("Invalid argument `y`: ", err_h)
#'   if (!is.numeric(x))
#'     err_h_x("Not a number.")
#'   if (any(is.na(x)))
#'     err_h_x("Has missings.", action = warning)
#'   if (!is.numeric(y))
#'     err_h_y("Not a number.")
#'   if (any(is.na(y)))
#'     err_h_y("Has missings.", action = warning)
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
#'   err_h <- composerr(paste0("Invalid argument `", obj_name, "`: "), err_h)
#'   if (!is.numeric(obj))
#'     err_h("Not a number.")
#'   err_h_list <- composerr(err_h = composerr("\n", err_h))
#'   composerr_halt(err_h_list)
#'   for (i in seq_along(obj)) {
#'     err_h_item <- composerr(paste0("  - Item-", i, " is "), err_h_list)
#'     if (is.na(obj[i]) && !is.nan(obj[i]))
#'       err_h_item("NA.")
#'     if (is.nan(obj[i]))
#'       err_h_item("NaN.")
#'     if (is.infinite(obj[i]))
#'       err_h_item("infinite.")
#'   }
#'   composerr_flush(err_h_list)
#'   invisible(obj)
#' }
#' 
#' my_vec_mult2 <- function(x, y) {
#'   err_h <- composerr("In `my_vec_mult2()`: ")
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
#'   "There was a problem: ",
#'   action = function(msg) {
#'     cat(msg, file = logfile, append = TRUE, fill = TRUE)
#'   }
#' )
#' err_h("I am hungry.")
#' err_h("The fridge is empty.")
#' cat(paste(readLines(logfile), collapse = "\n"))
#' # There was a problem: I am hungry.
#' # There was a problem: The fridge is empty.
#' 
#' ##### Example-6: Optionally suppressing warnings #####
#' my_sum <- function(x, suppress_warnings = FALSE) {
#'   if (isFALSE(suppress_warnings)) {
#'     err_h <- composerr("Problem in `my_sum()`: ", action = warning)
#'   } else {
#'     # ultimate error handler does nothing
#'     err_h <- composerr(action = function(...) {})
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
  before = NULL,
  err_h = NULL,
  after = NULL,
  collapse = "\n",
  action = NULL
) {
  composerr_helper(
    before = before,
    err_h = err_h,
    after = after,
    collapse = collapse,
    action = action,
    err_h_usage = function(msg = NULL)
      stop(paste("Error while calling `composerr()`:", msg), call. = FALSE)
  )
}

#' Helper function for [composerr()]
#' 
#' @inheritParams composerr
#' @param err_h_usage An error handling function for catching
#'   errors because of improper usage of `composerr_helper()`.
composerr_helper <- function(
  before,
  err_h,
  after,
  collapse,
  action,
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
  validate_composerr(err_h, allow_null = TRUE, err_h = err_h_usage)
  if (!is.null(before) && (!is.character(before) || length(before) != 1 || is.na(before)))
    err_h_usage("Argument `before` must be a string or omitted.")
  if (!is.null(after) && (!is.character(after) || length(after) != 1 || is.na(after)))
    err_h_usage("Argument `after` must be a string or omitted.")
  if (!is.null(collapse) && (
    !is.character(collapse) || length(collapse) != 1 || is.na(collapse)
  ))
    err_h_usage("Argument `collapse` must be a string or `NULL`.")
  if (!is.null(action) && (
    !is.function(action) || length(rlang::fn_fmls(action)) == 0
  ))
    err_h_usage("Argument `action` must be a function that has at least one argument.")
  # composerr_helper
  if (is.null(err_h) && is.null(action))
    action <- stop
  if(identical(action, stop) || identical(action, warning))
    action <- funky::restrict_fn_env(
      fn = function(msg, ...) {
        do.call(
          action,
          append(
            list(msg), # TODO: continue
            plyr::defaults(
              list(...),
              list(call. = FALSE)
            )
          )
        )
      },
      vars = "action"
    )
  err_h <- funky::eval_closure(
    expr = {
      assigned_before <- NULL
      assigned_after <- NULL
      assigned_collapse <- NULL
      assigned_action <- NULL
      err_stack <- NULL
      flush_halted <- FALSE
      modify <- function(
        before = NULL,
        after = NULL,
        collapse = NULL,
        action = NULL
      ) {
        if (!is.null(before))
          assigned_before <<- before
        if (!is.null(after))
          assigned_after <<- after
        if (!is.null(collapse))
          assigned_collapse <<- collapse
        if (!is.null(action))
          assigned_action <<- action
        invisible(NULL)
      }
      halt <- function(
      ) {
        flush_halted <<- TRUE
        invisible(NULL)
      }
      flush <- function(action = NULL, ...) {
        if (is.null(err_stack))
          return(invisible(NULL))
        if (is.null(action))
          action <- assigned_action
        full_msg <- paste(err_stack, collapse = assigned_collapse)
        flush_halted <<- FALSE
        err_stack <<- NULL
        if (!is.null(err_h)) {
          invisible(err_h(full_msg, action = action, ...))
        } else {
          if (is.null(action))
            action <- stop
          if(identical(action, stop) || identical(action, warning))
            action <- funky::restrict_fn_env(
              fn = function(msg, ...) {
                do.call(
                  action,
                  append(
                    list(msg), # TODO: continue
                    plyr::defaults(
                      list(...),
                      list(call. = FALSE)
                    )
                  )
                )
              },
              vars = "action"
            )
          action(full_msg, ...)
        }
      }
      counterr <- function() {
        invisible(length(err_stack))
      }
      get_action <- function() {
        if (is.null(assigned_action)) {
          if (is.null(err_h))
            stop(
              paste(
                "Error while calling `get_action()`:",
                "There is no parent error handler and no `action()`"
              ),
              call. = FALSE
            )
          get("get_action", envir = environment(err_h))()
        } else {
          invisible(assigned_action)
        }
      }
      function(msg = NULL, action = NULL, ...) {
        err_stack <<- c(
          err_stack,
          paste0(assigned_before, msg, assigned_after)
        )
        if (isFALSE(flush_halted)) {
          flush(action = action, ...)
        } else {
          invisible(NULL)
        }
      }
    },
    vars = "err_h"
  )
  class(err_h) <- "composerr"
  # update (old or new) handler
  get("modify", envir = environment(err_h))(
    before = before,
    after = after,
    collapse = collapse,
    action = action
  )
  invisible(err_h)
}

#' Halting and flushing errors
#' 
#' The following functions allow the accumulation of multiple errors
#' in an internal error stack of an error handler `err_h` and flushing them
#' all at once later on:
#' - `composerr_halt(err_h)`: Set the error handling function `err_h1` to
#'   **non-flushing mode**, which means that each time `err_h1(msg)` is called,
#'   the error message (`msg`) gets accumulated in an internal error stack,
#'   but no error is thrown.
#' - `composerr_flush(err_h)`: If the internal error stack is non-empty,
#'   then create the full error message from the internal error stack and
#'   throw an error.
#'   If the internal error stack is empty, then do nothing.
#' - `composerr_counterr(err_h)`: Counts the number of times `err_h1` was
#'   called, since `composerr_halt(err_h1)` was called.
#' @inheritSection composerr Cascading error handlers
#' @inheritSection composerr Stacked error messages
#' @param err_h An error handler created with [composerr()]
#' @inheritParams composerr
#' @return `composerr_halt()` silently returns the passed in error handler
#'   `err_h` and `composerr_flush()` silently returns the return value of
#'   the ultimate error handler `action`.
#' @seealso [composerr()],
#'   [composerr_get_action()] and [validate_composerr()]
#' @examples 
#' \dontrun{
#' ##### Example-1 #####
#' # create general error handler
#' err_h_parent <- composerr(before = "There are problems:\n")
#' 
#' # create a more precise error handler listing the problems
#' err_h_child <- composerr("  - problem-", err_h_parent)
#' 
#' # halt `err_h_child` processing in order to collect
#' # multiple errors
#' composerr_halt(err_h_child)
#' err_h_child(1)
#' err_h_child(2:3)
#' err_h_child(4)
#' composerr_flush(err_h_child)
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
#'   err_h <- composerr(paste0("Invalid argument `", obj_name, "`: "), err_h)
#'   if (!is.numeric(obj))
#'     err_h("Not a number.")
#'   err_h_list <- composerr(err_h = composerr("\n", err_h))
#'   composerr_halt(err_h_list)
#'   for (i in seq_along(obj)) {
#'     err_h_item <- composerr(paste0("  - Item-", i, " is "), err_h)
#'     if (is.na(obj[i]) && !is.nan(obj[i]))
#'       err_h_item("NA.")
#'     if (is.nan(obj[i]))
#'       err_h_item("NaN.")
#'     if (is.infinite(obj[i]))
#'       err_h_item("infinite.")
#'   }
#'   composerr_flush(err_h_list)
#'   invisible(obj)
#' }
#' my_vec_mult2 <- function(x, y) {
#'   err_h <- composerr("In `my_vec_mult2()`: ")
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
composerr_halt <- function(err_h) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_halt()`:", msg), call. = FALSE)
  validate_composerr(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  # composerr_halt
  get("halt", envir = environment(err_h))()
  invisible(err_h)
}

#' @param ... Additional arguments, which will be passed down the error handler
#'   cascade and ultimately be passed to the ultimate error handler
#'   `action(msg_full, ...)`.
#' @export
#' @rdname composerr_flush
composerr_flush <- function(err_h, action = NULL, ...) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_flush()`:", msg), call. = FALSE)
  validate_composerr(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  if (!is.null(action) && (
    !is.function(action) || length(rlang::fn_fmls(action)) == 0
  ))
    err_h_usage("Argument `action` must be a function, which has at least one argument.")
  # composerr_flush
  get("flush", envir = environment(err_h))(
    action = action,
    ...
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

#' Retrieve the default ultimate error handler of a [composerr error handler][composerr()]
#' 
#' @param err_h An error handler created with [composerr()].
#' @return The **default ultimate error processing function**
#'   `action` assigned to `err_h <- composerr(..., action = my_default_action)`.
#'   If `err_h` is a child error handler of `err_h_parent`
#'   has been created without assigning a default ultimate error handler `action`,
#'   then the default ultimate error handler assigned to `err_h_parent`
#'   is returned by the call `composerr_get_action(err_h)`.
#'   If `err_h_parent` also has no default ultimate error handler assigned to
#'   it, then this process is repeated recursively until the first ancestor
#'   of `err_h` is found that has
#'   a non-null default ultimate error handler `action` assigned to.
#'   If no default ultimate error handler was assigned for any error handler
#'   in the entire error handler cascade, then [stop()] will be the return
#'   value of `composerr_get_action(err_h)`.
#' @export
#' @seealso [composerr()], [composerr_flush()], [composerr_halt()]
#'    and [validate_composerr()]
composerr_get_action <- function(err_h) {
  err_h_usage <- function(msg = NULL)
    stop(paste("Error while calling `composerr_get_action()`:", msg), call. = FALSE)
  validate_composerr(
    obj = err_h,
    err_h = err_h_usage,
    allow_null = FALSE
  )
  # composerr_counterr
  get("get_action", envir = environment(err_h))()
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
#' @seealso [composerr()], [composerr_flush()],
#'   [composerr_halt()], [composerr_get_action()] and
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
