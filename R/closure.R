#' @include utils.R
NULL

#' Restrict function environment
#' 
#' Build a new function with an optimal scope. Normally the entire
#' environment tree (including the entire ancestry) is kept in memory,
#' but with `restrict_fn_env()` you get a function that has only
#' a copies of all needed variables and the scope ancestry holds only the
#' global environment.
#' @param fn A function whose parent scope should be restricted to the
#'   set of variables given in `vars` and the loaded packages.
#' @param vars An optional object, telling which variables should be
#'   available in `fn()`. It can either be
#'   - a `character` vector holding the names of the variables which should 
#'     looked up in the environment `lookup_env`.
#'   - a named `list`: In this case, the values are not looked up in
#'     `lookup_env`, but directly taken from the list item values and the
#'     list item names are used as variable names.
#' @param lookup_env The environment holding the variables for which the names
#'   are defined in the  character vector `vars`.
#'   If `vars` is a list or `NULL`, then `lookup_env` is not used.
#'   The default for `lookup_env` is the environment where the function `fn`
#'   was defined.
#' @return A new function with a small scope containing the variables given 
#'   in `vars`.
restrict_fn_env <- function(fn, vars = NULL, lookup_env = environment(fn)) {
  err_h <- function(msg)
    stop(paste("Error while calling `restrict_fn_env()`:", msg), call. = FALSE)
  if (!is.function(fn))
    err_h("Argument `fn` must be a function.")
  if (!is.null(vars) && !is.list(vars) && !is.character(vars))
    err_h("Argument `vars` must either be a named list or a character vector or omitted.")
  if (is.list(vars)) {
    names_vars <- names(vars)
    if (is.null(names_vars) || any(is.na(names_vars)))
      err_h("Argument `vars` is a list, but not a named list.")
    id_names_invalid <- which(names_vars != make.names(names_vars))
    if (length(id_names_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a named list, but the names of the following",
        "entries are not valid variable names:\n  ",
        stringify(id_names_invalid)
      ))
    if (length(names_vars) != length(unique(names_vars)))
      err_h("Argument `vars` is a named list, but some names have duplicates.")
  } else if (is.character(vars)) {
    if (any(is.na(vars)))
      err_h("Argument `vars` is character vector, but contains `NA` values.")
    id_vars_invalid <- which(vars != make.names(vars))
    if (length(id_vars_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a character vector, but the following",
        "entries are not valid variable names:\n  ",
        stringify(id_vars_invalid)
      ))
    if (length(vars) != length(unique(vars)))
      err_h("Argument `vars` is a character vector, but has duplicate entries.")
    if (!is.environment(lookup_env))
      err_h(paste(
        "Since argument `vars` is a character vector, then `lookup_env`",
        "must be an environment, but it is not."
      ))
  }
  # restrict
  if (is.character(vars)) {
    names(vars) <- vars
    vars <- lapply(
      vars,
      function(v) get(v, envir = lookup_env)
    )
  }
  new_env <- new.env(parent = .GlobalEnv)
  for(var in names(vars)) {
    assign(
      var,
      vars[[var]],
      envir = new_env
    )
  }
  environment(fn) <- new_env
  fn
}

#' Eval code in closure without scoping problems (memory leaks)
#' 
#' @param vars An optional object, telling which variables should be
#'   available inside the closure. It can either be
#'   - a `character` vector holding the names of the variables which should 
#'     looked up in the environment `lookup_env`.
#'   - a named `list`: In this case, the values are not looked up in
#'     `lookup_env`, but directly taken from the list item values and the
#'     list item names are used as variable names.
#' @param expr The expression, which should be evaluated inside of the
#'   closure.
#' @inheritParams restrict_fn_env
eval_closure <- function(
  expr,
  vars = NULL,
  lookup_env = parent.frame()
) {
  err_h <- function(msg)
    stop(paste("Error while calling `eval_closure()`:", msg), call. = FALSE)
  if (!is.null(vars) && !is.list(vars) && !is.character(vars))
    err_h("Argument `vars` must either be a named list or a character vector or omitted.")
  if (is.list(vars)) {
    names_vars <- names(vars)
    if (is.null(names_vars) || any(is.na(names_vars)))
      err_h("Argument `vars` is a list, but not a named list.")
    id_names_invalid <- which(names_vars != make.names(names_vars))
    if (length(id_names_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a named list, but the names of the following",
        "entries are not valid variable names:\n  ",
        stringify(id_names_invalid)
      ))
    if (length(names_vars) != length(unique(names_vars)))
      err_h("Argument `vars` is a named list, but some names have duplicates.")
  } else if (is.character(vars)) {
    if (any(is.na(vars)))
      err_h("Argument `vars` is character vector, but contains `NA` values.")
    id_vars_invalid <- which(vars != make.names(vars))
    if (length(id_vars_invalid) > 0)
      err_h(paste(
        "Argument `vars` is a character vector, but the following",
        "entries are not valid variable names:\n  ",
        stringify(id_vars_invalid)
      ))
    if (length(vars) != length(unique(vars)))
      err_h("Argument `vars` is a character vector, but has duplicate entries.")
    if (!is.environment(lookup_env))
      err_h(paste(
        "Since argument `vars` is a character vector, then `lookup_env`",
        "must be an environment, but it is not."
      ))
  }
  # eval closure
  if (is.character(vars)) {
    names(vars) <- vars
    vars <- lapply(
      vars,
      function(v) get(v, envir = lookup_env)
    )
  }
  vars[["expr"]] <- substitute(expr)
  new_env <- new.env(parent = .GlobalEnv)
  for(var in names(vars)) {
    assign(
      var,
      vars[[var]],
      envir = new_env
    )
  }
  local(
    tryCatch(
      eval(expr),
      error = function(e) stop(
        "Error while calling `eval_closure()`: ",
        "The following expression could not be evaluated:\n  '",
        deparse(expr),
        "'\n",
        e,
        "Please pass the expression directly to `eval_closure()` and ensure ",
        "that all needed variables are specified in `vars`."
      ),
      finally = rm(expr)
    ),
    envir = new_env
  )
}
