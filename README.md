
# composerr

<!-- badges: start -->

[![GitHub last
commit](https://img.shields.io/github/last-commit/a-maldet/composerr.svg?logo=github)](https://github.com/a-maldet/composerr/commits/master)
[![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/a-maldet/composerr.svg?logo=github)](https://github.com/a-maldet/composerr)
<!-- badges: end -->

``` r
library(composerr)
```

This R package offers easy to use functions that help implementing a
professional error handling, which is maintainable.

## Installation

``` r
# Install development version from GitHub
devtools::install_github('a-maldet/composerr', build_opts = NULL)
```

## Usage

``` r
library(composerr)
```

### Creating advanced error handlers

The call `err_h <- composerr(text_1 = "TEST", sep_1 = ": ")` creates a
`composerr` class object `err_h`. The created object `err_h` is
basically a function `err_h = function(msg)`, which throws an error with
an concatenated error message (`text_1 + sep_1 + msg`) when called.

The resulting error handling function also has an argument
`internal_handler`. This argument defines the internal processing of the
created error message. Usually `intenal_handler = stop`, but you can
define any other internal processing of the passed in error message,
like:

  - throwing an error (**default**): `internal_handler = message`
  - sending a message: `internal_handler = message`
  - sending a warning: `internal_handler = warning`
  - writing the message into a log file: `internal_handler =
    function(msg) cat(msg, FILEPATH)`
  - or any other custom behavior, that can be achieved with a function
    `function(msg) {...}`

By setting

``` r
err_h_x <- composerr("MORE DETAILS", err_h)
```

one can create a new error handler `err_h_x()` from the previously
created error handler `err_h()`. When calling

``` r
err_h_x("PROBLEM DESCRIPTION")
```

then `err_h()` is called by `err_h_x()`, but first the additional
message part of `"MORE DETAILS"` is added to `"PROBLEM DESCRIPTION"`.
Therefore, `err_h_x()` is a **more specific** error handler than the
original error handler `err_h()`. Overwriting

``` r
err_h <- composerr("MORE DETAILS", err_h)
```

is also possible and sometimes useful.

The following **example** shows the implementation of a vector
multiplication function **with complete error handling**:

``` r
my_vec_mult <- function(x, y) {
  # create your error handlers
  err_h <- composerr("In `my_vec_mult()`")
  err_h_x <- composerr("Invalid argument `x`", err_h)
  err_h_x <- composerr("Invalid argument `y`", err_h)
  if (!is.numeric(x))
    err_h_x("Not a number.")
  if (any(is.na(x)))
    err_h_x("Has missings.", internal_handler = warning)
  if (!is.numeric(y))
    err_h_x("Not a number.")
  if (any(is.na(y)))
    err_h_x("Has missings.", internal_handler = warning)
  if (length(x) != length(y))
    err_h("Vectors `x` and `y` have different length.")
  sum(x*y, na.rm = TRUE)
}
my_vec_mult("a", 1:2)
# Error: In `my_vec_mult()`: Invalid argument `x`: Not a number.
my_vec_mult(c(1, NA), 1:2)
# Warning: In `my_vec_mult()`: Invalid argument `x`: Has missings.
# 1
my_vec_mult(1:2, "b")
# Error: In `my_vec_mult()`: Invalid argument `y`: Not a number.
my_vec_mult(1:2, c(1, NA))
# Warning: In `my_vec_mult()`: Invalid argument `y`: Has non-finite values.
# 1
my_vec_mult(1:2, 1:3)
# Error: In `my_vec_mult()`: Vectors `x` and `y` have different length.
my_vec_mult(1:2, 1:2)
# 14
```

### Halt error processing and flush entire error stack at once

Sometimes it is useful to halt the error processing and just collect the
error messages in an internal error stack and flush the entire stack
later on. This can be done by calling:

  - `composerr_halt(err_h)`: Halt the error processing
  - `composerr_flush(err_h)`: Flush the entire error stack at once

The following **example** shows a more advanced implementation of
`my_vec_mult2()` by using a validation routine with a **complete error
handling**:

``` r
validate_numeric_vec <- function(obj, err_h) {
  obj_name <- deparse(substitute(obj))
  err_h <- composerr(paste0("Invalid argument `", obj_name, "`"), err_h)
  if (!is.numeric(obj))
    err_h("Not a number.")
  err_h <- composerr(err_h = err_h)
  composerr_halt(err_h)
  for (i in seq_along(obj)) {
    err_h_item <- composerr(text_1 = paste0("Item-", i), err_h, sep_1 = " is ")
    if (is.na(obj[i]) && !is.nan(obj[i]))
      err_h_item("NA.")
    if (is.nan(obj[i]))
      err_h_item("NaN.")
    if (is.infinite(obj[i]))
      err_h_item("infinite.")
  }
  composerr_flush(err_h)
  invisible(obj)
}
my_vec_mult2 <- function(x, y) {
  err_h <- composerr("In `my_vec_mult2()`")
  validate_numeric_vec(x, err_h)
  validate_numeric_vec(y, err_h)
  if (length(x) != length(y))
    err_h("Vectors `x` and `y` have different length.")
  sum(x*y)
}
my_vec_mult2("a", 1:4)
# Error: In `my_vec_mult2()`: Invalid argument `x`: Not a number.
my_vec_mult2(c(1, NA, NaN, Inf, 5), 1:5)
# Error: In `my_vec_mult2()`: Invalid argument `x`:
#   - Item-2 is NA.
#   - Item-3 is NaN.
#   - Item-4 is infinite.
my_vec_mult2(1:5, c(NaN, 2, 3, NA, Inf))
# Error: In `my_vec_mult2()`: Invalid argument `y`:
#   - Item-1 is NA.
#   - Item-4 is NaN.
#   - Item-5 is infinite.
my_vec_mult2(1:5, 1:4)
# Error: In `my_vec_mult2()`: Vectors `x` and `y` have different length.
my_vec_mult2(1:5, 1:5)
55
```

### List of contained functions

  - `composerr()`: Create new error handling function from scratch.
  - `composerr(err_h = err_h)`: Create new error handling function from
    the previously created error handler `err_h()`.
  - `composerr_modify(err_h)`: Modify the existing error handling
    function `err_h()` without creating a new one.
  - `composerr_halt(err_h)`: Halt error processing and accumulate all
    error messages generated by calling `err_h()` in an error stack.
  - `composerr_flush(err_h)`: Flush entire error stack, holding all
    stacked error messages generated by calling `err_h()`.
  - `composerr_counterr(err_h)`: Number of accumulated error messages
    generated by calling `err_h()`.
  - `composerr_get_internal_handler(err_h)`: Retrieve the internal error
    processing function of `err_h()`.
  - `composerr_validate(err_h)`: Validate that `err_h()` is indeed an
    error handler created by `composerr()`.

## Remark:

If you like this package, please give me a star on github:
<https://github.com/a-maldet/composerr>

## License

GPL-3
