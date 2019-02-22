#' holodeck: A package for simulating multivariate datasets (and some wrappers for the `ropls` package)
#'
#' The `holodeck` package contains two categories of functions.  Those starting with `sim_` are aimed at simulating multivariate data.  The package also provides some wrappers to the `ropls` package for extracting data from `opls()` models in a tidy framework and some default plots for `opls()` models that use `ggplot2`.
#'
#' What make it 'tidy'?  All `sim_*` functions accept dataframes or tibbles as their first argument and return tibbles, meaning they work with the pipe operator (`%>%`) from the `dplyr` package.
#'
#' @section `sim_*` functions
#'
#'
#' @section Interfacing with the `ropls` package
#'
#' @docType package
#' @name holodeck
NULL
