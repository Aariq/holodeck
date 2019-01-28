#' Definition operator
#'
#' Internally, this package uses the definition operator, \code{:=},
#' to make assignments that require computing on the LHS.
#'
#' @importFrom rlang :=
#' @name :=
#' @rdname op-definition
#' @param x An object to test.
#' @param lhs,rhs Expressions for the LHS and RHS of the definition.
NULL


# pipe-able wrapper to diag()
#' Pipe friendly wrapper to `diag(x) <- value`
#'
#' @param x a matrix
#' @param value either a single value or a vector of length equal to the diagonal of `x`.
#'
#' @return a matrix
#' @export
#'
#' @examples
#' library(dplyr)
#' matrix(0,3,3) %>%
#' set_diag(1)
set_diag <- function(x, value){
  diag(x) <- value
  return(x)
}
