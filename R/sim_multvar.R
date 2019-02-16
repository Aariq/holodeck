#' Simulate categorical data
#'
#' To-do:
#'
#' - Make this optionally create multiple categorical variables as being nested or crossed or random
#'
#' @description This is a simple wrapper that creates a tibble of length `N` with a single column `groups`.  It will warn if there are fewer than three replicates per group.
#' @param .data An optional dataframe.  If a dataframe is supplied, simulated categorical data will be added to the dataframe. Either `.data` or `N` must be supplied.
#' @param N Total number of observations/rows to simulate if `.data` is not supplied.
#' @param n_groups How many groups or treatments to simulate.
#' @param name The column name for the grouping variable.  Defaults to "group".
#'
#' @return a tibble
#'
#' @import tibble
#' @import dplyr
#' @importFrom rlang :=
#' @export
#' @family multivariate normal functions
#' @seealso \code{\link{sim_covar}}, \code{\link{sim_discr}}
#' @examples
#' df <- sim_cat(N = 30, n_groups = 3)
sim_cat <- function(.data = NULL, N = NULL, n_groups, name = "group") {
  name <- sym(name)
  #assert that either .data or N is supplied, not neither nor both
  if(is.null(.data) & is.null(N)) {
    stop("You must supply either '.data' or 'N'")
  }
  if(!is.null(.data) & !is.null(N)) {
    stop("Please supply either '.data' or 'N', not both")
  }
  # if data is supplied, check that it's a dataframe and set N
  if(is.null(N)) {
    assertthat::assert_that(is.data.frame(.data))
    N <- nrow(.data)
  }
  ## Consider moving this check to sim_discr()
  #stopifnots for group size.  Groups should have at least 3 obserations, warn if less than 5.
  if(N/n_groups < 3){
    stop("Not enough replicates per group")
  }
  if(N/n_groups < 5){
    warning("Fewer than 5 replicates per group")
  }
  df <-
    tibble(!!name := rep(letters[1:n_groups], length.out = N)) %>%
    arrange(!!name)
  if(!is.null(.data)){
    df <- bind_cols(.data, df)
  }
  return(df)
}


#' Simulate co-varying variables
#'
#' Adds a group of variables (columns) with a given variance and covariance to a data frame or tibble
#'
#' @param .data An optional dataframe.  If a dataframe is supplied, simulated categorical data will be added to the dataframe. Either `.data` or `N` must be supplied.
#' @param N Total number of observations/rows to simulate if `.data` is not supplied.
#' @param p Number of variables to simulate.
#' @param var Variance used to construct variance-covarinace matrix.
#' @param cov Covariance used to construct variance-covarinace matrix.
#' @param name An optional name to be appended to the column names in the output.
#' @param seed An optional seed for random number generation.  If `NA` (default) a random seed will be used.
#'
#' @return a tibble
#' @family multivariate normal functions
#' @seealso \code{\link{sim_cat}}, \code{\link{sim_discr}}
#' @importFrom MASS mvrnorm
#' @import dplyr
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' sim_cat(N = 30, n_groups = 3) %>%
#' sim_covar(p = 5, var = 1, cov = 0.5, name = "correlated")

sim_covar <- function(.data = NULL, N = NULL, p, var, cov, name = NA, seed = NA) {
  if(is.null(.data) & is.null(N)) {
    stop("You must supply either '.data' or 'N'")
  }
  if(!is.null(.data) & !is.null(N)) {
    stop("Please supply either '.data' or 'N', not both")
  }
  # if data is supplied, check that it's a dataframe and set N
  if(is.null(N)) {
    assertthat::assert_that(is.data.frame(.data))
    N <- nrow(.data)
  }
  if(length(cov) != 1){
    stop("Error: length(cov) not equal to 1")
  }
  if(is.na(seed)){
    seed = as.integer(runif(1) * 10e6)
  }

  S <- matrix(rep(cov, p^2), ncol = p) %>% set_diag(var)

  set.seed(seed)
  sim_matrix <- MASS::mvrnorm(n = N, Sigma = S, mu = rep(0, p))

  #set column names
  if(!is.na(name)){
    colnames(sim_matrix) <- paste0(name, "_", 1:p)
  } else {
    colnames(sim_matrix) <- paste0("V", 1:p)
  }

  sim_data <- sim_matrix %>%
    as_tibble(.name_repair = "check_unique")

  #either make a new df or bind to existing one
  if(!is.null(.data)) {
    new_df <- bind_cols(.data, sim_data)
  } else {
    new_df <- sim_data
  }
  return(new_df)
}



#' Simulate co-varying variables with different means by group
#'
#' To-do: make this work with `dplyr::group_by()` instead of `group =`
#'
#' @param .data A dataframe containing a grouping variable column.
#' @param p Number of variables to simulate.
#' @param var Variance used to construct variance-covarinace matrix.
#' @param cov Covariance used to construct variance-covarinace matrix.
#' @param group_means A vector of the same length as the number of grouping variables.
#' @param name An optional name to be appended to the column names in the output.
#' @param seed An optional seed for random number generation.  If `NA` (default) a random seed will be used.
#'
#' @return a tibble
#'
#' @family multivariate normal functions
#' @seealso \code{\link{sim_cat}}, \code{\link{sim_covar}}
#'
#' @importFrom MASS mvrnorm
#' @importFrom stats runif
#' @import dplyr
#' @import purrr
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' sim_cat(N = 30, n_groups = 3) %>%
#' group_by(group) %>%
#' sim_discr(p = 5, var = 1, cov = 0.5, group_means = c(-1, 0, 1), name = "descr")
sim_discr <- function(.data, p, var, cov, group_means, name = NA, seed = NA){
  if(is.na(seed)){
    seed = as.integer(runif(1) * 10e6)
  }
  N <- nrow(.data)
  group_var <- groups(.data)
  if(is.null(group_var)){
    stop("Please supply a grouping variable with dplyr::group_by()")
    }
  n_groups <- select(.data, !!!group_var) %>% unique() %>% nrow()
  #arrange by grouping variable for easier merging later
  .data <- arrange(.data, !!!group_var)

  #number of observations in each group
  len_groups <-
    .data %>%
    group_by(!!!group_var) %>%
    summarise(lens = n()) %>%
    purrr::pluck("lens")

  if(length(group_means) != n_groups){
    stop(paste("Please supply means for all", n_groups, "groups."))
  }
  if(length(var) != 1 & length(var) != n_groups){
    stop("length(var) should be equal to 1 or n_groups")
  }
  if(length(cov) !=1 & length(cov) != n_groups){
    stop("length(cov) should be equal to 1 or n_groups")
  }
  if(length(var) == 1){
    Var <- rep(var, n_groups)
  }
  if(length(cov) == 1){
    Cov <- rep(cov, n_groups)
  }

  #list of vcov matrices for each group
  S.list <-
    purrr::map2(Cov, Var,
                ~matrix(rep(.x, p^2), ncol = p) %>%
                  set_diag(.y))
  #list of all params for pmap_df()
  l <- list(S = S.list, n = len_groups, mu = group_means)

  set.seed(seed)
  #for each group, use that groups vcov matrix, sample size, and group mean in mvrnorm
  sim_data <-
    pmap_df(l, .f = function(S, n, mu){
      MASS::mvrnorm(n = n,
                    Sigma = S,
                    mu = rep(mu, p)) %>%
        as.data.frame
    })

  if(!is.na(name)){
    colnames(sim_data) <- paste0(name, "_", 1:p)
  }
  new_df <- bind_cols(.data, sim_data)
  return(new_df)
}


#' Simulate missing values
#'
#' Takes a data frame and randomly replaces a user-supplied proportion of values with `NA`.
#' @param .data A dataframe.
#' @param prop Proportion of values to be set to `NA`.
#' @param seed An optional seed for random number generation.  If `NA` (default) a random seed will be used.
#'
#' @return a dataframe with NAs
#'
#' @import dplyr
#' @importFrom stats runif
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' df <- sim_cat(N = 10, n_groups = 2) %>%
#' sim_covar(p = 10, var = 1, cov = 0.5) %>%
#' sim_missing(0.05)
sim_missing <-
  function(.data, prop, seed = NA){ #consider adding vars= to specify what columns.  Pass to select()
    if(is.na(seed)){
      seed = as.integer(runif(1) * 10e6)
    }
    g <- groups(.data)
    .data <- ungroup(.data)

    datadim <- .data %>%
      select_if(is.double) %>%
      dim()

    size = datadim[1] * datadim[2]

    na.num = round(size * prop)
    if(na.num == 0){
      warning("No NA's will be introduced.  Select a larger proportion to introduce NAs")
    }

    set.seed(seed)
    na.vector <- sample(c(rep(NA, na.num), rep(0, size - na.num)), size)

    mask <- matrix(na.vector, nrow = datadim[1], ncol = datadim[2])
    newdata <- .data %>%
      select_if(is.numeric) + mask
    newdf <- bind_cols(select_if(.data, ~!is.numeric(.)), newdata) %>%
      group_by(!!!g)
    return(newdf)
  }
