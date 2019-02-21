#' Get VIP scores from PLS and OPLS models created by `ropls::opls()`
#'
#' Provides a wrapper for \code{\link{getVipVn}} from the \link{ropls} package that returns a tibble rather than a named numeric vector.
#'
#' @param .model a pls object created by \code{\link{opls}}
#'
#' @return a tibble
#'
#' @import ropls
#' @import tibble
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' pls.model <- opls(X, Y)
#' get_VIP(pls.model)
#' }
get_VIP <- function(.model){
  if(.model@typeC == "PCA"){
    stop("VIP scores are only available for (O)PLS(-DA) models")
  }
  getVipVn(.model) %>%
    enframe(name = "Variable") %>%
    rename(VIP = "value")
}

#' Get axis loadings from models created by `ropls::opls()`
#'
#' Provides a wrapper for \code{\link{getLoadingMN}} from the \link{ropls} package that returns a tibble rather than a matrix
#'
#' @param .model a pls object created by \code{\link{opls}}
#' @return a tibble
#'
#' @import ropls
#' @import tibble
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' pls.model <- opls(X, Y)
#' get_loadings(pls.model)
#' }
get_loadings <- function(.model){
  getLoadingMN(.model) %>%
    as_tibble(rownames = "Variable")
}


#' Get axis scores from models created by `ropls::opls()`
#' Returns a dataframe of PC axis scores for PCA, predictive axis scores for PLS and PLS-DA, and predictve and orthogonal axis scores for OPLS and OPLS-DA models.
#'
#' @param model a model object created by `opls()`
#'
#' @return a dataframe
#' @import ropls
#' @import tibble
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' pls.model <- opls(X, Y)
#' get_scores(pls.model)
#' }
get_scores <- function(model){
  #check object type
  if(class(model) != "opls"){
    stop(paste("Expected a model object created by ropls::opls(), but was passed an object of class",
               class(model)[1]))
  }

  if(model@typeC == "PCA"){
    plot_data <-
      model@scoreMN %>%
      as_tibble(rownames = "sample")

  } else if(model@typeC == "PLS-DA"){
    y <-
      model@suppLs$yMCN %>%
      as_tibble(rownames = "sample")
    scores <-
      model@scoreMN %>%
      as_tibble(rownames = "sample")
    plot_data <- full_join(y, scores, by = "sample")

  } else if(model@typeC == "OPLS-DA"){
    #make a OPLS-DA data frame
    pred.scores <-
      model@scoreMN %>%
      as_tibble(rownames = "sample")
    ortho.scores <-
      model@orthoScoreMN %>%
      as_tibble(rownames = "sample")
    scores <-
      full_join(pred.scores, ortho.scores, by = "sample")
    y <-
      model@suppLs$yMCN %>%
      as_tibble(rownames = "sample")
    plot_data <- full_join(y, scores, by = "sample")

  } else if(model@typeC == "PLS"){
    #make a PLS data frame
    scores <-
      model@scoreMN %>%
      as_tibble(rownames = "sample")
    y <-
      model@suppLs$yMCN %>%
      as_tibble(rownames = "sample")
    plot_data <- full_join(y ,scores, by = "sample")

  } else if(model@typeC == "OPLS"){
    #make an OPLS data frame
    pred.scores <-
      model@scoreMN %>%
      as_tibble(rownames = "sample")
    ortho.scores <-
      model@orthoScoreMN %>%
      as_tibble(rownames = "sample")
    scores <-
      full_join(pred.scores, ortho.scores, by = "sample")
    y <-
      model@suppLs$yMCN %>%
      as_tibble(rownames = "sample")
    plot_data <- full_join(y, scores, by = "sample")
  }
  return(plot_data)
}



#' Retrieve model parameters from models created by `ropls::opls()`
#' For PCA, returns percent variance explained by each axis.  For (o)PLS(-DA), returns variance explained by axes and cross-validation statistics.
#'
#' @param model a model object created by `opls()`
#'
#' @return a list of two dataframes, `axis_stats` and `validation`
#' @import ropls
#' @import tibble
#' @import dplyr
#' @export
#'
#' @examples
#' \dontrun{
#' pls.model <- opls(X, Y)
#' get_modelinfo(pls.model)
#' }
get_modelinfo <- function(model){
  #check object type
  if(class(model) != "opls"){
    stop(paste("Expected a model object created by ropls::opls(), but was passed an object of class",
               class(model)[1]))
  } else {
    axis_stats <- model@modelDF
    validation <- model@summaryDF
  }
  return(list("axis_stats" = axis_stats,
              "model_stats" = validation))
}

#' Extract data for plotting (O)PLS(-DA) data with ggplot2
#'
#' Extracts relevant data from an "opls" object for making annotated score plots with ggplot2 or other plotting packages.
#'
#' @param model An object created by \code{\link{opls}}
#' @import dplyr
#' @import ropls
#'
#' @return A list containing dataframes for scores, loadings, axis statistics (%variance explained), and model cross-validation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(ropls)
#' data(sacurine)
#' sacurine.oplsda <- opls(sacurine$dataMatrix, sacurine$sampleMetadata[, "gender"],
#'                         predI = 1,
#'                         orthoI = NA)
#' df <- get_plotdata(sacurine.oplsda)
#' }

get_plotdata <- function(model){
  return(list("scores" = get_scores(model),
              "loadings" = get_loadings(model),
              "axis_stats" = model@modelDF,
              "model_stats" = model@summaryDF))
}
