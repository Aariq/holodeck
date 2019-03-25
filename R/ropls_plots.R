#to appease R CMD check
p1 <- p2 <- o1 <- y1 <- NULL

#' Plot PCA models created by `ropls::opls()`
#'
#' @param ropls_pca a PCA model produced by `ropls::opls()`
#' @param group_var a discrete variable used to plot groups
#' @param annotate location on the plot to print model statistics
#'
#' @return a ggplot object
#'
#' @import latex2exp
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_pca(pca, data$treatment)
#' }
plot_pca <- function(ropls_pca, group_var = NULL, annotate = c("caption", "subtitle", "none")){
  plotdata <- get_plotdata(ropls_pca)
  if(is.null(group_var)){
    base <- ggplot(plotdata$scores, aes(x = p1, y = p2))
  } else {
    base <- ggplot(plotdata$scores, aes(x = p1, y = p2, color = group_var))
  }
  p <- base +
    geom_point() +
    stat_ellipse() +
    labs(x = paste0("PC1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("PC2 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_colour_discrete("Group Membership") +
    theme_bw() +
    labs(title = "PCA")
  stats <- latex2exp::TeX(
    paste0("$R^2(cumulative) = ", plotdata$model_stats$`R2X(cum)`,
           "$ with ", plotdata$model_stats$pre, " PCs"))

  if(missing(annotate)){
    annotate = "caption"
  }

  out <- switch(annotate,
         caption = p + labs(caption = stats),
         subtitle = p + ggtitle("PCA", subtitle = stats),
         none = p)
  out
}


#' Plot PLS-DA models produced by `ropls::opls()`
#'
#' @param ropls_plsda a PLS-DA model with a discrete Y variable produced by `ropls::opls()`
#' @param annotate location on the plot to print model statistics
#' @return a ggplot object
#'
#' @import latex2exp
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_plsda(plsda)
#' }
plot_plsda <- function(ropls_plsda, annotate = c("caption", "subtitle")){
  plotdata <- get_plotdata(ropls_plsda)
  p <- ggplot(plotdata$scores, aes(x = p1, y = p2, color = y1)) +
    geom_point() +
    stat_ellipse() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("P2 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_discrete("Group Membership") +
    theme_bw() +
    labs(title = "PLS-DA")
  stats <- latex2exp::TeX(
    paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
           "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
           "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$"))

  if(missing(annotate)){
    annotate = "caption"
  }

  out <- switch(annotate,
                caption = p + labs(caption = stats),
                subtitle = p + ggtitle("PLS-DA", subtitle = stats),
                none = p)
  out
}


#' Plot PLS regression models produced by `ropls::opls()`
#'
#' @param ropls_pls a PLS model with a continuous Y variable produced by `ropls::opls()`
#' @param annotate location on the plot to print model statistics
#' @return a ggplot object
#'
#' @import ggplot2
#' @import latex2exp
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_pls(pls)
#' }
plot_pls <- function(ropls_pls, annotate = c("caption", "subtitle")){
  plotdata <- get_plotdata(ropls_pls)
  p <- ggplot(plotdata$scores, aes(x = p1, y = p2, color = y1)) +
    geom_point() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("P2 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = "PLS")
  stats <- latex2exp::TeX(
    paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
           "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
           "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$"))

  if(missing(annotate)){
    annotate = "caption"
  }

  out <- switch(annotate,
                caption = p + labs(caption = stats),
                subtitle = p + ggtitle("PLS", subtitle = stats),
                none = p)
  out
}




#' Plot OPLS regression models produced by `ropls::opls()`
#'
#' @param ropls_pls an OPLS model with a continuous Y variable produced by `ropls::opls()`
#' @param annotate location on the plot to print model statistics
#'
#' @return a ggplot object
#' @import ggplot2
#' @import latex2exp
#' @export
#'
#' @examples
#' \dontrun{
#' plot_opls(opls)
#' }
plot_opls <- function(ropls_pls, annotate = c("caption", "subtitle")){
  plotdata <- get_plotdata(ropls_pls)
  p <- ggplot(plotdata$scores, aes(x = p1, y = o1, color = y1)) +
    geom_point() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("O1 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = "OPLS")
  stats <- latex2exp::TeX(
    paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
           "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
           "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$"))

  if(missing(annotate)){
    annotate = "caption"
  }

  out <- switch(annotate,
                caption = p + labs(caption = stats),
                subtitle = p + ggtitle("OPLS", subtitle = stats),
                none = p)
  out
}


#' Plot OPLS-DA models produced by `ropls::opls()`
#'
#' @param ropls_pls an OPLS-DA model with a discrete Y variable produced by `ropls::opls()`
#' @param annotate location on the plot to print model statistics
#'
#' @return a ggplot object
#' @import ggplot2
#' @import latex2exp
#' @export
#'
#' @examples
#' \dontrun{
#' plot_oplsda(oplsda)
#' }
plot_oplsda <- function(ropls_pls, annotate = c("caption", "subtitle")){
  plotdata <- get_plotdata(ropls_pls)
  p <- ggplot(plotdata$scores, aes(x = p1, y = o1, color = y1)) +
    geom_point() +
    stat_ellipse() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("O1 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_discrete("Group Membership") +
    theme_bw() +
    labs(title = "OPLS-DA")
  stats <- latex2exp::TeX(
    paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
           "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
           "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$"))

  if(missing(annotate)){
    annotate = "caption"
  }

  out <- switch(annotate,
                caption = p + labs(caption = stats),
                subtitle = p + ggtitle("OPLS-DA", subtitle = stats),
                none = p)
  out
}
