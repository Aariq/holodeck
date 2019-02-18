#' Plot PCA models created by `ropls::opls()`
#'
#' @param ropls_pca a PCA model with a discrete Y variable produced by `ropls::opls()`
#' @param group_var a variable used to plot groups
#' @param annotate where to put model statistics on the plot
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
plot_pca <- function(ropls_pca, group_var = NULL, annotate = c("caption", "subtitle")){
  plotdata <- tidymvsim::get_plotdata(ropls_pca)
  if(is.null(group_var)){
    base <- ggplot(plotdata$scores, aes(x = p1, y = p2)) #this needs tidyeval help I think.
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
    paste0("$R^2(cumulative) = ", max(plotdata$model_stats$`R2X(cum)`, "$")))
  if(annotate == "caption"){
  p + labs(caption = stats)
  } else if(annotate == "subtitle"){
    p + ggtitle("PCA", subtitle = stats)
  } else {
    p
  }
}


#' Plot PLS-DA models produced by `ropls::opls()`
#'
#' @param ropls_plsda a PLS model with a discrete Y variable produced by `ropls::opls()`
#' @param annotate place to put model statistics on the plot
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
  plotdata <- tidymvsim::get_plotdata(ropls_plsda)
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
  if (annotate == "caption"){
    p + labs(caption = stats)
  } else if(annotate == "subtitle"){
    p + labs(subtitle = stats)
  } else{
    p
  }
}

#' Plot PLS regression models produced by `ropls::opls()`
#'
#' @param ropls_pls a PLS model with a discrete Y variable produced by `ropls::opls()`
#' @param annotate place to put model statistics on the plot
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
  plotdata <- tidymvsim::get_plotdata(ropls_pls)
  p <- ggplot(plotdata$scores, aes(x = p1, y = p2, color = y1)) +
    geom_point() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("P2 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = "PLSR")
  stats <- latex2exp::TeX(
    paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
           "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
           "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$"))
  if (annotate == "caption"){
    p + labs(caption = stats)
  } else if (annotate == "subtitle"){
    p + labs(subtitle = stats)
  } else {
    p
  }
}



#' Plot OPLS regression models produced by `ropls::opls()`
#'
#' @param ropls_pls a PLS model with a discrete Y variable produced by `ropls::opls()`
#' @param annotate place to put model statistics on the plot
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
  plotdata <- tidymvsim::get_plotdata(ropls_pls)
  p <- ggplot(plotdata$scores, aes(x = p1, y = o1, color = y1)) +
    geom_point() +
    labs(x = paste0("P1 (", plotdata$axis_stats$R2X[1] * 100, "%)"),
         y = paste0("P2 (", plotdata$axis_stats$R2X[2] * 100, "%)")) +
    scale_color_viridis_c() +
    theme_bw() +
    labs(title = "OPLSR")
  stats <- latex2exp::TeX(
    paste0("$R^{2}_{Y} = ", plotdata$model_stats$`R2Y(cum)`, "$; ",
           "$Q^{2} = ", plotdata$model_stats$`Q2(cum)`, "$; ",
           "$p_{Q^{2}} = ", plotdata$model_stats$pQ2, "$"))
  if (annotate == "caption"){
    p + labs(caption = stats)
  } else if (annotate == "subtitle"){
    p + labs(subtitle = stats)
  } else {
    p
  }
}
