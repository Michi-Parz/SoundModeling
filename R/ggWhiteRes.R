maxhelp <- function(x, n) {
  x * n * dnorm(x) * pnorm(x)^(n - 1)
}


#' Whitened residual plots
#'
#' @param df data.frame with whintened residuals
#' @param mapping see ?ggplot2::ggplot
#' @param numbCurves number of curves
#' @param numbVariants number of variants
#' @param txtsize text size of line informations
#' @param rmargin size of the right margin
#' @param tikz If TRUE, the percent signs are replaced with p because it is more convenient for TikZ.


ggWhiteRes <- function(df, mapping = aes(),
                       numbCurves = nrow(df)/numbVariants/21, numbVariants = 1L,
                       txtsize = 2.8, rmargin = 80, tikz = F){
  N <- nrow(df)/numbVariants
  I <- numbCurves

  exLocalMax <- integrate(maxhelp, -Inf, Inf, n = I)$value
  exGlobalMax <- integrate(maxhelp, -Inf, Inf, n = N)$value

  median_stdev <- sqrt(pi/(2*I))

  linenames <- c(
    paste("E[min] (N=", N, ")", sep = ""),
    paste("E[min] (N=", I, ")", sep = ""),
    "25 % quantile",
    "SD bands (med.)",
    "75 % quantile",
    paste("E[max] (N=", I, ")", sep = ""),
    paste("E[max] (N=", N, ")", sep = "")
  )

  if (tikz) linenames <- stringr::str_replace(linenames, "%", "p")

  linvalpos <- c(
    exGlobalMax,  exLocalMax, qnorm(0.75)
  )

  ggplot(df, mapping = mapping)+
    geom_hline(yintercept = c(-3:3)*median_stdev, # Sigmabereiche für Median
               color = "black", linetype = "dashed")+
    geom_boxplot()+
    labs(y = "Whitened residuals", x = "Index j")+
    geom_hline(yintercept = qnorm(0.75)*c(-1,1),
               color = "darkgrey",
               linetype ="solid", size = 1.5)+
    geom_hline(yintercept = exLocalMax*c(-1,1), color = "darkgrey",
               linetype ="dashed", size = 0.7)+
    geom_hline(yintercept = exGlobalMax*c(-1,1), color = "darkgrey",
               linetype ="dashed", size = 0.5)+
    annotate("text",
             x = Inf,
             y = c(-linvalpos, 0, rev(linvalpos)),
             label = linenames, size = txtsize,
             hjust = -0.1)+
    coord_cartesian(clip = "off") +
    theme(plot.margin = margin(5.5, rmargin, 5.5, 5.5))

}
