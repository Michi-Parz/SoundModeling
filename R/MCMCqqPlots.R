#' Converts a \eqn{S \times N} matrix containing chi-squared-distributed samples into a dataframe for qq plots.
#'
#' @param x \eqn{S \times N} matrix
#' @param df degree of freedom for Chisq
#' @param numb_q number of quantiles
#' @param index column names of x
#'


MCMCqqPlotsChi <- function(x, df, numb_q = nrow(x), index = NULL){
  N <- ncol(x)

  if (is.null(index)) {
    index <- 1:N
  }

  if (length(index) != N) {
    stop("Index length must match ncol(x)")
  }

  index <- rep(index, each = numb_q)


  probs <- seq(
    1/(2*numb_q), (2*numb_q-1)/(2*numb_q), len = numb_q
  )

  qqs <- apply(x, 2,quantile, probs = probs)

  theoretical <- qchisq(probs, df = df)


  VarEmpQuantile <- probs*(1-probs)/(nrow(x)*dchisq(theoretical, df)^2)
  StdevEmpQuantile <- sqrt(VarEmpQuantile)


  data.frame(
    "Index" = index,
    "EmpQuantiles" = c(qqs),
    "TheoQuantiles" = rep(theoretical, N),
    "StDevQuantile" = StdevEmpQuantile
  )


}



# x <- matrix(rchisq(4000*142,21), nrow = 4000)
#
# qq <- MCMCqqPlotsChi(x,21, 400)
#
#
# ggplot(qq)+
#   aes(x= TheoQuantiles,group = Index)+
#   geom_line(aes(y= EmpQuantiles), alpha = 1/3, col = "lightblue")+
#   geom_line(aes(y = TheoQuantiles))+
#   geom_line(aes(y = TheoQuantiles + 2*StDevQuantile), linetype = "dotted")+
#   geom_line(aes(y = TheoQuantiles - 2*StDevQuantile), linetype = "dotted")



#debugonce(car::qqPlot)
#car::qqPlot(rchisq(4000, 21), "chisq", df = 21)


