#' Performs a whitening under the TVAR assumption
#'
#' @param x a \eqn{I \times J} matrix
#' @param maxlag maximum number of lags
#' @param returnSS return squared sum for each row
#'
#'



whiteningTVAR <- function(x, maxlag, returnSS = T) {
  # center
  x <- scale(x, T,F)
  # Whitening matrix
  whiteMatrix <- residual_tvAR(x, maxlag = maxlag, returnWhite = T)
  # Whitening
  x <- x%*%whiteMatrix

  if (!returnSS) return(x)

  apply(x, 1,\(x){sum(x^2)})
}

