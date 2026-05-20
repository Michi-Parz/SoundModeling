#' Transforms a \eqn{I \times J} matrix into a LSL-Matrix
#'
#' @param y \eqn{I \times J} matrix
#' @param maxlag maximum number of lags
#'
#' @description
#' result is a \eqn{I (J-1)\times Q(maxlag)} matrix where
#'
#' \eqn{Q(l) = l(J-(l+1)/2)}
#'


least_squares_lag_mat <- function(y, maxlag){
  I <- nrow(y)
  J <- ncol(y)
  Bi <- NULL
  B <- NULL

  for (i in 1:I) {
    for (l in 1:maxlag) {
      nullmat <- matrix(0, l-1, J-l)
      if (J-l != 1) {
        help <- rbind(nullmat, diag(y[i,1:(J-l)]))
      }
      if (J-l == 1) {
        help <- rbind(nullmat, y[i,1])
      }
      Bi <- cbind(Bi, help)
    }
    B <- rbind(B,Bi)
    Bi <- NULL
  }
  B
}
