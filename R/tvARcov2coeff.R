#' transforms a tvAR covariance matrix into a matrix of ar coefficients
#'
#' @param x tvAR covariance matrix
#'

tvARcov2coeff <- function(x){
  n <- ncol(x)
  cx <- t(chol(x))
  icv <- solve(cx)
  sicv <- diag(1/diag(icv))%*%icv
  diag(n)-sicv
}
