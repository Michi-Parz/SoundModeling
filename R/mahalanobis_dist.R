#' Mahalanobis Distance
#'
#' @param x p dimensional vector
#' @param center p dimensional mean vector
#' @param cov \eqn{p\times p} covariance matrix
#' @param inverted if TRUE, cov is supposed to contain the inverse of the covariance matrix.
#' @param chol if TRUE cov is a lower triangle matrix.
#' @param ... further arguments for solve if inverted = FALSE
#'
#' @return Mahalanobis Distance


mahalanobis_dist <- function(x, center, cov, inverted = FALSE, chol = FALSE, ...) {
  y <- x - center
  if (inverted & !chol) return(y%*%cov %*%y)
  if (inverted & chol) {
    z <- cov%*%y
    return(t(z)%*%z)
  }
  if (!inverted & chol) {
    z <- forwardsolve(cov, y)
    return(t(z)%*%z)
  }
  z <- solve(cov, y, ...)
  z%*%y
}

