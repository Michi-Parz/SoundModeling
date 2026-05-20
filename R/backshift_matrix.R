#' Quantile-Comparison Plot between data and a simulation-list.
#'
#' @param coeff coefficients
#' @param n matrix dimension
#'
#' @return Backshift-Matrix
#'
#' @examples
#' backshift_matrix(rnorm(5+4+3), 6L)
#'
#' @details
#' the first n-1 values in coeff represent the values of the subdiagional the next n-2 for the sub-subdiagnol and so on
#'
#'


backshift_matrix <- function(coeff, n = 21L) {
  M <- length(coeff)
  res <- matrix(0,n,n)

  i <- j <- 1L

  for (m in 1:M) {
    res[i+j,i] <- coeff[m]
    i <- i+1L
    if (i+j>n) {
      i <- 1L
      j <- j+1L
    }
  }
  res
}
