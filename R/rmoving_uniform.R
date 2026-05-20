#' @title Sample from a moving uniform distribution
#'
#' @param x Sample point
#' @param a minimum value
#' @param b maximum value
#' @param l length of searching area



rmoving_uniform <- function(x, a=-5,b=5,l=sqrt(12)) {
  l <- l/2

  if (a == -Inf & b == Inf) {
    stop("A least one of a and b must be finite!")
  }
  if (a == -Inf) {
    lower <- x-l+b-max(b,x+l)
    upper <- min(b,x+l)
  }
  if (b == Inf) {
    lower <- max(x-l,a)
    upper <- x+l + a - min(a,x-l)
  }
  if (a != -Inf & b != Inf) {
    lower <- max(x-l,a)+b-max(b,x+l)
    upper <- min(b,x+l) + a - min(a,x-l)
  }


  runif(1, min = lower, max = upper)
}
