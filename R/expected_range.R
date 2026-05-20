min_expga_func <- function(x, N,J){
  a <- N/2
  J*x*(1-pgamma(exp(x), a, 1))^(J-1) * exp(x*a - exp(x))/gamma(a)
}
max_expga_func <- function(x, N,J){
  a <- N/2
  J*x*pgamma(exp(x), a, 1)^(J-1) * exp(x*a - exp(x))/gamma(a)
}

range_expga_func <- function(x, N,J){
  a <- N/2
  J*x*(pgamma(exp(x), a, 1)^(J-1) - (1-pgamma(exp(x), a, 1))^(J-1)) * exp(x*a - exp(x))/gamma(a)
}




expected_min <- function(N,J){
  integrate(min_expga_func, -Inf, Inf, N = N, J=J)$value
}
expected_max <- function(N,J){
  integrate(max_expga_func, -Inf, Inf, N = N, J=J)$value
}


#' Expected range of \eqn{J} random variables \eqn{log(X)}, where \eqn{X} follows a \eqn{Ga(N/2,1)} distribution
#'
#' @param N degrees of freedom
#' @param J Number of observations
#'
#' @details
#' Same functions exits for min and max. Called expected_min and expected_max.
#'


expected_range <- function(N,J){
  integrate(range_expga_func, -Inf, Inf, N = N, J=J)$value
}

