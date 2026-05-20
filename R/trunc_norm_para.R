find_para_truncnorm <- function(x, a,b, tmean, tsd) {
  c(
    tmean - etruncnorm(a, b, x[1], x[2]),
    tsd^2 -  vtruncnorm(a, b, x[1], x[2])
  )
}



#' @title Find µ and \eqn{\sigma} for a truncated gaussian given mean and standard deviation
#'
#' @param mean mean value of the truncated gaussian
#' @param sd standarad deviation of the truncated gaussian
#' @param low lower bound
#' @param upp upper bound
#'
#' @details
#' For the optimization see ?nleqslv::nleqslv
#'


trunc_norm_para <- function(low, upp, mean, sd) {
  zero <- nleqslv::nleqslv(
    c(mean, sd),
    fn = find_para_truncnorm,
    a = low, b = upp,
    tmean = mean, tsd = sd
  )
  c(
    "mu" = zero$x[1],
    "sigma" = zero$x[2]
  )
}



