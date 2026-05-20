#' @title Like dtrunc from the LaplacesDemon Package but it can deal with values outside of the range...


dtrunc2 <- function(x, spec, a = -Inf, b = Inf, log = FALSE, ...){
  y <- rep(NA, length(x))

  support <- x >= a & x <= b

  y[!support] <- ifelse(log, -Inf, 0)

  y[support] <- LaplacesDemon::dtrunc(x[support], spec, a, b, log, ...)

  y

}

dlog_gauss <- function(x, a = -Inf, b = Inf, mu, sigma){
  y <- rep(NA, length(x))

  support <- x >= a & x <= b

  y[!support] <- ifelse(log, -Inf, 0)

  y[support] <- LaplacesDemon::dtrunc(x[support], "norm", a, b, TRUE,
                                      mean = mu, sd = sigma)

  y

}
