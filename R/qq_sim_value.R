#' Quantile-Comparison values between data and a simulation-list.
#'
#' @param x a data vector
#' @param simulation a matrix of simulations
#'
#' @return \eqn{1-2|p-1/2|}, where  \eqn{p} is the estimated probablity that the observed quantile is smaller than the simulated quantile.
#'
#' @examples
#' qq_sim_value(rnorm(24), t(replicate(1000, rnorm(24))))



qq_sim_value <- function(x, simulation){

  # Anzahl der Simulationen
  n <- length(x)
  qq_probs <- seq(0, 1, length.out = n)

  # Quantile von X berechnen
  obs_quantiles <- quantile(x, probs = qq_probs)

  simulated_quantiles <- t(apply(simulation, 1, quantile, probs = qq_probs))

  res <- c()

  for (i in 1:n) {
    res[i] <- mean(obs_quantiles[i]<= simulated_quantiles[,i])
  }

  1-2*abs(res-1/2)

}
