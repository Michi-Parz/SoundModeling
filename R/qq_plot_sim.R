#' Quantile-Comparison Plot between data and a simulation-list.
#'
#' @param x a data vector
#' @param simulation a matrix of simulations
#'
#' @return qq-plot
#'
#' @examples
#' qq_plot_sim(rnorm(24), t(replicate(1000, rnorm(24))))
#'


qq_plot_sim <- function(x, simulation){

  # Anzahl der Simulationen
  n <- length(x)
  qq_probs <- seq(0, 1, length.out = n)

  # Quantile von X berechnen
  obs_quantiles <- quantile(x, probs = qq_probs)

  simulated_quantiles <- t(apply(simulation, 1, quantile, probs = qq_probs))

  # Berechnung des Unsicherheitsbereichs (z.B. 95% CI)

  alphas <- c(0.01,0.05,0.5)

  bounds <- apply(simulated_quantiles, 2, quantile, probs = c(alphas/2, 1-alphas/2))
  expected <- apply(simulated_quantiles,2, mean)


  # Erstellen eines Dataframes für ggplot
  plot_data <- data.frame(
    Quantile = obs_quantiles,
    Expected = expected,
    LB1 = bounds[1,],
    LB2 = bounds[2,],
    LB3 = bounds[3,],
    UB1 = bounds[4,],
    UB2 = bounds[5,],
    UB3 = bounds[6,],
    Position = seq(0, 1, length.out = n)
  )

  # Plot erstellen
  ggplot(plot_data, aes(x = expected, y = Quantile)) +
    geom_line(color = "blue") +
    geom_point(color = "blue")+
    geom_ribbon(aes(ymin = LB3, ymax = UB3), alpha = 0.3) +
    geom_ribbon(aes(ymin = LB2, ymax = UB2), alpha = 0.2) +
    geom_ribbon(aes(ymin = LB1, ymax = UB1), alpha = 0.1) +
    labs(
         x = "Simulated quantiles",
         y = "Observed quantiles") +
    theme_minimal()

}
