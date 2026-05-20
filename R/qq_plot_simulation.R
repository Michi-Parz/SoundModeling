#' Quantile-Comparison Plot between data and a simulation-list.
#'
#' @param data a data vector.
#' @param simulation a list of simulations.
#'
#' @return qq-plot
#'
#' @examples
#' qq_plot_simulation(rnorm(24), replicate(1000, rnorm(24), simplify = F))
#'
#' @details
#' THIS FUNCTION IS OUTDATED! Please use 'qq_plot_sim' instead!
#'

qq_plot_simulation <- function(data, simulation) {
  warning("This function is buggy. Please use 'qq_plot_sim' instead.")
  n <- length(data)

  quant <- lapply(simulation, quantile, probs = seq(0,1,1/(n-1)))
  quant <- lapply(quant, \(x){as.data.frame(t(x))})

  quant <- Reduce(rbind, quant)


  resdf <- data.frame(
    "Data" = sort(data),
    "q_sd" = apply(quant, 2, FUN = sd),
    "median" = apply(quant, 2, FUN = median)
  )

  x <- quantile(resdf$median, probs = c(0.25, 0.75))
  y <- quantile(resdf$Data, probs = c(0.25, 0.75))

  slope <- diff(y) / diff(x)
  intc <- y[1] - slope * x[1]

  resdf$lin <- intc + slope * resdf$median

  ggplot(resdf) +
    geom_point(aes(y = Data, x = median)) +
    geom_ribbon(aes(ymin = lin - q_sd*qnorm(0.84),
                    ymax = lin + q_sd*qnorm(0.84),
                    x = median), alpha = 0.4) +
    geom_ribbon(aes(ymin = lin - q_sd*qnorm(0.975),
                    ymax = lin + q_sd*qnorm(0.975),
                    x = median), alpha = 0.2)+
    geom_ribbon(aes(ymin = lin - q_sd*qnorm(0.995),
                    ymax = lin + q_sd*qnorm(0.995),
                    x = median), alpha = 0.1) +
    geom_line(aes(y = lin, x = median)) +
    labs(x = "Simulation quantiles", y = "Data quantiles")

}

