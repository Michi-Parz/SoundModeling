#' Uses simulations/samples to calculate a point-by-point prediction range
#'
#' @title Pointwise prediction interval
#' @param samples a \eqn{S \times I\times J} array of simulations
#' @param alpha The range between \eqn{\alpha/2} and \eqn{1-\alpha/2} is calculated
#' @param qnames Column names for the quantiles
#' @param simultaneous if TRUE alpha is adjusted for simultanious intervals
#' @param data If NULL, only the range is returned, otherwise cbind is also performed


prediction_range <- function(samples, alpha = 0.05,
                             qnames = c("lower", "upper"),
                             simultaneous = FALSE,
                             data = NULL){

  if (simultaneous){
    sim_alpha <- optimize(find_simultaneous_prop, c(0,alpha),
                          samples = samples, goal_prob = 1-alpha)
    alpha <- sim_alpha$minimum
  }


  # long df
  df_lower <- data.frame(apply(samples, c(2,3),
                               quantile, probs  = alpha/2))
  df_upper <- data.frame(apply(samples, c(2,3),
                               quantile, probs  = 1-alpha/2))
  # wide df
  qlower <- tidyr::pivot_longer(df_lower,cols = 1:21,
                                names_to = "fpHz", values_to = "RpdB")

  qupper <- tidyr::pivot_longer(df_upper,cols = 1:21,
                                names_to = "fpHz", values_to = "RpdB")

  # Return df
  n_obs <- nrow(df_lower)
  df <- cbind(rep(freq, n_obs),qlower[,2], qupper[,2])
  names(df) <- c("fpHz",qnames)
  if (is.null(data))
    return(df)

  df <- cbind(data, df[,-1])
  df
}



# Function for simultaneous interval


find_simultaneous_prop <- function(alpha,samples, goal_prob = 0.95, result_prob = FALSE){
  low <- apply(samples, c(2,3),
               quantile, probs  = alpha/2)
  upp <- apply(samples, c(2,3),
               quantile, probs  = 1-alpha/2)
  S <- dim(samples)[1]

  low2 <- replicate(S,low)
  upp2 <- replicate(S,upp)
  low2 <- aperm(low2, c(3,1,2))
  upp2 <- aperm(upp2, c(3,1,2))


  aha <- samples>=low2 & samples <= upp2
  (goal_prob - mean(apply(aha, 1:2, all)))^2
}











