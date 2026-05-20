#' Function which is proportional to logarithmic gaussian distribution
#'
#' @param var_values named list of variable values
#' @param target_values vector of response variable
#' @param Sigma Covariance matrix
#' @param para_names vector of variable names
#' @param hyperpara vector or list of hyperparameters
#' @param formu formula
#'
#'



log_poster_gauss <- function(var_values, target_values, Sigma,
                       para_names,
                       hyperpara = c(a = 0, b = 1), formu){
  p <- nrow(Sigma)
  n <- length(target_values)
  m <- n/p

  inv_Sigma <- solve(Sigma)
  inv_Sigma <- kronecker(diag(m), inv_Sigma)


  f_values <- eval(formu[[3L]], envir = var_values)

  maha <- mahalanobis_dist(target_values, f_values, inv_Sigma, inverted = T)

  fit_para <- var_values[para_names]
  fit_para <- unlist(fit_para)

  hyper_a <- hyperpara[["a"]]
  hyper_b <- hyperpara[["b"]]

  -maha/2 - sum( (hyper_a - fit_para)^2 / (2*hyper_b))

}
