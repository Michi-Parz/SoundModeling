#' leave one out cross validation on the data under gaussian assumption
#'
#' @param data matrix or data.frame of dimension I times J
#' @param cov_type type of covariance estimation: See ?cov_estimation
#' @param penalty See ?cov_estimation
#' @param alpha Only for SSS. See ?cov_estimation
#'
#' @details
#'
#' Calculates the negative log density for each element of the group
#' where the parameters are estimated with the remaining observations! Lower values are better!
#'
#' The expected value is equal to the entropy!
#'
#'
#' -log(N(\eqn{y_i - \bar{y_{-i}}, \hat{\Sigma}_{-i}}))
#'

loo_cv_data <- function(data, cov_type = c("sd", "sample", "SSS", "LW"),
                        penalty = 1, alpha = 1/100) {
  I <- nrow(data)

  loo_list <- list()

  for (i in 1:I) {
    y_i <- data[i,]
    y_rest <- data[-i,]
    cv_mean <- colMeans(sl_list_stan$y)
    cv_cov <- cov_estimation(y_rest, cov_type, penalty, alpha)

    loo_list[[i]] <- -mvtnorm::dmvnorm(y_i,
                                       mean = cv_mean, sigma  = cv_cov,
                                       log = TRUE)
  }
  unlist(loo_list)
}





