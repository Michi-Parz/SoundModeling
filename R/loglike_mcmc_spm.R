# waic_spm <- function(fit_curve_list, cov_list, target){
#
#   lhood <- lapply(loglhood, exp)
#   # dens_marginal
#   # dens_marginal[i] = p(y_i)
#   lhood_marginal <- Reduce('+', lhood)/sample_size
#
#   # lppd
#   lppd <- sum(log(lhood_marginal))
#
#   # E[log p(y|theta)]
#   loglh_marginal <- Reduce('+', loglhood)/sample_size
#   # log p(y|theta) - E[log p(y|theta)]
#   loglhood_centered <- lapply(
#     loglhood, \(x, c){x-c}, c = loglh_marginal
#   )
#   # Var[log p(y|theta)] = E[(log p(y|theta) - E[log p(y|theta)])^2]
#   varpost <- lapply(
#     loglhood_centered,
#     \(x){
#       x^2
#     }
#   )
#   varpost <- sum(Reduce('+', varpost)/(sample_size-1) )
#
#   # WAIC
#   waic <- -2 * lppd + 2 * varpost
#
#   list(
#     "WAIC" = waic,
#     "lppd" = lppd,
#     "pWAIC" = varpost
#   )
# }



#' @title Calculate A S by N log Likelihood matrix
#'
#' @param fit_curve_list a list of mcmc fit curves of length S
#' @param cov_list a list of mcmc covariances of length S
#' @param target a target vector of length N
#' @param cov TRUE if correlation should be included
#'
#' @details
#' Result can be used for loo:loo and loo::waic
#'



loglike_mcmc_spm <- function(fit_curve_list, cov_list, target, cov = T){
  sample_size <- length(fit_curve_list)

  # Check if both lists are of the same length!
  if (sample_size != length(cov_list)) {
    stop("fit_curve_list and cov_list must be of same length!")
  }

  # Transform curves into matrices
  if (!is.matrix(fit_curve_list[[1]]))
    fit_curve_list <- lapply(fit_curve_list, FUN = \(x){
      matrix(x,  nrow = 21)
    })

  # Transform target values into matrices
  if (!is.matrix(target))
    target <- matrix(target, nrow = 21)

  # Calculate the residuals for each mcmc sample
  fit_difference <- lapply(fit_curve_list, \(x,y){x-y}, y = target)

  # Calculate the (log)likelihood for each mcmc sample
  # lhood[[s]][i] = p(y_i|theta_s)
  if (cov){
    loglhood <- purrr::map2(
      fit_difference, cov_list,
      \(res, cov) {
        mvtnorm::dmvnorm(
          t(res), sigma = cov,
          log = TRUE
        )
      }
    )
  }
  if (!cov){
    loglhood <- purrr::map2(
      fit_difference, cov_list,
      \(res, cov) {
        c(dnorm(
          t(res), sd = sqrt(diag(cov)),
          log = TRUE
        ))
      }
    )
  }

  t(Reduce(cbind, loglhood))
}





