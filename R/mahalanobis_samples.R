#' Mahalanobis Distance for MCMC, Bootstrap or other samples
#'
#' @param y_samples \eqn{S\times p} samples
#' @param cov_samples \eqn{S\times p\times p} samples of covariance matrices
#' @param cholesky TRUE if matrices are lower triangle matrices
#' @param inverted TRUE if matrices are inverse matrices
#' @param centered TRUE if y_samples are already centered otherwise the samples will be centered
#'
#' @return Mahalanobis distance for samples

mahalanobis_samples <- function(y_samples, cov_samples,
                                cholesky = FALSE, inverse = FALSE,
                                centered = FALSE){

  if (!centered) y_samples <- scale(y_samples, scale = F)
  S <- dim(y_samples)[1]

  center <- colMeans(y_samples)

  sapply(1:S, \(s){
    mahalanobis_dist(y_samples[s,], center, cov = cov_samples[s,,],
                     inverted = inverse, chol = cholesky)
  })
}







