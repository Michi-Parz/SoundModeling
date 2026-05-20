#' Different types of covariance estimation
#'
#' @param data matrix or data.frame of dimension I times J
#' @param cov_type covariance matrix type.
#'  sd: only include variance / standard deviation.
#'  samlple: estimate sample coviarance matrix.
#'  SSS: Simple sample shrinkage: \eqn{\Sigma}(1-\eqn{\alpha}) + D\eqn{\alpha}, D=diag(\eqn{\sigma_1^2},\dots, \eqn{\sigma_J^2})
#'  LW: Ledoit-Wolf linear shrinkage Estimation. See cvCovEst::linearShrinkLWEst
#' @param penalty nrow(data)-penalty degrees of freedom
#' @param alpha in Simple Sample Shrink (SSS). Otherwise ignored.
#' @param ... further arguments for cov or var


cov_estimation <- function(data, cov_type = c("sd", "sample", "SSS", "LW"),
                           penalty = 1, alpha = 1/100, ...){
  cov_type <- match.arg(cov_type)

  I <- nrow(data)
  k <- (I-1)/(I-penalty)


  if (cov_type == "sd") {
    return(
      diag(apply(data, 2,var, ...)*k)
    )
  }

  if (cov_type == "sample") {
    return(cov(data, ...)*k)
  }

  if (cov_type == "SSS") {
    S <- cov(data, ...)*k
    D <- diag(apply(data, 2,var, ...)*k)

    return(S*(1-alpha) + D*alpha)
  }

  if (cov_type == "LW") {
    return(
      cvCovEst::linearShrinkLWEst(data)
    )
  }

}
