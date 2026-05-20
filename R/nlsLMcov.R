#' minpack.lm::nlsLM including a (co)variance estimation
#'
#' @param formula nonlinear model formula like nlsLM
#' @param data data.frame or list.
#' @param start a named list or named numeric vector of starting values.
#' @param cov_type
#'\itemize{
#'  \item sd: only include variance / standard deviation.
#'  \item samlple: estimate sample coviarance matrix.
#'  \item SSS: \eqn{\Sigma}(1-\eqn{\alpha}) + D\eqn{\alpha}, D=diag(\eqn{\sigma_1^2},\dots, \eqn{\sigma_J^2})
#'  \item LW: Ledoit-Wolf linear shrinkage Estimation. See cvCovEst::linearShrinkLWEst
#'  \item iid: iid error which is equal to nlsLM
#'  \item tvAR: time varying autoregressive covariance structure
#'}
#'
#' @param n_iter number of iterations for covariance estimation. (Gets ignored if data_cov=TRUE)
#' @param sss_alpha \eqn{\alpha} in Simple Sample Shrink (SSS). Otherwise ignored.
#' @param maxlag maximum number of lags for tvAR assumption
#' @param data_cov If TRUE the covariance matrix is centered by using the mean values
#' @param curve_len length of measurement curves
#' @param ... further arguments for nlsLM
#'
#' @details
#' All covariances use: I-k degrees of freedom.
#' Where I is the number of measurement curves an k the number of fit parameters.
#'
#' First, a model without covariance assumption is estimated.
#' Then the residuals of this model are used for the covariance estimation.
#' This process is repeated n_times.


nlsLMcov <- function(formula, data, start,
                     cov_type = c("sd", "sample","SSS", "LW", "iid", "tvAR"),
                     n_iter = 3,
                     sss_alpha = 1/10000,
                     maxlag = 1,
                     data_cov = F,
                     curve_len = 21L,
                     ...) {

  # Basic definitions
  cov_type <- match.arg(cov_type)
  I <- nrow(data)/curve_len
  mod <- nlsLM(formula, data, start = start,...)
  if (cov_type == "iid") return(mod)
  k <- length(coefficients(mod))
  cov_list <- list()
  cov_est_sq <- diag(I*curve_len)

  data <- as.list(data)

  # Adjust fomula
  formula2 <- paste(
    "What%*%",formula[2], "~ What %*%", formula[3], sep = ""
  )
  formula2 <- as.formula(formula2)

  if (data_cov) n_iter <- 1L

  # Iterations
  for (i in seq_len(n_iter)) {
    # Residual matrix
    if (!data_cov) {
      resid <- residuals(mod)
      resid <- cov_est_sq %*% resid
      res_matrix <- matrix(resid, ncol = curve_len, byrow = T)
    }
    if (data_cov) {
      target_name <- as.character(formula[2])
      target_values <- data[[target_name]]
      target_matrix <- matrix(target_values, ncol = curve_len, byrow = T)
      res_matrix <- scale(target_matrix, scale = F)
    }

    # covariance
    cov_est <-  resid_to_cov(res_matrix, covtype = cov_type,
                             k = k, alpha = sss_alpha, maxlag = maxlag)
    cov_list[[i]] <- cov_est

    cov_est_sq <- t(chol(cov_est))
    weight_mat <- solve(cov_est_sq)


    cov_est_sq <- kronecker(diag(I),cov_est_sq)
    weight_mat <- kronecker(diag(I),weight_mat)

    data$What <- weight_mat

    mod <- nlsLM(formula2, data, start = coefficients(mod),...)
  }
  mod$cov <- cov_list
  mod$cov_matrix <- cov_list[[n_iter]]
  if (cov_type == "sd") {
    var_df <- data.frame(lapply(mod$cov, diag))
    names(var_df) <- paste("Iter", 1:n_iter, sep = "")
    row.names(var_df) <- freq
    mod$cov <- var_df
  }

  mod$fitted <- cov_est_sq %*% fitted(mod)
  mod$resid <- cov_est_sq %*% resid(mod)
  mod$cov_type <- cov_type
  if (cov_type == "tvAR") mod$maxlag <- maxlag

  mod
}



#' Transforms a matrix of residuals into a covariance matrix
#'
#' @param resmat a matrix of residuals or other centered values
#' @param covtype covariance matrix type.
#'  sd: only include variance / standard deviation.
#'  samlple: estimate sample coviarance matrix.
#'  SSS: Simple sample shrinkage: \eqn{\Sigma}(1-\eqn{\alpha}) + D\eqn{\alpha}, D=diag(\eqn{\sigma_1^2},\dots, \eqn{\sigma_J^2})
#'  LW: Ledoit-Wolf linear shrinkage Estimation. See cvCovEst::linearShrinkLWEst
#'  tvAR: time-varying autoregressive assumption with 'maxlag'
#'
#' @param k nrow(resmat)-k degrees of freedom
#' @param maxlag maximum number of lags for tvAR assumption
#'
resid_to_cov <- function(resmat, covtype, k = 0, alpha = 1/100, maxlag = 1) {
  n <- nrow(resmat)
  if (covtype == "sd") {
    return(
      diag(diag(t(resmat)%*%resmat)/(n-k))
    )
  }
  if (covtype == "sample") {
    return(
      t(resmat)%*%resmat/(n-k)
    )
  }
  if (covtype == "SSS") {
    S <- t(resmat)%*%resmat/(n-k)
    D <- diag(diag(S))

    return(
      S*(1-alpha) + D*alpha
    )
  }
  if (covtype == "LW") {
    return(residual_LWcov(resmat,k))
  }
  if (covtype == "tvAR") {
    residual_tvAR(resmat, maxlag, k)
  }
}

#' Transforms a matrix of residuals into a Ledoit-Wolf covariance matrix
#'
#' @param resmat a matrix of residuals or other centered values
#' @param k nrow(resmat)-k degrees of freedom
#'
#' @details
#' See ?cvCovEst::linearShrinkLWEst
#'

residual_LWcov <- function(resmat, k = 1){
  p_n <- ncol(resmat)
  n <- nrow(resmat)
  sample_cov_mat <- t(resmat)%*%resmat/(n-k)
  idn_pn <- diag(p_n)
  #dat <- as.matrix(dat)
  m_n <- matrixStats::sum2(sample_cov_mat * idn_pn)/p_n
  d_n_2 <- matrixStats::sum2((sample_cov_mat - m_n * idn_pn)^2)/p_n
  b_bar_n_2 <- apply(resmat,
                     1, \(x) {
                       matrixStats::sum2((tcrossprod(x) - sample_cov_mat)^2)
                     })
  b_bar_n_2 <- 1/n^2 * 1/p_n * sum(b_bar_n_2)
  b_n_2 <- min(b_bar_n_2, d_n_2)
  estimate <- (b_n_2/d_n_2) * m_n * idn_pn + (d_n_2 - b_n_2)/d_n_2 *
    sample_cov_mat
  return(estimate)
}


#' Transforms a matrix of residuals into a tvAR covariance matrix
#'
#' @param resmat a matrix of residuals or other centered values
#' @param maxlag maximum number of lags
#' @param k nrow(resmat)-k degrees of freedom
#' @param curve_len length of measurement curves
#' @param returnWhite return whitening matrix
#'
#'

residual_tvAR <- function(resmat, maxlag = 1, k = 1, returnWhite = F){
  # Standardization
  # sd_esti <- apply(resmat, 2, sd)
  # sd_resmat <- resmat %*% diag(1/sd_esti)
  # sd_residuals <- c(t(sd_resmat))

  #if (!is.null(bs_weights)) bs_weights <- rep(bs_weights, each = maxlag)

  curve_len <- ncol(resmat)

  resvec <- c(t(resmat))

  # lag matrix
  lslm <- least_squares_lag_mat(resmat, maxlag) # sd_resmat or resmat
  lslm <- as.data.frame(lslm)

  # AR Fit
  not_the_first <- seq(1, nrow(resmat)*curve_len, curve_len)
  target <- resvec[-not_the_first] # resvec or sd_residuals
  lslm$target <- target
  lag_lm <- lm(target~0+., data = lslm)

  # AR Matrix
  ar_coeffs <- coefficients(lag_lm)
  ar_mat <- backshift_matrix(ar_coeffs, curve_len)

  # Difference matrix
  diff_mat <- diag(curve_len)-ar_mat

  # Cov mat
  sd_esti <-  apply((diag(curve_len) - ar_mat)%*%t(resmat), 1, sd)

  if (returnWhite) {
    return(
      t(diff_mat)%*%diag(1/sd_esti)
    )
  }

  sd_mat <- diag(sd_esti)



  chol_cov_mat <- solve(diff_mat)%*%sd_mat
  chol_cov_mat%*%t(chol_cov_mat)

}
