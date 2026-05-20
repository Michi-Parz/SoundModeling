#' minpack.lm::nlsLM including tvAR coefficients
#'
#' @param formula nonlinear model formula like nlsLM
#' @param data data.frame or list.
#' @param start a named list or named numeric vector of starting values.
#' @param max_lag maximum number of lags
#' @param n_iter number of iterations for covariance estimation. (Gets ignored if data_cov=TRUE)
#' @param data_cov If TRUE the covariance matrix is centered by using the mean values
#' @param ... further arguments for nlsLM


nlsLMtvAR <- function(formula, data, start,
                      max_lag = 1L,
                      n_iter = 3L,
                      data_cov = F,
                      ...) {

  return(
    cat("Please use nlsLMcov instead!")
  )

  # Basic definitions
  J <- nrow(data)/21
  mod <- nlsLM(formula, data, start = start,...)
  k <- length(coefficients(mod))
  cov_list <- list()
  cov_est_sq <- diag(J*21)

  # Standard residuals
  residuals <- resid(mod)
  resid_mat <- matrix(residuals, ncol = 21, byrow = T)
  sd_esti <- apply(resid_mat, 2, sd)
  sd_resid_mat <- resid_mat %*% diag(1/sd_esti)
  sd_residuals <- c(t(sd_resid_mat))

  # lag matrix
  lslm <- least_squares_lag_mat(sd_resid_mat, max_lag)
  lslm <- as.data.frame(lslm)

  # AR Fit
  not_the_first <- seq(1, nrow(data), 21)
  target <- sd_residuals[-not_the_first]
  lslm$target <- target
  lag_lm <- lm(target~0+., data = lslm)

  # AR Matrix
  ar_coeffs <- coefficients(lag_lm)
  ar_mat <- backshift_matrix(ar_coeffs)

  ## Todo
  # I - A
  # W = (I-A) * D
  # What <- W
  # Das ganze iter mal wiederholen...
  # Schaue auch auf tvAR bei Kalksandstein/Klasisch... (Wobei jetzt gehts ja um den loop der ist da nicht trotzdem)

  # Wobei darüber nachdenken... wenn ich das jetzt standardisiere sind dann im nächsten Schritt die sd schätzunge nicht einfach 1?
  # Ah ne weil ich es ja wie in nlsLMcov wieder rücktransformiere...

  ar_mat

}





