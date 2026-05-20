#' leave one out cross validation for nlsLMcov function
#'
#' @param formula nonlinear model formula like nlsLM
#' @param data data.frame
#' @param start a named list or named numeric vector of starting values.
#' @param groups column name which indicates the groups
#' @param cov_type type of covariance estimation: See ?nlsLMcov
#' @param maxlag maximum number of lags if cov_type is set to 'tvAR'
#' @param curve_len length of measurement curves
#' @param verbose If TRUE, it is communicated for which curves the calculations have already been completed.
#' @param ... further arguments of nlsLMcov
#'
#' @details
#'
#' Calculates the negative log density for each element of the group
#' where the parameters are estimated with the remaining observations! Lower values are better!
#'
#' The expected value is equal to the entropy!
#'
#'
#' -log(N(\eqn{y_i - g(\hat{\theta}_{-i}), \hat{\Sigma}_{-i}}))
#'


loo_cv_nlsLMcov <- function(formula, data, start, groups,
                   cov_type = c("sd", "sample", "SSS", "LW", "iid", "tvAR"),
                   verbose = TRUE,
                   maxlag = 1,
                   curve_len = 21L,
                   ...){

  group_values <- unique(data[[groups]])
  J <- nrow(data)/curve_len

  loo_cv_sapp <- sapply(
    group_values,
    FUN = loo_cv_build,
    formula = formula,
    data = data,
    groups = groups,
    start = start,
    cov_type = cov_type,
    maxlag = maxlag,
    curve_len = curve_len,
    verbose = verbose,
    ...
  )

  -loo_cv_sapp
}




# help function


loo_cv_build <- function(i, formula, data, groups, start,
                         cov_type, maxlag, curve_len, verbose, ...){
  subl <- data[[groups]] != i
  mod <- nlsLMcov(formula, data[subl,], start, cov_type,
                  maxlag = maxlag, curve_len=curve_len,...)
  cv_data <- data[!subl,]
  # alt: data[rep(data[,groups][!subl], J-1),] (löschen sobald neu funktioniert)
  cv_list <- as.list(cv_data)
  if (cov_type == "sd") {
    cov_iter <- ncol(mod$cov)
    cv_cov <- diag(mod$cov[,cov_iter])
  }
  if (cov_type != "sd" & cov_type != "iid") {
    cv_cov <- tail(mod$cov,1)[[1]]
  }

  if (cov_type != "iid") {
    cv_list$What <- diag(curve_len)
  }

  cv_predict <- c(predict(mod, newdata = cv_list))
  expected_v <- data[[as.character(formula[2])]][!subl]

  if (cov_type == "iid"){
    cv_cov <- sigma(mod)^2 * diag(curve_len)
  }

  if (verbose) cat(i,"\n")
  dmvnorm(cv_predict, expected_v, sigma = cv_cov, log = TRUE)
}






