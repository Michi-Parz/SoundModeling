#' Create an overview table for the AIC and BIC
#'
#' @param modlist list of fitted models
#' @return data.frame
#' @export
#'


ic_overview <- function(modlist){
  if (class(modlist) == "nls") modlist <- list(modlist)

  listlen <- length(modlist)

  ll_list <- llse_list <- aic_list <- bic_list <- kic_list <- ksic_list <- list()


  for (i in 1:listlen) {

    mod <- modlist[[i]]

    if (!is.null(mod$resid))
      resid_values <- c(mod$resid) #nlsLMcov
    if (is.null(mod$resid))
      resid_values <- resid(mod) #nlsLM

    resid_mat <- matrix(resid_values, ncol = 21, byrow = T)

    I <- nrow(resid_mat)
    p <- length(coefficients(mod))

    k <- p+231 # p Fit parameter + 21*22/2 covariances

    if (is.data.frame(mod$cov)){
      mod$cov <- as.list(mod$cov)
      mod$cov <- lapply(mod$cov, diag)
      k <- p+21 # p Fit parameter + 21 variances
    }

    if (is.null(mod$cov)) {
      mod$cov <- list(diag(21)*sigma(mod)^2)
      k <- p+1 # p Fit parameter + 1 variance
    }
    if (mod$cov_type == "tvAR") {
      maxlag <- mod$maxlag
      k <- p + 21 + maxlag * (21 - (maxlag+1)/2)
    }


    cov_mat <- tail(mod$cov, 1)[[1]]




    cov_mat_ml <- cov_mat * (I-p)/I

    logL <- mvtnorm::dmvnorm(
      x = resid_mat, sigma = cov_mat_ml, log = T
    )

    logLsum <- sum(logL)


    ll_list[[i]] <- logLsum
    llse_list[[i]] <- sqrt(I) * sd(logL)
    aic_list[[i]] <- -2*logLsum + 2*k
    bic_list[[i]] <- -2*logLsum + log(I)*k
    kic_list[[i]] <- -2*logLsum + 2*p + 2*kappa(cov_mat, exact = T)
    ksic_list[[i]] <- -2*logLsum + 2*p + 2*kappa(cov_mat, exact = T)^(1/2)

  }

  aic <- unlist(aic_list)
  bic <- unlist(bic_list)
  kic <- unlist(kic_list)
  ksic <- unlist(ksic_list)
  ll <- unlist(ll_list)
  llse <- unlist(llse_list)

  res <- data.frame("AIC" = aic,"BIC" = bic,"KappaIC" = kic, "KsqrtIC" = ksic,
                    "negLogL" = -ll, "LL_se" = llse)
  row.names(res) <- names(modlist)

  res
}



