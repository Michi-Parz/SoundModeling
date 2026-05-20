


#' Create an overview table for the AIC and BIC
#'
#' @param mod_list list of fitted models
#' @param data_list data list used to generate the models
#' @param cov_types Used covariance assumtion.
#'  "iid" (No correlation and identical variance),
#'  "classic" (pearson covariance estimation),
#'  "ls" (shrinkage covariance estimation),
#'  "var" no correlation but different variances.
#'  @param return_ll Should the log likelihood also be returned?
#'
#' @return data.frame
#' @export
#'
aic_bic_overview <- function(mod_list, data_list,
                             cov_types = rep("iid", length(mod_list)),
                             return_ll = TRUE){
  np <- length(data_list$RpdB)
  n <- np/21

  possible_ctypes <- c("iid", "classic", "ls", "var")

  if (!all(is.element(cov_types, possible_ctypes))) {
    stop("'cov_types' must be 'iid', 'classic', 'ls', or 'var'!")
  }


  nplog <- -np*log(2*pi)/2

  aic_list <- list()
  bic_list <- list()
  ll_list <- list()

  for (i in 1:length(mod_list)) {
    resid <- c(mod_list[[i]]$m$resid())

    numb_fits <- length(mod_list[[i]]$m$getPars())

    if (cov_types[i] == "iid") {
      sd_resid <- sd(resid)
      resid <- resid / sd(resid)
      log_det <- np*log(sd_resid)
      k <- numb_fits + 1
    }
    if (cov_types[i] == "classic") {
      #resid <- c(data_list$weightmat %*% resid)
      cov_mat <- data_list$sq_cov%*% data_list$sq_cov
      cov_mat <- cov_mat[1:21, 1:21]
      log_det <- n*log(det(cov_mat))/2
      k <- numb_fits + 22*21/2
    }
    if (cov_types[i] == "ls") {
      #resid <- c(data_list$weightmat_lw %*% resid)
      cov_mat <- data_list$sq_cov_lw%*% data_list$sq_cov_lw
      cov_mat <- cov_mat[1:21, 1:21]
      log_det <- n*log(det(cov_mat))/2
      k <- numb_fits + 22*21/2
    }

    if (cov_types[i] == "var") {
      #resid <- c(data_list$weightmat_lw %*% resid)
      log_det <- sum(log(data_list$var))/2
      k <- numb_fits + 21
    }


    log_lik <- c(nplog - log_det - resid %*% resid/2)

    ll_list[[i]] <- log_lik
    aic_list[[i]] <- -2*log_lik + 2*k
    bic_list[[i]] <- -2*log_lik + log(n)*k
  }

  aic <- unlist(aic_list)
  bic <- unlist(bic_list)
  ll <- unlist(ll_list)

  res <- data.frame("AIC" = aic,"BIC" = bic, "logL" = ll)
  row.names(res) <- names(mod_list)

  if (!return_ll) return(res[,-3])

  res
}

