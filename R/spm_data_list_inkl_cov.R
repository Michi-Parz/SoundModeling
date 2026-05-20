#' A description of spm_data_list_inkl_cov
#'
#' A details of spm_data_list_inkl_cov
#'
#' @title spm_data_list_inkl_cov: Title
#' @param data data.frame with sound reduction index values
#' @param methods Details will follow, but you can specify here which estimators are to be calculated.
#' @rdname spm_data_list_inkl_cov
#' @export spm_data_list_inkl_cov
spm_data_list_inkl_cov <- function(data,
                                   methods = c("cov", "lwls", "var", "mean")){
  data_list <- as.list(data)
  numb_curves <- nrow(data)/21

  if (any(methods == "cov") | any(methods == "var")) {
    cov_mat <- r_korr_given_freq(data, "cov")
  }

  if (any(methods == "cov")) {
    sq_cov_mat <- expm::sqrtm(cov_mat)
    sq_prec_mat <- solve(sq_cov_mat)
    data_list$weightmat <- kronecker(diag(numb_curves),sq_prec_mat)
    data_list$sq_cov <- kronecker(diag(numb_curves), sq_cov_mat)
  }

  if (any(methods == "lwls")) {
    cov_mat_lwls <- r_korr_given_freq(data, "lwls")
    sq_cov_mat_lwls <- expm::sqrtm(cov_mat_lwls)
    sq_prec_mat_lwls <- solve(sq_cov_mat_lwls)
    data_list$weightmat_lw <- kronecker(diag(numb_curves),sq_prec_mat_lwls)
    data_list$sq_cov_lw <- kronecker(diag(numb_curves), sq_cov_mat_lwls)
  }

  if (any(methods == "var")) {
    data_list$var <- rep(diag(cov_mat), 24)
    data_list$sd <- sqrt(data_list$var)
  }

  if (any(methods == "mean")) {
    data_list$mean <- rep(r_mean_given_freq(data), 24)
  }

  data_list
}
