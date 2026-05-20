#' \eqn{J(\log(2\pi)+1)/2 + \log|\Sigma|/2}
#'
#' A details of entropy_multi_gauss
#'
#' @title entropy_multi_gauss:
#' @param x A nls model or a matrix or a list
#' @rdname entropy_multi_gauss
#' @export entropy_multi_gauss
entropy_multi_gauss <- function (x) {
  UseMethod("entropy_multi_gauss")
}


#'
#' @rdname entropy_multi_gauss
#' @method entropy_multi_gauss nls
#' @exportS3Method fit_info_tab nls
entropy_multi_gauss.nls <- function(x){
  cov_estimations <- x$cov

  if (is.null(cov_estimations)) {
    cov_iid <- sigma(x)^2*diag(21)
    return(entropy_multi_gauss(cov_iid))
  }

  if (is.data.frame(cov_estimations)) {
    iter <- ncol(cov_estimations)
    cov_sd <- diag(cov_estimations[,iter])
    return(entropy_multi_gauss(cov_sd))
  }

  cov <- tail(cov_estimations, 1)[[1]]
  entropy_multi_gauss(cov)
}


#'
#' @rdname entropy_multi_gauss
#' @method entropy_multi_gauss matrix
#' @exportS3Method entropy_multi_gauss matrix
entropy_multi_gauss.matrix <- function(x){
  J <- ncol(x)
  J*(log(2*pi)+1)/2 + log(det(x))/2
}




#'
#' @rdname entropy_multi_gauss
#' @method entropy_multi_gauss list
#' @exportS3Method entropy_multi_gauss list
entropy_multi_gauss.list <- function(x){
  sapply(x, entropy_multi_gauss)
}




