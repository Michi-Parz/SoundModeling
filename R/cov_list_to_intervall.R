#' @title Converts a list of covariance matrices into a matrix representing component-wise interval estimates
#' @param x a list of covariance matrices
#' @param alpha significance niveau
#' @param df_names row names for data.frame

cov_list_to_intervall <- function(x, alpha = 0.05, df_names = NULL){

  n <- nrow(x[[1]])

  x <- lapply(x, cov2cor)


  x <- lapply(x, c)
  x <- Reduce(cbind, x)

  low <-  apply(
    x, 1, FUN = quantile, probs = alpha/2, names = F
  )

  upp <-  apply(
    x, 1, FUN = quantile, probs = 1-alpha/2, names = F
  )

  low <- matrix(low, nrow = n, ncol = n)
  upp <- matrix(upp, nrow = n, ncol = n)

  triangle_mat <- matrix(0, nrow = n, ncol = n)
  triangle_mat[lower.tri(triangle_mat)] <- 1

  res <- triangle_mat * low + t(triangle_mat) * upp + diag(n)
  if (is.null(df_names))
    return(res)

  colnames(res) <- rownames(res) <- df_names

  res
}


