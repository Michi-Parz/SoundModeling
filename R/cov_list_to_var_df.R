#' Transforms a list of covariances into a data.frame of variances
#'
#' @param x list of matrices
#' @param df_names row names for data.frame
#' @examples
#' a <- replicate(
#'    1000, expr = {
#'    x <- rnorm(10)
#'    x <- x%*%t(x)
#'    x
#'    },
#'    simplify = F
#'    )
#'
#' cov_list_to_var_df(a)

cov_list_to_var_df <- function(x, df_names = NULL){
  x <- lapply(x, diag)
  x <- lapply(x, t)
  x <- lapply(x, data.frame)
  x <- Reduce(rbind, x)

  if (is.null(df_names))
    return(x)

  names(x) <- df_names
  x
}



