#' Checks if values are out of range
#'
#' @param df a data.frame with lower and upper bounds
#' @param targetName name of the target column
#' @param lowerName name of the lower column
#' @param upperName name of the upper column
#' @param result result format see details
#' @param ncol column length for the case: result == matrix
#'
#' @details
#' \itemize{
#'  \item data.frame: Return is the original data.frame including a boolean column.
#'   TRUE means the target value is not in the range
#'  \item sum: Returns the absolute number of values outside of the range
#'  \item mean: Returns the relative number of values outside of the range
#'  \item matrix: Returns a boolean matrix
#'}




OutOfRange <- function(df, targetName, lowerName, upperName,
                          result = c("data.frame", "sum", "mean", "matrix"),
                       ncol = 21L){

  result <- match.arg(result)

  lower <- df[[lowerName]]
  upper <- df[[upperName]]

  target <- df[[targetName]]

  df$OOR <- !(target >= lower & target <= upper)

  if (result == "data.frame")
    return(df)

  if (result == "sum")
    return(sum(df$OOR))

  if (result == "mean")
    return(sum(df$OOR))

  if (result == "matrix")
    return(matrix(df$OOR, ncol = ncol))

}
