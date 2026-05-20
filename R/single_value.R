#' Single value for a given shift
#'
#' @param shift todo
#' @param r measured values
#'
#' @return Single value
#'
#' @examples single_value(0, rnorm(16))


single_value <- function(shift, r) {
  reference <- c(3*(11:17), 52:56, rep(56, 4))
  res <- c()
  i <- 1
  for (s in shift) {
    d <- sum(pmax(reference + s - r,0))
    if (d>32) {
      d <- -1
    }
    res[i] <- d
    i <- i+1
  }
  res
}
