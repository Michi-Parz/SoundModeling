#' Linear continuation as soon as a limit is undershot.
#'
#' @param y Data vector
#' @param limit Limit value for y at which the values are no longer credible
#' @param orientation Number of observations used to continue the data linearly.


linear_continuation <- function(y, limit = 10, orientation = 5){
  under_lim <- y <= limit

  if (sum(under_lim) == 0)
    return(y)

  len <- length(y)

  first_b <- which.max(under_lim)

  line_end <- first_b - 1
  line_start <- line_end - orientation + 1

  indx <- line_start:line_end

  remain <- (1+line_end):len

  y_sub <- y[indx]

  lcont <- lm(y_sub~indx)

  y_new <- y

  y_new[remain] <- predict(lcont, data.frame(indx = remain))

  y_new
}
