#' Calculate single value Rw
#'
#' @param r measured values
#' @param digits number of digits
#' @param min_s minimal shift
#' @param lvl relevant for recursive execution
#'
#' @return Optimal shift
#' @export rw_value()
#'
#' @examples rw_value(rnorm(16))


rw_value <- function(r, digits = 1, min_s = -100, lvl = 1) {
  len_r <- length(r)
  if (len_r != 21 & len_r != 16) stop("r must be of length 16 or 21")
  if (len_r == 21) r <- r[4:19]
  reference <- c(3*(11:17), 52:56, rep(56, 4))
  s <- min_s
  m <- -1
  d <- 0
  while (d>-1) {
    m <- m+1
    d <- sum(pmax(reference + s + m*10^lvl - r,0))
    if (d>32) {
      d <- -1
    }
  }
  if (lvl > -digits) {
    s <- s + (m-1)*10^lvl
    lvl <- lvl - 1
    return(
      rw_value(r, digits = digits, min_s = s, lvl = lvl)
    )
  }
  52 + s + (m-1)*10^lvl
}


