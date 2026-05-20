# The closer to the border (ucb) the more uncertain you are.
# Therefore, the weighting is weaker the closer you are to this boundary.


## Old version!
#
# uncertainty_smoothing <- function(y, x, ucb, power = 1){
#   if (ucb <= min(x) | ucb >= max(x)) {
#     return(y)
#   }
#   pos <- c(max(which(x < ucb)), min(which(x > ucb)))
#   d <- abs(x[pos] - ucb)
#
#   dy <- diff(y[pos])
#   y[pos] <- y[pos] + power * rev(d) * c(dy, -dy)/sum(d)
#
#   y
# }



sum_of_2_mat <- function(rows){
  m <- matrix(0, nrow = rows, ncol = 2*rows)
  j <- 1
  for (i in 1:rows) {
    m[i,j:(j+1)] <- 1
    j <- j+2
  }
  m
}


uncertainty_smoothing <- function(y,x, ucb, power = 1, log = F) {
  if (max(ucb) <= min(x) | min(ucb) >= max(x)) {
    return(y)
  }
  if (log) {
    x <- log(x)
    ucb <- log(ucb)
  }
  x_zent <- x - ucb
  x_len <- length(x)
  xz_swtich <- which(sign(x_zent[-1]*x_zent[-x_len])<=0)
  new_x <- which(x==min(x))
  xz_swtich <- xz_swtich[!is.element(xz_swtich + 1, new_x)]

  xz_sp1 <- xz_swtich + 1
  pos <- sort(c(xz_swtich, xz_sp1))

  dy <- y[xz_sp1] - y[xz_swtich]
  dy <- rep(dy, each = 2) * rep(c(1,-1), length(dy))

  if (length(ucb) != 1) {
    ucb <- ucb[pos]
  }

  d <- abs(x[pos] - ucb)

  len_d_half <- length(d)/2

  nebendiagmat <- matrix(c(0,1,1,0), 2)

  revd <- c(d%*%kronecker(diag(len_d_half), nebendiagmat) )
  sumd <- c(sum_of_2_mat(len_d_half)%*%d)
  sumd <- rep(sumd, each = 2)

  y[pos] <- y[pos] + power * revd * dy/sumd

  y
}


