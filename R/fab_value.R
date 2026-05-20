

fab_value <- function(f_c, c0, a=1, b=1,
                      u = NULL, s = NULL, l1 = NULL, l2 = NULL){
  u_null <- is.null(u)
  s_null <- is.null(s)
  l1_null <- is.null(l1)
  l2_null <- is.null(l2)
  
  if (sum(u_null, s_null, l1_null, l2_null)>= 3) {
    stop("Two of the parameters u, s, l1 and l2 must be specified!")
  }
  
  if (l1_null & l2_null) {
    ll <- side_lengths_calc(u = u, s = s)
    l1 <- ll$l1
    l2 <- ll$l2
  }
  if (l1_null & u_null) {
    l1 <- s/l2
  }
  if (l2_null & u_null) {
    l2 <- s/l1
  } 
  if (l1_null & s_null) {
    l1 <- u/2 - l2
  }
  if (l2_null & s_null) {
    l2 <- u/2 - l1
  } 
  
  c0^2 * (a^2/l1^2 + b^2/l2^2) / (4 * f_c)
}
