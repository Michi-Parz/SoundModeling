# u: Perimeter
# s: surface


side_lengths_calc <- function(u, s){
  u0p5 <- u/2
  l1 <- (u0p5 + sqrt(u0p5^2 - 4 *s)) / 2
  l2 <- u0p5 - l1
  
  list("l1" = l1, "l2" = l2)
}
