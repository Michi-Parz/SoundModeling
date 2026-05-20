


radiation_level_forced <- function(frequency, c0 = 340,
                                   l1 = 3.41692, l2 = 2.86808,
                                   max_value = 2){
  lmax <- pmax(l1, l2)
  lmin <- pmin(l1, l2)
  
  k0 <- 2 * pi * frequency / c0
  
  lambda <- -0.964 - (0.5 + lmin/(pi*lmax)) * log(lmin/lmax) +
    (5*lmin)/(2*pi*lmax) - 1/(4*pi*lmax*lmin*k0^2)
  
  
  pmin(
    0.5 * (log(k0*sqrt(lmax*lmin)) - lambda),
    max_value
  )
  
}

