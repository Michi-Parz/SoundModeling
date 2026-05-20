# Radiation Level acording to DIN EN ISO 12354-1 Anhang B.3 Seite 33


# Help functions-----------------------------

sigma1 <- function(frequency, f_c){
  1/sqrt(1- f_c / frequency)
}

sigma2 <- function(frequency, c0, s){
  4 * s * (frequency / c0)^2
}

sigma3 <- function(frequency, c0, u){
  sqrt(
    (pi*frequency * u) / (16 * c0)
  )
}

f11_value <- function(f_c, c0, u, s){
  ll <- side_lengths_calc(u = u, s = s)

  c0^2 * (1/ll$l1^2 + 1/ll$l2^2) / (4 * f_c)
}


# For radiation case 1
radiation_case_delta <- function(frequency, bound_value, c0, u, s, f_c){
  lambda <- sqrt(frequency/f_c)
  delta1top <- (1-lambda^2)*log((1+lambda)/(1-lambda)) + 2*lambda
  delta1bot <- 4*pi^2 *(1-lambda^2)^(1.5)
  delta1 <- delta1top / delta1bot
  delta2 <- ifelse(frequency > f_c * bound_value,
                   0,
                   8*c0^2*(1-2*lambda^2)/
                     (f_c^2*pi^4 * s * lambda* sqrt(1-lambda^2))
  )
  (u*c0)*delta1/(s*f_c)+delta2
}

# f11 <= f_c/2
radiation_case1 <- function(frequency, lower_rl_bound, upper_rl_bound, delta_bound,
                        c0, u, s, f_c, f11){

  n_freq <- length(frequency)

  res <- rep(NA, n_freq)

  # f >= ub*f_c
  is_u_freq <- frequency >= upper_rl_bound * f_c

  res[is_u_freq] <- sigma1(frequency[is_u_freq], f_c[is_u_freq])

  # f < lb*f_c
  is_l_freq <- frequency < lower_rl_bound * f_c
  l_freq <- frequency[is_l_freq]

  sigma_cd <- radiation_case_delta(l_freq,
                                   bound_value = delta_bound,
                                   c0,
                                   u[is_l_freq],
                                   s[is_l_freq],
                                   f_c[is_l_freq])
  sigma_cd[l_freq < f11[is_l_freq]] <- pmin(sigma_cd,
                                            sigma2(l_freq, c0, s[is_l_freq])
                                            )[l_freq < f11[is_l_freq]]

  res[is_l_freq] <- sigma_cd
  # ll <- unlist(side_lengths_calc(u,s))
  # a <- max(ll)
  # b <- min(ll)
  # 0.45 * sqrt(f_c * u / c0) * (a/b)^(1/4)
  # 2

  ## f \in [lb*f_c, ub*f_c)
  res[is.na(res)] <- 2
  res
}


# f11 > f_c/2
# for length(frequency) = 1
radiation_case2 <- function(frequency, c0, u, s, f_c){

  n_freq <- length(frequency)



  res <- rep(NA, n_freq)

  sig3 <- sigma3(frequency, c0, u)
  sig2 <- sigma2(frequency, c0, s)

  sig2_kl_sig3 <- sig2 < sig3

  sub_case1 <- frequency < f_c & sig2_kl_sig3
  sub_case2 <- frequency > f_c


  sig1 <- sigma1(frequency[sub_case2], f_c[sub_case2])
  sig1_kl_sig3 <- sig1 < sig3[sub_case2]

  sub_case2[sub_case2] <- sub_case2[sub_case2] & sig1_kl_sig3

  res[sub_case1] <- sigma2(frequency[sub_case1], c0, s[sub_case1])
  res[sub_case2] <- sigma1(frequency[sub_case2], f_c[sub_case2])

  remaining <- is.na(res)
  res[remaining] <- sig3[remaining]

  res
}


# Main-Function-----------------------------

# frequency: Input value
# lower_rl_bound: f_c * lower_rl_bound defines the first radiation level region
# upper_rl_bound: f_c * upper_rl_bound defines the last radiation level region
# delta_bound: Defines the region for which delta2 is not zero.
# max_value: Maximum value reached by the radiation level.
# c0: Speed of sound
# u: Perimeter of the wall
# s: Area
# f_c: Coincidence cutoff frequency

radiation_level_din <- function(x,
                            lower_rl_bound = 1,
                            upper_rl_bound = 1,
                            delta_bound = 0.5,
                            max_value = 2,
                            c0 = 340, u = 12.570, s = 9.8,
                            f_c = (4*sqrt(3)*340^2)/(pi * 1250)){

  f11 <- f11_value(f_c = f_c, c0 = c0, u = u, s = s)

  n_freq <- length(x)

  res <- rep(NA, n_freq)

  if (length(u) == 1) {
    u <- rep(u, n_freq)
  }
  if (length(s) == 1) {
    s <- rep(s, n_freq)
  }
  if (length(f_c) == 1) {
    f_c <- rep(f_c, n_freq)
  }
  if (length(f11) == 1) {
    f11 <- rep(f11, n_freq)
  }

  case1_cond <- f11 <= f_c/2

  res[case1_cond] <- pmin(max_value, radiation_case1(x[case1_cond],
                                                       lower_rl_bound,
                                                       upper_rl_bound,
                                                       delta_bound,
                                                       c0,
                                                     u[case1_cond],
                                                     s[case1_cond],
                                                     f_c[case1_cond],
                                                     f11[case1_cond]))

  res[is.na(res)] <- pmin(max_value,
                          radiation_case2(
                            x[!case1_cond], c0,
                            u[!case1_cond], s[!case1_cond],
                            f_c[!case1_cond]))

  res
}







