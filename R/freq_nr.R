

freq_nr <- function(f) {
  n <- length(f)
  res <- rep(NA, n)
  
  df <- data.frame("freq" = freq, "pos" = 1:21)
  
  for (i in 1:n) {
   res[i] <- df[df$freq == f[i],]$pos
  }
  res
}

