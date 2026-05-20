library(parallel)


qqsimsim <- function(i,s,n, verbose = F){
  x <- rnorm(n)
  sim <- t(replicate(s, rnorm(n)))
  if (verbose)
    cat(i,"\n")
  qq_sim_value(x,sim)
}


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(SoundModeling)
})

qqs2000 <- parSapply(
  cl = cl,
  1:1000,
  qqsimsim,
  s = 2000, n = 24, verbose = T
)
qqs4000 <- parSapply(
  cl = cl,
  1:1000,
  qqsimsim,
  s = 4000, n = 24, verbose = T
)

stopCluster(cl)


qqs2000 <- t(qqs2000)
qqs4000 <- t(qqs4000)

summary(apply(qqs2000, 1, min))
summary(apply(qqs4000, 1, min))



















