# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/testing-design.html#sec-tests-files-overview
# * https://testthat.r-lib.org/articles/special-files.html

library(testthat)
library(SoundModeling)

test_check("SoundModeling")



library(parallel)
cl <- makeCluster(detectCores(), outfile = "test.txt")
clusterEvalQ(cl, {
  library(SoundModeling)
  # Weitere Pakete, die du verwenden willst
}) # Das Paket auf allen Knoten laden


was <- function(x){
  print(x)
  x+1
}


test <- parLapply(
  cl,
  1:4000,
  was
)



