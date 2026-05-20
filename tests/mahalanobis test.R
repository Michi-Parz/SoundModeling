S <- 10000

mat <- matrix(0, nrow = 21, ncol = 21)

for (i in 1:21) {
  for (j in 1:21) {
    mat[i,j] <- 0.9^abs(i-j)
  }
}


sim <- rmvnorm(S, rep(0,21), sigma = mat)

# Cov
mat_rep <- replicate(S, mat)
mat_rep <- aperm(mat_rep, c(3,1,2))

# Chol cov
cmat <- t(chol(mat))
cmat_rep <- replicate(S, cmat)
cmat_rep <- aperm(cmat_rep, c(3,1,2))

# Cov inverse
inv_mat <- solve(mat)
inv_matr <- replicate(S, inv_mat)
inv_matr <- aperm(inv_matr, c(3,1,2))

# Cov inverse
inv_cmat <- solve(cmat)
inv_cmatr <- replicate(S, inv_cmat)
inv_cmatr <- aperm(inv_cmatr, c(3,1,2))


results1 <- mahalanobis_samples(sim, mat_rep)
results2 <- mahalanobis_samples(sim, cmat_rep, cholesky = TRUE)
results3 <- mahalanobis_samples(sim, inv_matr, inverse = TRUE)
results4 <- mahalanobis_samples(sim, inv_cmatr, cholesky = TRUE, inverse = TRUE)

range(results1-results2)
range(results1-results3)
range(results1-results4)








