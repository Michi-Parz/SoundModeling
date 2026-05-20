#

formu_final <- RpdB ~ soundproofing_measure(
  eta_int, C, cL,
  w_perimeter = u,
  w_surface = s,
  freq = fpHz,
  fce_gamma = gamma,
  cL1 = 2500,
  plateau_style = "tau0real",
  eta_ref = NULL
)

debugonce(residual_tvAR)
b <- nlsLMcov(formu_final, sldf, c("eta_int" = 0.02,"C" = 2,"cL" = 1000, "gamma" = 0.77),
         cov_type = "tvAR", n_iter = 3,  maxlag = 20)

corrplot::corrplot(cov2cor(b$cov[[1]]), "color")

diag(b$cov[[1]])

b$maxlag
b$cov_type




