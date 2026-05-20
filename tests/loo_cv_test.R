m4pep_formu <- RpdB ~ soundproofing_measure(
  eta_int, C, cL, fce_gamma = gamma,
  plateau_style = "tau0real",
  cL1 = 2500, eta_ref = NULL,
  poisson = 0.25, max_rl = 2,
  max_rl_type = "dynamic",
  w_perimeter = u,
  w_surface = s,
  freq = fpHz
)

testmod <- nlsLMcov(m4pep_formu, data = sldf, start = c("eta_int" = 0.02, "C" = 2, "cL" = 1000, "gamma" = 0.77),
                    cov_type = "sd", lower = c(5/1000,0,0,0))





cov_sd <- loo_cv_nlsLMcov(m4pep_formu, sldf, start = c("eta_int" = 0.02, "C" = 2, "cL" = 1000, "gamma" = 0.77),
       groups = "Building", cov_type = "sd", lower = c(5/1000,0,0,0))

cov_samp <- loo_cv_nlsLMcov(m4pep_formu, sldf, start = c("eta_int" = 0.02, "C" = 2, "cL" = 1000, "gamma" = 0.77),
       groups = "Building", cov_type = "sample", lower = c(5/1000,0,0,0))

cov_sss <- loo_cv_nlsLMcov(m4pep_formu, sldf, start = c("eta_int" = 0.02, "C" = 2, "cL" = 1000, "gamma" = 0.77),
                   groups = "Building", cov_type = "SSS", lower = c(5/1000,0,0,0))

cov_lw <- loo_cv_nlsLMcov(m4pep_formu, sldf, start = c("eta_int" = 0.02, "C" = 2, "cL" = 1000, "gamma" = 0.77),
                   groups = "Building", cov_type = "LW", lower = c(5/1000,0,0,0))

boxplot(cov_sd, cov_samp, cov_sss, cov_lw)

summary(cov_sd)
summary(cov_lw)

mean(cov_sss)
