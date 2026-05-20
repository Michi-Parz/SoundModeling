#' What should be taken into account if the calculations are to be carried out strictly in accordance with the standard?


strict_to_standard <- function(){
  cat("If sts == TRUE then

      \t - fc is calculated with 1/1.8 instead of sqrt(3)/pi.
      \t - cLex = 1.1cL
      \t - if gamma != NULL: 3.6 / (1-poisson) in f_c_eff is replaced by 4.8


      and YOU shold set

      \t - sts == TRUE
      \t - fce_gamma to NULL (c) or 0.77 (s)
      \t - lower_rl_bound = 1
      \t - upper_rl_bound = 1
      \t - smoothing_log = FALSE
      \t - smoothing_power to 0 (c) or 2/3 (s)
      \t - rho0 = 1.29
      \t - max_rl to 1.18 or 2 (c) or 2 (s)
      \t - max_rl_level to 'value' (c) or to 'dynamic' (s)

      Here (c) is short for the current ISO 12354-1 (2017) version and (s) for the DAGA25 suggestions by Hoeller et al.
      ")
}


