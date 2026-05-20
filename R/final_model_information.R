#' Informations about the final Version(s) of the sound reduction index model!


final_model_information <- function(){
  cat("
      \t - Fit parameter: internal_loss, lab_loss_factor, cL, fce_gamma
      \t - plateau_style  = 'tau0real'
      \t - cL1 = 2500
      \t - eta_ref = NULL
      \t - poisson = 0.25 (CaSi)
      \t - max_rl_type = 'dynamic' OR max_rl_type = 'value'
      \t - max_rl = 2 if max_rl_type = 'dynamic' OR max_rl = 1.18 if 'value'

      \t - don't forget the (Co)variances!
      ")
}
