# Soundproofing measure acording to DIN EN ISO 12354-1 Anhang B.3 Seite 33
# ACHTUNG für f<f_c ist ein Fehler vorhanden!

# Statt ((1-f^2)/fc^2)^-2 ist es (1-(f/fc)^2)^{-2}
# In der älteren Version scheint der Rest außerdem zu passen.... also der Rest bei dem tau


# Help functions
transmittance_left <- function(total_loss, frequency, rad_level, forced_rl,
                               c0 = 340, rho0 = 400/c0, mass = 440,
                               f_c = (4*sqrt(3)*340^2)/(pi * 1250)){
  ((rho0*c0)/(pi*frequency * mass))^2 *(
    2 * forced_rl  *
    (1 - (frequency^2)/(f_c^2))^(-2) +
    (pi * f_c * rad_level^2) /
    (2*frequency *total_loss)
    )
}


transmittance_between <- function(total_loss, frequency, rad_level, c0 = 340,
                                  rho0 = 400/c0, mass = 440,
                                  f_c = (4*sqrt(3)*340^2)/(pi * 1250)){
  ((rho0*c0)/(pi*frequency * mass))^2 *
    (pi * rad_level^2) /
    (2*total_loss)
}


transmittance_right <- function(total_loss, frequency, rad_level, c0 = 340,
                                rho0 = 400/c0, mass = 440, poisson,
                                f_c, cL, t, gamma, sts = FALSE){
  if (is.null(gamma)) {
    f_c_eff <- f_c * (
      4.05 * t * frequency / cL + sqrt(1+ (4.05 * t * frequency/cL)^2)
    )
  }
  if (!is.null(gamma))  {
    inv_gammap3 <- (1/gamma)^3
    if (!sts) {
      f_c_eff <- f_c * (
        1 + inv_gammap3*((3.6 * t*frequency) /((1-poisson)*cL))^(3/2)
      )^(2/3)
    }
    if (sts) {
      f_c_eff <- f_c * (
        1 + inv_gammap3*((4.8 * t*frequency) /(cL))^(3/2)
      )^(2/3)
    }

  }

  ((rho0*c0)/(pi*frequency * mass))^2 *
    (pi * f_c_eff * rad_level^2) /
    (2*frequency *total_loss)
}

adj_tau0 <- function(freq, cl, z0, rho, t, eta_tot, eta_ref){
  zm <- cl*rho
  kmt <- 2*pi*freq*t/cl
  inv_tau <- (cos(kmt)^2 + 0.25* (z0/zm +
                         zm/z0)^2 * sin(kmt)^2)*eta_tot / eta_ref
  1/inv_tau
}

# complx_tau0 <- function(freq, clex, z0, rho, t, etaint) {
#   kmt <- 2*pi*freq*t/clex
#   kmtc <- kmt/sqrt(1+1i*etaint)
#   inv_tau0 <- cos(kmtc)^2 +
#     0.25 * (rho*clex/z0 + z0/(rho*clex))^2 * sin(kmtc)^2
#   1/inv_tau0
# }
# real_tau0 <- function(freq, clex, z0, rho, t, etaint) {
#   kmt <- 2*pi*freq*t/clex
#   kmtc <- kmt/sqrt(1+etaint)
#   inv_tau0 <- cos(kmtc)^2 +
#     0.25 * (rho*clex/z0 + z0/(rho*clex))^2 * sin(kmtc)^2+etaint^2
#   1/inv_tau0
# }

tau0_func <- function(freq, clex, z0, rho, t, etaint, v = 1i,h=0) {
  kmt <- 2*pi*freq*t/clex
  kmtc <- kmt/sqrt(1+v*etaint)
  z1 <- rho*clex
  inv_tau0 <- cos(kmtc)^2 + 0.25 * (z1/z0 + z0/z1)^2 * sin(kmtc)^2 + h
  1/inv_tau0
}


transmittance <- function(internal_loss, lab_loss_factor, cL,
                          lower_t_bound, upper_t_bound,
                          type,
                          c0 = 340, rho0 = 400/c0, mass = 440,
                          w_perimeter = 12.57,
                          w_surface = 9.8,
                          plateau_style = c("max", "sum", "mixed", "smooth",
                                            "tau0add", "tau0Lim",
                                            "tau0Complex", "tau0real",
                                            "test","approx", "none"),
                          wall_thickness = 0.25,
                          rho = 1760,
                          poisson = 0.25,
                          fix_between_rad,
                          fce_gamma, cL1 = 2500,
                          cLplateau = c("1", "eff"),
                          eta_ref = 0.02,
                          freq,
                          gm_weight = 4,
                          sts = FALSE,
                          max_rl = max_rl,
                          max_rl_type = max_rl_type,
                          ...){
  type <- match.arg(type,
                    c("Original","Average", "Geometric", "Rindel", "A_Rindel"))
  plateau_style <- match.arg(plateau_style,
                             c("max", "sum", "mixed", "smooth",
                               "tau0add", "tau0Lim", "tau0Complex", "tau0real",
                               "test","approx", "none"))


  cLplateau <- match.arg(cLplateau, c("1", "eff"))

  n_freq <- length(freq)

  # Wall thickness
  t <- wall_thickness
  # Coincidence cutoff frequency
  f_c <- (sqrt(3)*c0^2)/(pi * cL *t)
  if (sts) {
    f_c <- c0^2 / (1.8*cL * t)
  }

  # Extend
  if (length(f_c) == 1) {
    f_c <- rep(f_c, n_freq)
  }
  if (length(mass) == 1) {
    mass <- rep(mass, n_freq)
  }
  if (length(t) == 1) {
    t <- rep(t, n_freq)
  }
  if (length(cL) == 1) {
    cL <- rep(cL, n_freq)
  }

  # Radiation levels
  rad_df <- smooth_radiation_level(f_c = f_c, c0 = c0,
                                   w_perimeter = w_perimeter,
                                   w_surface = w_surface,
                                   freq = freq,
                                   max_rl = max_rl,
                                   max_rl_type = max_rl_type,
                                   ...)
  rad_df$nr <- 1:n_freq
  tr_all <- rep(NA, n_freq)
  rad_df$total_loss <- internal_loss + lab_loss_factor / sqrt(rad_df$Frequency)

  # Split radiation  levels and frequency in corresponding regions
  third_region <- lower_t_bound != upper_t_bound

  cond_low <- rad_df$Frequency < f_c * lower_t_bound
  cond_upp <- rad_df$Frequency >= f_c * upper_t_bound

  lower_rad_df <- rad_df[cond_low,]
  upper_rad_df <- rad_df[cond_upp,]
  if (third_region) {
    cond1_bet <- (rad_df$Frequency >= f_c * lower_t_bound)
    cond2_bet <- (rad_df$Frequency < f_c * upper_t_bound)
    cond_between <- cond1_bet & cond2_bet
    beween_rad_df <- rad_df[cond_between ,]
  }

  c_T <- cL1 * sqrt((1-poisson)/2)
  f_T <- f_c  *cL/cL1 * (c_T / c0)^2
  cLextended <- cL1 * (1-poisson)/sqrt(1-2*poisson)

  if (sts) {
    cLextended <- 1.1* cL
  }


  if (is.null(eta_ref)) {
    eta_ref <- internal_loss +  lab_loss_factor / sqrt(2^(2/3)*f_T)
  }


  tau_plateau_eff <- ((4*rho0 * c0) / (cLextended * rho))^2 * (eta_ref / rad_df$total_loss)
  tau_plateau_1 <- ((4*rho0 * c0) / (cLextended * rho))^2 * (eta_ref / rad_df$total_loss)

  if (cLplateau == "eff") {
    tau_plateau <- tau_plateau_eff
  }
  if (cLplateau == "1") {
    tau_plateau <- tau_plateau_1
  }


  # Calculate the transmittance for both regions
  tr_left <- transmittance_left(total_loss = lower_rad_df$total_loss,
                                frequency = lower_rad_df$Frequency,
                                rad_level = lower_rad_df[,type],
                                forced_rl = lower_rad_df$Forced,
                                c0 = c0, rho0 = rho0,
                                mass = mass[cond_low], f_c = f_c[cond_low])

  tr_right <- transmittance_right(total_loss = upper_rad_df$total_loss,
                                 frequency = upper_rad_df$Frequency,
                                 rad_level = upper_rad_df[,type],
                                 c0 = c0, rho0 = rho0,
                                 poisson = poisson,
                                 mass = mass[cond_upp], f_c = f_c[cond_upp],
                                 cL = cL[cond_upp], t = t[cond_upp],
                                 gamma = fce_gamma,
                                 sts = sts)

  tr_all[lower_rad_df$nr] <- tr_left
  tr_all[upper_rad_df$nr] <- tr_right

  if (third_region) {
    if (!is.null(fix_between_rad)) {
      beween_rad_df[,type] <- fix_between_rad
    }
    tr_between <- transmittance_between(total_loss = beween_rad_df$total_loss,
                                      frequency = beween_rad_df$Frequency,
                                      rad_level = beween_rad_df[,type],
                                      c0 = c0, rho0 = rho0,
                                      mass = mass[cond_between],
                                      f_c = f_c[cond_between])
    tr_all[beween_rad_df$nr] <- tr_between
  }

  if (plateau_style == "sum") {
    return(tr_all + tau_plateau)
  }
  if (plateau_style == "mixed") {
    return(
      sqrt(pmax(tr_all, tau_plateau) * (tr_all + tau_plateau))
    )
  }
  if (plateau_style == "smooth") {
    tr_all[18] <- sqrt(tr_all[17]*tau_plateau[19])
    tr_all[19:21] <- tau_plateau[19:21]
    return(tr_all)
  }
  if (plateau_style == "tau0add") {
    adj_tau_region <- 2^(2/3)*f_T < freq # 2^(2/3) wegen zwei Terzbänder!

    etatot <- internal_loss + lab_loss_factor / sqrt(freq)



    tau0 <- adj_tau0(freq,
                                 cl = cLextended,
                                 z0 = rho0 * c0,
                                 rho = rho,
                                 t = t,
                                 eta_tot = etatot,
                                 eta_ref = eta_ref)

    harmonic_tau <- 2/(1/tau0[adj_tau_region]+
                         1/tau_plateau[adj_tau_region])

    shift <- tr_all[adj_tau_region][1] -  harmonic_tau[1]

    tr_all[adj_tau_region] <- harmonic_tau + shift

    return(tr_all)
  }
  if (plateau_style == "tau0Lim") {

    change_freq <- 2^(2/3)*f_T# 2^(2/3) wegen zwei Terzbänder!
    abs_dist_to_cf <- abs(log(change_freq) - log(freq))
    change_freq <- freq[abs_dist_to_cf == min(abs_dist_to_cf)]

    adj_tau_region <- change_freq < freq

    etatot <- internal_loss + lab_loss_factor / sqrt(freq)

    tau0 <- adj_tau0(freq,
                                 cl = cLextended,
                                 z0 = rho0 * c0,
                                 rho = rho,
                                 t = t,
                                 eta_tot = etatot,
                                 eta_ref = eta_ref)
    n_obs <- length(freq)/21
    tau0_mat <- matrix(tau0, ncol = n_obs)
    tau0_mr <- pmin(tau0_mat[-1,]/tau0_mat[-21,],4)
    tau0_cp <- apply(tau0_mr, 2, FUN = cumprod)
    tau0_cp1 <- matrix(1,nrow = 21, ncol = n_obs)
    tau0_cp1[-1,] <- tau0_cp
    tau0 <- c(tau0_mat[1,] * tau0_cp1)

    harmonic_tau <- 2/(1/tau0+1/tau_plateau)

    shift <- tr_all[adj_tau_region][1] /  tau0[adj_tau_region][1]

    tr_all[adj_tau_region] <- pmax(tau0*shift,
                                   tau_plateau)[adj_tau_region]

    return(tr_all)
  }
  if (plateau_style == "tau0Complex") {

    change_freq <- 2^(2/3)*f_T# 2^(2/3) wegen zwei Terzbänder!
    abs_dist_to_cf <- abs(log(change_freq / freq))
    change_freq <- freq[abs_dist_to_cf == min(abs_dist_to_cf)]

    adj_tau_region <- change_freq < freq

    etatot <- internal_loss + lab_loss_factor / sqrt(freq)

    tau0 <- tau0_func(freq,
                        clex = cLextended,
                        z0 = rho0 * c0,
                        rho = rho,
                        t = t,
                        etaint = internal_loss)

    #geom_tau <- sqrt(tau_plateau * Re(tau0))

    geom_tau <- (tau_plateau^gm_weight * Re(tau0))^(1/(1+gm_weight))

    #tr_all[adj_tau_region] <- pmax(geom_tau, tau_plateau)[adj_tau_region]

    shifty <- tr_all[freq == change_freq] / geom_tau[freq == change_freq]

    tr_all[adj_tau_region] <- geom_tau[adj_tau_region]*shifty

    return(tr_all)
  }

  if (plateau_style == "tau0real") {

    change_freq <- 2^(2/3)*f_T# 2^(2/3) wegen zwei Terzbänder!
    abs_dist_to_cf <- abs(log(change_freq / freq))
    change_freq <- freq[abs_dist_to_cf == min(abs_dist_to_cf)]

    adj_tau_region <- change_freq < freq

    etatot <- internal_loss + lab_loss_factor / sqrt(freq)

    tau0 <- tau0_func(freq,
                        clex = cLextended,
                        z0 = rho0 * c0,
                        rho = rho,
                        t = t,
                        etaint = internal_loss,
                        v = 0, h = internal_loss^2)

    #geom_tau <- sqrt(tau_plateau * Re(tau0))

    geom_tau <- (tau_plateau^gm_weight * Re(tau0))^(1/(1+gm_weight))

    #tr_all[adj_tau_region] <- pmax(geom_tau, tau_plateau)[adj_tau_region]

    shifty <- tr_all[freq == change_freq] / geom_tau[freq == change_freq]

    tr_all[adj_tau_region] <- geom_tau[adj_tau_region]*shifty

    return(tr_all)
  }

  if (plateau_style == "approx") {

    adj_tau_region <- 2^(2/3)*f_T < freq # 2^(2/3) wegen zwei Terzbänder!

    etatot <- internal_loss + lab_loss_factor / sqrt(freq)

    tau0approx <- (rho0 * c0)^2*eta_ref / ((pi*freq * mass)^2*etatot)

    #harmonic_tau <- 2/(1/tau0approx + 1/tau_plateau)

    tr_all[adj_tau_region] <- tau0approx[adj_tau_region]

    return(tr_all)
  }
  if (plateau_style == "none") {
    return(tr_all)
  }


  pmax(tr_all, tau_plateau)

}

# Flankenübertragung

flank_transmission <- function(impact = 1, power = 1, start){
  expo <- -0.1 * log10(impact * (start/freq)^power)

  -10 * log10(1 + 10^expo)
}



###############
# Main Function
###############
# internal_loss: Internal loss, in [0.01, 0.02]
# lab_loss_factor: Lab loss factor, in [0.2, 1]
# cL: Longitudinal wave velocity, in [1000,3000]
# lower_t_bound: lower_t_bound * f_c < frequency defines the defines the first transmission region
# upper_t_bound: upper_t_bound * f_c < frequency defines the defines the last transmission region
# type: Choose whether the radiation level should be smoothed with the moving average, the geometric moving average or not at all.
# c0: Speed of sound
# rho0: Air density
# mass: The mass per unit area, in kilograms per square meter
# ...: Further arguments, which are needed for the radiation level


#' Calculate the soundproofing measure R
#'
#' @param internal_loss Internal loss factor
#' @param lab_loss_factor Constant "C" respresenting the boundary loss C/sqrt(f)
#' @param cL Effective longitudinal velocity
#' @param lower_t_bound If f < lower_t_bound*fc, the first section applies for the transmittance
#' @param upper_t_bound If f > upper_t_bound*fc, the last section applies for the transmittance
#' @param type Choose whether the radiation level should be smoothed with moving average, geometric moving average or not at all.
#' @param c0 Sound speed in air in m/s. Default = 340, which is the speed at 15 °C
#' @param rho0 Density of air in kg/m³. Default = 1.23, which is the density at 15 °C
#' @param mass Mass per unit area in kg/m². Default = 440.
#' @param w_perimeter Wall perimeter.
#' @param w_surface Wall surface.
#' @param smoothing_power Smoothing strength. If zero, no smoothing is applied and if one, the two values are set identically by one uncertainty point.
#' @param smoothing_direction If "left" only the values around lower_t_bound fc are getting smothed and if "both" the values around upper_t_bound fc will also be smoothed.
#' @param smoothing_log If FALSE the distance between two frequencies is calculated via absolute distance and if TRUE the logarithm of the frequencies is used instead.
#' @param plateau_style What should the curve look like for extremely high frequencies? *Add description here as soon as I get to grips with this description and know how to make it clearer, etc.*
#' @param wall_thickness Constuction thickness of the wall in m.
#' @param rho Density of the material in kg/m³. Default = 1760.
#' @param poisson Poisson number / material value. Between 0 and 0.5.
#' @param cL1 (Quasi) longitudinal velocity / material longitudinal velocity
#' @param cLplateau which longitudinal velocity should be used for tau-plateau? cL1 or cLeff?
#' @param volume_rec_room Reception room volume in m³.
#' @param surface_rec_room Total surface in reception room in m².
#' @param fix_between_rad Should the values between lower_t_bound fc and upper_t_bound fc be set to an fixed value? If no set fix_between_rad to NULL.
#' @param fce_gamma gamma for fceff. If gamma == NULL the DIN EN ISO 12354-1 calculation is used. Otherwiese gamma should be between 0.5 and 1.
#' @param break_point Insert a straight line before a break point. If FALSE no break point is use. For numeric the break point is set to this frequency and for NULL the breakpoint is at f11.
#' @param pre_bp_slope Slope of the line before break point. (Pre break point slope.)
#' @param add_flank_transmission If TRUE add an extension for flank transmission.
#' @param flank_impact One parameter for the flank extension.
#' @param flank_power Another parameter for the flank extension.
#' @param flank_start At which frequency should the flank transmission apply?
#' @param freq Vector of frequencies.
#' @param sts If TRUE the model is calculated strict according to standard ISO 12354-1. See strict_to_standard() for more information.
#' @param max_rl Maximal radiation level value
#' @param max_rl_type Maximum radiation level type. See ?smooth_radiation_level.
#' @param offset R+offset
#' @param ... Further arguments mainly from the function for the radiation level.
#'
#' @return Soundproofing measure R
#' @export soundproofing_measure()
#'
#' @examples soundproofing_measure()
soundproofing_measure <- function(internal_loss = 0.015, lab_loss_factor = 0.9, cL = 1250,
                                  lower_t_bound = 0.89, upper_t_bound = 1.4,
                                  type = c("Original","Average",
                                           "Geometric", "Rindel", "A_Rindel"),
                                  c0 = 340, rho0 = 1.23, mass = 440,
                                  w_perimeter = 12.57,
                                  w_surface = 9.8,
                                  smoothing_power = 2/3,
                                  smoothing_direction = c("left","both"),
                                  smoothing_log = TRUE,
                                  plateau_style =  c("max", "sum",
                                                     "mixed", "smooth",
                                                     "tau0add", "tau0Lim",
                                                     "tau0Complex","tau0real",
                                                     "test","approx", "none"),
                                  wall_thickness = 0.25,
                                  rho = 1760,
                                  poisson = 0.25,
                                  cL1 = cL,
                                  cLplateau = c("1", "eff"),
                                  volume_rec_room = 50,
                                  surface_rec_room = 85,
                                  fix_between_rad = NULL,
                                  fce_gamma = NULL,
                                  break_point = F,
                                  pre_bp_slope = 3,
                                  add_flank_transmission = F,
                                  flank_impact = 1,
                                  flank_power = 1,
                                  flank_start = NULL,
                                  freq = NULL,
                                  gm_weight = 4,
                                  sts = FALSE,
                                  max_rl = 2,
                                  max_rl_type = c("dynamic", "value", "fc", "sigma3"),
                                  offset = 0,
                                  ...){
  if (upper_t_bound < lower_t_bound) {
    stop("The lower bound must be smaller than the upper bound!")
  }
  smoothing_direction <- match.arg(smoothing_direction)
  max_rl_type <- match.arg(max_rl_type)

  # Wall thickness
  t <- wall_thickness
  # Coincidence cutoff frequency
  f_c <- (sqrt(3)*c0^2)/(pi * cL *t)
  if (sts) {
    f_c <- c0^2 / (1.8*cL * t)
  }
  # f11 if reinforced concrete adjusting is TRUE
  if (isTRUE(break_point)) {
    break_point <- f11_value(f_c, c0, u = w_perimeter, s = w_surface)
  }

  if (is.null(freq)) {
    # Terzvalues
    freq <- c(50,63,80,100,125,160,200,250,315,400,500,630,
              800,1000,1250,1600,2000,2500,3150,4000,5000)
  }

  sp_mea <- -10 * log10(
    transmittance(internal_loss = internal_loss,
                  lab_loss_factor = lab_loss_factor,
                  cL = cL,
                  cL1 = cL1,
                  lower_t_bound = lower_t_bound,
                  upper_t_bound = upper_t_bound,
                  type = type,
                  c0 = c0,
                  rho0 = rho0,
                  mass = mass,
                  w_perimeter = w_perimeter,
                  w_surface = w_surface,
                  plateau_style = plateau_style,
                  wall_thickness = t,
                  rho = rho,
                  poisson = poisson,
                  fix_between_rad = fix_between_rad,
                  fce_gamma = fce_gamma,
                  cLplateau = cLplateau,
                  freq = freq,
                  gm_weight = gm_weight,
                  sts = sts,
                  max_rl = max_rl,
                  max_rl_type = max_rl_type,
                  ...)
   )-10 * log10(
     1 + c0 * surface_rec_room /(8* volume_rec_room*freq)
   )

  sp_mea <- uncertainty_smoothing(y = sp_mea, x = freq,
                                  ucb = lower_t_bound * f_c,
                                  power = smoothing_power,
                                  log = smoothing_log)

  if (smoothing_direction == "both") {
    sp_mea <- uncertainty_smoothing(y = sp_mea, x = freq, ucb = upper_t_bound * f_c,
                                    power = smoothing_power,
                                    log = smoothing_log)
  }

  if (is.numeric(break_point))
    sp_mea <- rconcrete_lin_adj(freq, sp_mea, break_point, slope = pre_bp_slope)

  if (add_flank_transmission) {
    if (is.null(flank_start))
      flank_start <- f_c


    sp_mea <- sp_mea + flank_transmission(
      impact = flank_impact,
      power = flank_power,
      start = flank_start
    )
  }

  sp_mea + offset
}

# 72.25 = c0*S_{tot}/(8*V)
# S_{tot} = 85m² (Total surface area of the receiving room)
# V = 50 m³ (Volume reception room)
