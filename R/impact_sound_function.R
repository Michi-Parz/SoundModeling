# Norm: 12354-2


#' Calculate the impact sound on a raw ceiling acording to DIN EN ISO 12354-2
#'
#' @param mass mass per unit area
#' @param volume room volumne
#' @param reference_freq reference frequency
#' @param offset offset (= 155 dB in standard)
#' @param cL Longitudinal velocity
#' @param c0 Sound speed in air in m/s. Default = 340, which is the speed at 15 °C
#' @param freq Frequency in Hz
#' @param max_rl See ?smooth_radiation_level
#' @param max_rl_type See ?smooth_radiation_level
#'
#' @description
#' Equation (B.2) from DIN EN ISO 12354-2
#'

# (B.2)
impact_sound_raw <- function(thickness, density, volume, refrence_freq = 1000,
                             offset = 155, cL = 3500, c0 = 340,
                             freq = NULL,
                             max_rl = 1.18, max_rl_type = "value"){
  if (is.null(freq))
    freq <- c(50, 63, 80,100,125,160,200,250,315,400,500,630,800, 1000, 1250,
              1600, 2000, 2500, 3150, 4000, 5000)

  f_c <- (sqrt(3)*c0^2)/(pi * cL*thickness)

  sigma <- smooth_radiation_level(
    w_perimeter = volume_to_side_length(volume = volume, output = "perimeter"),
    w_surface = volume/2.5,
    f_c = f_c,
    freq = freq,
    max_rl = max_rl,
    max_rl_type = max_rl_type
  )$Original

  eta_tot <- 0.01 + 0.5/sqrt(freq)

  reverberation_t <- 2.2/(freq*eta_tot)

  mass <- thickness * density

  offset - 30*log10(mass) + 10*log10(reverberation_t) +
    10*log10(sigma) + 10*log10(freq/refrence_freq)
}



#' Calculate the impact sound reduction according to DIN EN ISO 12354-2
#'
#' @param prefactor 30 in (C.1) and 40 in (C.3)
#' @param reference_freq reference frequency
#' @param freq Frequency in Hz
#'
#' @description
#' Equation (C.1) and (C.2) from DIN EN ISO 12354-2


# (C.1) bzw. (C.3)
impact_sound_reduction <- function(prefactor = 30, resonance_freq, freq = NULL){
  if (is.null(freq))
    freq <- c(50, 63, 80,100,125,160,200,250,315,400,500,630,800, 1000, 1250,
              1600, 2000, 2500, 3150, 4000, 5000)
  prefactor*log10(freq/resonance_freq) * ifelse(freq >= resonance_freq, 1, 0)
}


#' Calculate the impact sound according to DIN EN ISO 12354-2
#'
#' @param thickness wall thickness in m
#' @param reduction_factor strength of reduction: 30 in (C.1) and 40 in (C.3)
#' @param resonance_freq resonance frequency of the system in Hz
#' @param volume volume of the receiving room, in cubic metres.
#' @param density density of the floor, in kilograms per cubic metre;
#' @param offset additive dB shift. Equal to 155 dB in equation (B.2)
#' @param freq Frequency in Hz
#' @param ... Further arguments for "impact_sound_raw"
#'
#' @description
#' Difference between equation (B.2) and (C.1) from DIN EN ISO 12354-2
# (B.2) - (C.1)

impact_sound_standard <- function(thickness, reduction_factor, resonance_freq,
                             volume, density = 2300, offset = 155, freq = NULL,
                             ...){
  if (is.null(freq))
    freq <- c(50, 63, 80,100,125,160,200,250,315,400,500,630,800, 1000, 1250,
              1600, 2000, 2500, 3150, 4000, 5000)

  #mass <- thickness * density
  Ln <- impact_sound_raw(thickness, density, volume,
                         offset = offset, freq = freq, ...)

  delta_L <- impact_sound_reduction(reduction_factor, resonance_freq, freq = freq)

  Ln - delta_L
}



# Frequenzweise (für gemischte Modelle notwendig)


# Es wichtig ist, dass jede Komponente auch Vektorweise übergeben werden kann..



# Fall stetigkeit notwendig
# glaettungs_fkt <- function(f, f0, delta, mq = 4/5){
#   log_dq <- log((1-delta)/delta)
#
#   alpha <- log_dq*(mq+1)/(mq-1)
#   beta <- 2*log_dq/(f0* (1-mq))
#
#   input <- alpha + beta * f
#
#   1/(1+exp(-input))
#
# }
#
#
# norm_glaettung <- function(f, f0, delta, mq = 4/5){
#   center <- (f0 + mq*f0)/2
#   speed <- (f0 - center)/qnorm(1-delta)
#
#   pnorm(f, mean = center, sd = speed)
#
# }



#
# norm_ts_roh_f <- function(f, t, q, f0, volumen,
#                           offset = 155, dichte = 2300,
#                           glaettung = 0, mq = 4/5,
#                           glaettfkt = c("logistisch", "norm")){
#
#
#   glaettfkt <- match.arg(glaettfkt)
#
#   sigma <- radiation_level_din(f, lower_rl_bound = 0.89, upper_rl_bound = 1.4,
#                                max_value = Inf,
#                                f_c = (4*sqrt(3)*340^2)/(pi * 3500),
#                                u = volume_to_side_length(volume = volumen,
#                                                          output = "perimeter"),
#                                s = volumen / 2.5)
#
#   eta_tot <- 0.01 + 0.5/sqrt(f)
#
#   nachhallzeit <- 2.2/(f*eta_tot)
#
#   ln_rohdecke <- (offset - 30*log10(t * dichte) +
#                     10 * log10(sigma * nachhallzeit*f/1000))
#
#   if (glaettung == 0) {
#     return(ln_rohdecke - ifelse(f0 <= f, q * log10(f/f0), 0) )
#   }
#
#
#   if (glaettfkt == "logistisch") {
#     gfkt <- glaettungs_fkt(f, f0, delta = glaettung, mq)
#   }
#   if (glaettfkt == "norm") {
#     gfkt <- norm_glaettung(f, f0, delta = glaettung, mq)
#   }
#
#   ln_rohdecke - q * log10(f/f0)* gfkt
#
# }

