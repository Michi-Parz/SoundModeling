#' Generate the radiation level for the typical terz-values and smooth it via moving average and moving geometric-average
#'
#' @param lower_rl_bound f_c * lower_rl_bound defines the first radiation level region
#' @param upper_rl_bound f_c * upper_rl_bound defines the last radiation level region
#' @param delta_bound Defines the region for which delta2 is not zero.
#' @param max_rl Maximum value reached by the radiation level. (Ignored for types: "fc" and "sigma3")
#' @param max_rl_type Maximum radiation level type.
#' @param c0 Speed of sound
#' @param w_perimeter Perimeter of the wall
#' @param w_surface Surface of a wall
#' @param f_c Coincidence cutoff frequency
#' @param freq Frequencies - if NULL the typical 50, 63, 100, ..., 4000, 5000 values are used.
#'
#' @details
#' For max_rl_type = 'dynamic' the maximal sigma is equal to \eqn{\min(\sigma_3, \mathrm{max\_rl}, \sqrt{1+u f_c/(5c_0)})}
#'
#' For max_rl_type = 'value' the maximal sigma is equal to \eqn{\mathrm{max\_rl}}
#'
#' For max_rl_type = 'fc' the maximal sigma is equal to \eqn{f_c}
#'
#' For max_rl_type = 'sigma3' the maximal sigma is equal to \eqn{\sigma_3}
#'
#'
#'
#' @return smooth_radiation_level an data.frame
smooth_radiation_level <- function(lower_rl_bound = 1,
                                   upper_rl_bound = 1,
                                   delta_bound = 0.5,
                                   max_rl = 2,
                                   max_rl_type = c("dynamic", "value", "fc", "sigma3"),
                                   c0 = 340,
                                   w_perimeter = 12.57,
                                   w_surface = 9.8,
                                   f_c = (4*sqrt(3)*340^2)/(pi * 1250),
                                   freq = NULL){

  max_rl_type <- match.arg(max_rl_type)


  # Typical terz-values
  if (is.null(freq)) {
    freq <- c(50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
              1250,1600,2000,2500,3150,4000,5000)
  }



  n <- length(freq)
  # calculate radiation_level

  rl <- radiation_level_din(freq, lower_rl_bound, upper_rl_bound, delta_bound,
                            max_value = Inf, c0,
                            u = w_perimeter,
                            s = w_surface, f_c)

  # rindel_rad <- radiation_level_rindel(c0 = c0, s = w_surface,
  #                                      u = w_perimeter, f_c = f_c)

  # Siehe unten!
  # approx_rindel_rad <- radiation_level_app_rindel(c0 = c0, s = w_surface,
  #                                      u = w_perimeter, f_c = f_c)


  if (max_rl_type == "value") {
    max_value <- max_rl
  }

  if (max_rl_type == "sigma3") {
    max_value <- sqrt((pi*freq * w_perimeter)/(16*c0))
  }
  if (max_rl_type == "fc") {
    max_value <- sqrt(1+ w_perimeter * f_c/(5*c0))
  }
  if (max_rl_type == "dynamic") {
    max_value <- pmin(
      sqrt((pi*freq * w_perimeter)/(16*c0)),
      sqrt(1+ w_perimeter * f_c/(5*c0)),
      max_rl
    )
  }
  rl <- pmin(max_value, rl)
  #rindel_rad <- pmin(max_value, rindel_rad)

  # Siehe unten!
  #approx_rindel_rad <- pmin(max_value, approx_rindel_rad)

  # calculate forced radiation level
  side_len <- side_lengths_calc(w_perimeter,w_surface)

  forced_rl <- radiation_level_forced(freq, c0 = c0, l1 = side_len$l1,
                                      l2 = side_len$l2)



  # Moving average
  glmit <- stats::filter(rl, 1/3*c(1,1,1))

  # Moving geometric-average
  glgmit <- exp(stats::filter(log(rl), 1/3*c(1,1,1)))

  # Remove NAs
  glmit[1] <- glgmit[1] <- rl[1]
  glmit[n] <- glgmit[n] <- rl[n]

  data.frame(
    "Frequency" = freq,
    "Forced" = forced_rl,
    "Original" = rl,
    "Average" = glmit,
    "Geometric" = glgmit#,
    #"Rindel" = rindel_rad,
    #"A_Rindel" = approx_rindel_rad  Siehe unten!
  )

}


# Der Abstrahlgrad approximativ nach Rindel ist vorübergehend auskommentiert da
# es in RMarkdown ansonsten zu folgender Fehlermeldung kommt: Registered S3 method overwritten by 'quantmod'
# vermutlich aufgrund von "imputeTS::na_interpolation"

# Seit 24.04.24 sind beide Rindel Versionen auskommentiert,
# da seitdem die Funktion für unterschiedliche Frequenzen laufen soll.
