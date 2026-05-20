

# Linear reinforced concrete adjusting
rconcrete_lin_adj <- function(freq, spm_values, f11, slope = 3){
  f_lower_f11 <- freq < f11
  if (all(!f_lower_f11))
    return(spm_values)

  which_flf11 <- which(f_lower_f11)

  lin_freq_nr <- freq_nr(freq)[f_lower_f11]

  numb_freq <- length(lin_freq_nr)
  abscissa <- rep(NA, numb_freq)
  intercept <- rep(NA, numb_freq)

  freq_jump <- c(diff(which_flf11)>1, FALSE)
  new_inter_pos <- which_flf11[freq_jump]+1

  freq_nr_change <- diff(lin_freq_nr)
  new_start <- c(1,which(freq_nr_change<=0)+1, numb_freq+1)
  ns_len <- length(new_start)

  for (i in seq_len(ns_len-1)) {
    start <- new_start[i]
    end <- new_start[i+1]-1
    stoend <- start:end #start to end
    abscissa[stoend] <- rev(lin_freq_nr[stoend])

    inter_pos <- seq_len(length(stoend))
    na_inter <- is.na(intercept)
    intercept[na_inter][inter_pos] <- spm_values[new_inter_pos[i]]
  }
  na_inter <- is.na(intercept)

  intercept[na_inter] <- spm_values[max(which_flf11)+1]

  spm_values[f_lower_f11] <-  intercept + slope*abscissa

  spm_values
}



## Old Version (simple but not flexible at all...)
# rconcrete_lin_adj <- function(spm_values, f11, slope = 3){
#   f_lower_f11 <- freq < f11
#
#   s_flf11 <- sum(f_lower_f11)
#   if (s_flf11 > 0) {
#     spm_values[f_lower_f11] <- spm_values[s_flf11 +1] + slope*(s_flf11:1)
#   }
#   spm_values
# }



