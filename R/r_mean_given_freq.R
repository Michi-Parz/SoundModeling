r_mean_given_freq <- function(data){
  freq <- unique(data$fpHz)
  r_given_freq <- lapply(
    freq,
    FUN = function(f, data){
      data$RpdB[data$fpHz == f]
    },
    data = data
  )
  
  mean_values <- lapply(r_given_freq, mean)
  unlist(mean_values)
}

r_median_given_freq <- function(data){
  freq <- unique(data$fpHz)
  r_given_freq <- lapply(
    freq,
    FUN = function(f, data){
      data$RpdB[data$fpHz == f]
    },
    data = data
  )
  
  mean_values <- lapply(r_given_freq, median)
  unlist(mean_values)
}
