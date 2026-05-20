# Korrelation of the R-Value when frequency is known
r_korr_given_freq <- function(data, method = c("cor", "cov", "lwls")){
  method <- match.arg(method)
  freq <- unique(data$fpHz)
  r_given_freq <- lapply(
    freq,
    FUN = function(f, data){
      data$RpdB[data$fpHz == f]
    },
    data = data
  )
  
  r_given_freq <- Reduce(cbind, r_given_freq)
  r_given_freq <- as.data.frame(r_given_freq)
  names(r_given_freq) <- paste("f =",freq)
  if (method == "cor") {
    return(cor(r_given_freq))
  }
  if (method == "lwls") {
    return(cvCovEst::linearShrinkLWEst(r_given_freq))
  }
  cov(r_given_freq)
}
