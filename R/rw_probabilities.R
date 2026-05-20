
#' Calculates the Rw value
#'
#' @title Transforms a rw matrix into a dataframe which indicates the relative frequencies for minimum values.
#' @param rw \eqn{S \times T} matrix of Rw values. With \eqn{S} samples and number of wall thicknesses \eqn{T}.
#' @param bounds boundaries for Rw
#' @param long if TRUE the result data.frame is in long format.

rw_probabilities <- function(rw, bounds = c(47,50,53,56,59), long = FALSE){
  probs <- sapply(bounds, \(x){
    colMeans(rw>=x)
  })
  probs <- data.frame(probs)

  thickness <- rownames(probs)
  thickness <- stringr::str_remove(thickness, "t")
  thickness <- as.numeric(thickness)

  names(probs) <- paste("Rw", bounds, sep = "")

  probs <- cbind("t"=thickness,probs)
  rownames(probs) <- 1:nrow(probs)

  if (long){
    bound_cols <- 1+1:length(bounds)
    probs <- tidyr::pivot_longer(probs, bound_cols,
                                 names_to = "Rw", values_to = "Prob")
    probs$Rw <- as.numeric(stringr::str_remove(probs$Rw, "Rw"))
  }

  probs
}
