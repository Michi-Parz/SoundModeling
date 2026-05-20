#' Soundproofing measure data with calcium silicate construction as a list for STAN
#'
#' @description
#' Note: These are NOT actual measurements but artificially generated sample data.
#' For actual measurements, use the \emph{SoundModelingData} package if available!
#'
#'@format
#'  \describe{
#'    \item{N}{Number of observations = 27}
#'    \item{y}{27 times 21 matrix of the measured R values in dB}
#'    \item{l1}{Vector of wall lengths (long side)}
#'    \item{l2}{Vector of wall lengths (short side)}
#'    \item{t}{Wall thickness in m}
#'    \item{mass}{mass per unit area}
#'    \item{mu}{poisson number}
#'    \item{V}{Total volume}
#'    \item{S_tot}{Total surface}
#'    \item{cL1}{Material cL}
#'    \item{max_rl}{maximal radiation level (hyperparameter)}
#'  }
#' data(sl_list_stan)
"sl_list_stan"
