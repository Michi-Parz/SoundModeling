#' A description of fit_info_tab
#'
#' A details of fit_info_tab
#'
#' @title fit_info_tab: Title
#' @param x A model or a list
#' @param ... other arguments like digits
#' @rdname fit_info_tab
#' @export fit_info_tab
fit_info_tab <- function (mod, ...) {
  UseMethod("fit_info_tab")
}

#' @return \code{NULL}
#'
#' @rdname fit_info_tab
#' @method fit_info_tab nls
#' @exportS3Method fit_info_tab nls
fit_info_tab.nls <- function(mod,
                         digits = NULL, freq_subset = FALSE){

  # Estimation
  coeff <- coefficients(summary(mod))
  p_star <- gtools::stars.pval(coeff[,4])
  esti <- coeff[,1]
  numb_coef <- length(esti)
  if (!is.null(digits)) {
    if (length(digits == 1)) {
      digits <- rep(digits, numb_coef)
    }

    for (i in 1:numb_coef) {
      esti[i] <- round(esti[i], digits[i])
    }
  }
  est_info <- paste(esti,"(",p_star, ")", sep = "")

  # upper lower infos
  is_lower <- !is.null(mod$call$lower)
  is_upper <- !is.null(mod$call$upper)

  if (is_lower & !is_upper) {
    range_info <- paste("[≥", mod$call$lower,"]", sep = "")
    est_info <- paste(est_info, range_info)
  }
  if (is_lower & is_upper) {
    range_info <- paste("[", mod$call$lower,",", mod$call$upper,"]", sep = "")
    est_info <- paste(est_info, range_info)
  }
  if (!is_lower & is_upper) {
    range_info <- paste("[≤", mod$call$upper,"]", sep = "")
    est_info <- paste(est_info, range_info)
  }

  names(est_info) <- row.names(coeff)

  if (freq_subset) {
    # Sub Frequency
    freq_area <- "[50,5000]"
    if (!is.null(mod$weights)) {
      inklfreq <- ifelse(mod$weights[1:21] == 1, T,F)

      sub_freq <- freq[inklfreq]

      freq_area <- paste("[", min(sub_freq), ",",
                         max(sub_freq), "]", sep = "")
    }
    est_info <- paste(c(est_info, freq_area))
    names(est_info) <- c(row.names(coeff), "inc freq")
  }

  est_info
}

#' @return \code{NULL}
#'
#' @rdname fit_info_tab
#' @method fit_info_tab list
#' @exportS3Method fit_info_tab list
fit_info_tab.list <- function(x,...){
  itab <- lapply(
    x, fit_info_tab, ...
  )
  itab <- lapply(itab, data.frame)

  infonames <- unique(unlist(lapply(itab, row.names)))
  info_len <- length(infonames)
  list_len <- length(x)

  infomat <- matrix(NA, nrow = list_len, ncol = info_len)

  for (i in 1:info_len) {
    for (j in 1:list_len) {
      infomat[j,i] <- itab[[j]][infonames[i],]
    }
  }

  infodf <- data.frame(infomat)

  row.names(infodf) <- names(x)
  names(infodf) <- infonames

  infodf
}



# BEISPIEL (evtl SDM Anpassungen gamma laden)
#fitlist <- sb_fit_list

# fit_info_tab(fitlist, digits = c(3,2,0, 2))
#
# fit_info_tab(sb_fit_list$basic, digits = c(3,2,0, 2))
# fit_info_tab(sb_fit_list$OhneTP, digits = c(3,2,0, 2))
# fit_info_tab(sb_fit_list$fk4000, digits = c(3,2,0, 2))




