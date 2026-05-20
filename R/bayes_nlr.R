#' Sample from a bayesian Nonlinear Regression
#'
#' @param formula a model formula including variables and parameters.
#' @param data a data frame containing data
#' @param start a named list of starting values
#' @param log_poster a function which is propotional to the logarithmic posteriori density
#' @param hyperpara a named list of hyperparameters. See details!
#' @param cov if TRUE the covariance is estimated using the inverse Wishard distribution
#' if FALSE only the variances are estimated via inverse gamma.SEE details!
#' @param suggest_sample a function to sample from a suggested distribution
#' @param n_iter number of iterations
#'
#'
#' @details
#' if cov == TRUE "hyperpara" must contain the covariance matrix \eqn{V} and \eqn{k} for \eqn{IW(kV,k)} prior.
#' if cov == FALSE "hyperpara" must contain shape and rate
#'
#'
#'
#' @return posteriori samples
#'
#' @examples
#' exa_func <- function(t, a,b,c,d){
#'   a - b/2 + pmin(abs(a)*t, c- abs(b)*t, d)
#' }
#'
#' test_df <- data.frame(t = rep(1:20, 24))
#'
#' test_df$y <- exa_func(test_df$t, 3,2,40,15) + rnorm(20*24, sd = 1)
#'
#'
#' bayes_nlr(y~exa_func(t, a,b,c,d),
#'    data = test_df,
#'    start = c("a"= 2,"b"=2, c=40, d = 15),
#'   log_poster = log_poster_gauss,
#'   hyperpara = list("a"=0,"b"=10, "V" = diag(20), "k" = 1),
#'   n_iter = 10
#'   )
#'


bayes_nlr <- function(formula, data, start,
                      log_poster = log_poster_gauss,
                      hyperpara,
                      cov = T,
                      suggest_sample = \(x){rnorm(length(x), x, sd =1)},
                      n_iter = 1000,
                      verbose = T) {
  # Write the names of all variables in one vector
  # Variables are here all fitparameter and covariables
  formula <- as.formula(formula)
  formula_wo_target <- formula
  formula_wo_target[2L] <- 0
  variables <- all.vars(formula_wo_target)


  # Find all covariable values from data and write these in one list
  data_names <- names(data)

  data_var_pos <- !is.na(match(data_names, variables))
  data_var_names <- data_names[data_var_pos]

  data_values <- data[,data_var_names]

  if (length(data_var_names) == 1) {
    data_values <- list(data_values)
    names(data_values) <- data_var_names
  }
  if (length(data_var_names) > 1) {
    data_values <- as.list(data_values)
  }

  # Combine the covariables and the start values to one list
  if (!is.list(start))
    start <- as.list(start)

  variable_values <- c(data_values, start)


  # Create result lists for fitparameter
  result <- list()
  result[[1]] <- unlist(start)
  accept <- c()
  accept[1] <- FALSE


  # Create result lists for covariance
  sigma_list <- list()
  if (cov) {
    hyper_V <- hyperpara$V
    hyper_k <- hyperpara$k
    J <- nrow(hyper_V)
    sigma_list[[1]] <- hyper_k * hyper_V
  }
  if (!cov) {
    hyper_shape <- hyperpara$shape
    hyper_rate <- hyperpara$rate
    J <- length(hyper_shape)
    if (J != length(hyper_rate)) {
      stop("hyperpara$shape and hyperpara$rate must be of length J")
    }
    sigma_list[[1]] <- diag(hyper_rate/hyper_shape)
  }



  # Variable names
  para_names <- names(start)

  # Target variable
  target_var <- data[, all.vars(formula)[1]]

  # Dimensions
  n_obs <- length(target_var)
  I <- n_obs/J

  if (I %% 1 != 0) {
    stop("Dimension of Sigma and number of observations do not fit!")
  }

  if (I %% 1)
    stop("There is something wrong with the dimension")


  for (i in 2:n_iter) {

    # Metropolis step
    suggest <- suggest_sample(result[[i-1]])

    var_values_sugg <- var_values_prev <- variable_values
    var_values_sugg[para_names] <- suggest
    var_values_prev[para_names] <- result[[i-1]]


    log_post_sugg <- log_poster(var_values_sugg, target_var,
                                Sigma = sigma_list[[i-1]],
                                para_names, hyperpara, formu = formula)

    log_post_last <- log_poster(var_values_prev, target_var,
                                Sigma = sigma_list[[i-1]],
                                para_names, hyperpara, formu = formula)

    l_post_diff <- log_post_sugg - log_post_last

    alpha <- min(l_post_diff, 0)
    u <- log(runif(1))

    if (alpha >= u) {
      accept[i] <- TRUE
      names(suggest) <- para_names
      result[[i]] <- suggest
    }
    if (alpha < u) {
      accept[i] <- FALSE
      result[[i]] <- result[[i-1]]
    }

    # Covariance step
    variable_values[para_names] <- result[[i]]
    residuals <- target_var - eval(formula[[3L]], envir = variable_values)
    S <- residuals %*% t(residuals)

    S <- lapply(
      seq_len(I),
      \(i, S){
        subs <- 1:J + J*(i-1)
        S[subs, subs]
      }, S = S
    )

    S <- Reduce(`+`, S)

    if (cov) {
      sigma_list[[i]] <- MCMCpack::riwish(hyper_k + I, S + hyper_k*hyper_V)
    }
    if (!cov) {
      sigma_list[[i]] <- 1/rgamma(
        J, (hyper_shape + I/2), (hyper_rate+diag(S)/2)
        )
      sigma_list[[i]] <- diag(sigma_list[[i]])
    }



    if (verbose)
      cat("Step: ",i, "| Accepted: ",accept[i], "\n")

  }

  result_df <- lapply(result, \(x){
    as.data.frame(t(x))
  })

  result_df <- Reduce(rbind, result_df)


  list(
    "Fitparameter" = result_df,
    "Covariance" = sigma_list,
    "Accepted" = accept
  )
}






















