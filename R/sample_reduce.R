#' @title Reduce (Metropolis) samples
#' @param sample a list or data.frame of samples
#' @param accepted a logical vector
#' @param len length finaly accepted samples
#' @param type if "burn" then the first len will be removed and after that the accepted values will be used
#' and for type "reverse" the accepted ones are selected first and then the last of these are selected.
#' @rdname sample_reduce
#' @export sample_reduce
sample_reduce <- function (sample, ...) {
  UseMethod("sample_reduce")
}

#'
#' @rdname sample_reduce
#' @method sample_reduce default
#' @exportS3Method sample_reduce default
sample_reduce.default <- function(sample, accepted,
                                  len, type = c("burn", "reverse")){
  type <- match.arg(type)
  if (type == "reverse")
    return(tail(sample[accepted], n = len))

  remove <- -seq_len(len)
  sample[remove][accepted[remove]]
}
#'
#' @rdname sample_reduce
#' @method sample_reduce data.frame
#' @exportS3Method sample_reduce data.frame
sample_reduce.data.frame <- function(sample, accepted,
                                     len, type = c("burn", "reverse")){
  type <- match.arg(type)
  if (type == "reverse")
    return(tail(sample[accepted,], n = len))

  remove <- -seq_len(len)
  sample[remove,][accepted[remove],]

}

