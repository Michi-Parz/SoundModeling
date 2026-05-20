
data_frame_round <- function(df, cols = 1:ncol(df), digits) {
  m <- length(cols)
  if (m != length(digits)) {
    stop("Hä?")
  }
  
  for (i in 1:m) {
    col_i <- cols[i]
    df[,col_i] <- round(df[,col_i], digits[i])
  }
  
  df
}
