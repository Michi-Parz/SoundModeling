#' Converts the volume of a wall into a side length
#'
#' @param volume m³
#' @param height m
#' @param side_ration short_side/long_side
#' @param output choose return: longer, shorter, both, perimeter


volume_to_side_length <- function(volume, height = 2.5, side_ratio = 0.8,
                                  output = c("longer", "shorter",
                                             "both", "perimeter")){
  if (side_ratio <= 0) {
    stop("An aspect ratio must be positive!")
  }
  if (height == 0) {
    stop("If the height is zero, then the volume is also zero and the whole calculation makes no sense!")
  }
  if (height < 0) {
    height <- -height
    warning("Negative height? Does this mean the depth? I'll pretending it does.")
  }
  if (min(volume)<0) {
    warning("A negative volume does not really make sense.")
    volume[volume<0] <- NA
  }

  output <- match.arg(output)

  s <- volume/height

  if (side_ratio > 1){
    side_ratio <- 1/side_ratio
  }

  if (output == "longer")
    return(
      sqrt(s/side_ratio)
    )
  if (output == "shorter")
    return(
      sqrt(s*side_ratio)
    )
  if (output == "both")
    return(
      data.frame("l1" = sqrt(s/side_ratio),
                 "l2" = sqrt(s*side_ratio))
    )
  if (output == "perimeter")
    2*(sqrt(s*side_ratio) + sqrt(s/side_ratio))

}
