#' @param x angle in radians
#'
#' @return angle in degrees
#' @examples
#' to_degree(2)
#'
to_degree <- function(x){
  x * 180/pi
}

#' @param x angle in degrees
#'
#' @return angle in radians
#' @examples
#' to_radian(180)
#'
to_radian <- function(x){
  x * pi/180
}
