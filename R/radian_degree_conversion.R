#'
#' to_degree
#'
#' @param x angle in radians
#'
#' @return angle in degrees
#'
#' @export
#'
#' @examples
#' to_degree(2)
to_degree <- function(x){
  x * 180/pi
}

#'
#' to_radian
#'
#' @param x angle in degrees
#'
#' @return angle in radians
#'
#' @export
#'
#' @examples
#' to_radian(180)
#'
to_radian <- function(x){
  x * pi/180
}
