
#' color_scale
#'
#' @param n number of colors
#'
#' @return set of colors (dark blue, dark red, yellow like wellCAD)
#'
#' @export
#'
#' @examples
#' color_scale(10)
color_scale <- function(n = 100) {
  grDevices::colorRampPalette(c('dark blue', 'dark red', 'yellow'))(n)
}
