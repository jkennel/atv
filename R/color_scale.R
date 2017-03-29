#' color_scale
#'
#' @param n number of colors
#'
#' @return
#' @export
#'
#' @examples
color_scale <- function(n = 100) {
  colorRampPalette(c('dark blue', 'dark red', 'yellow'))(n)
}
