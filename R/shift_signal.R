#' shift_signal
#'
#' @param x matrix
#' @param phase the amount to shift
#' @param direction the direction to shift
#'
#' @return
#' @export
#'
#' @examples
shift_signal <- function(x, phase, direction = 1){

  len   <- NCOL(x)
  shift <- direction * round(phase / (2 * pi) * len)

  for (i in 1:NROW(x)) {
    if (is.na(shift[i])) {
    } else if (shift[i] < 0) {
      x[i,] <- x[i, c((len + shift[i] + 1):len, 1:(len + shift[i]))]
    } else if (shift[i] > 0) {
      x[i,] <- x[i, c((shift[i] + 1):len, 1:(shift[i]))]
    } else {
    }
  }

  return(x)
}
