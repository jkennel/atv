#' row_filter
#'
#' @param x matrix of data
#' @param dif difference between values and the moving quantile
#' @param k width of moving window; must be an integer between one and n
#' @param probs numeric vector of probabilities with values in [0,1] range used by runquantile. For example Probs=c(0,0.5,1) would be equivalent to running runmin, runmed and runmax. Same as probs in quantile function.
#' @param ... arguments to pass to runquantile
#'
#' @return
#' @export
#'
#' @examples
row_filter <- function(x, dif = 4, k = 21, probs = 0.5, ...){

  x_vec <- as.vector(x)
  wh <- caTools::runquantile(x_vec, k = k, probs = probs, ...)
  wh <- (abs(wh) - x_vec) >= dif
  x[wh] <- NA_real_
  return(x)

}
