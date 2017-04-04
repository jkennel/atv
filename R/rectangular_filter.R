#' rectangular_filter
#'
#' @param x matrix of data
#' @param dif difference between values and the moving quantile
#' @param k_h width of horizontal moving window;
#' @param k_v width of vertical moving window;
#' @param ... arguments to pass to runquantile
#'
#' @return matrix of values masked with NA
#'
#' @export
rectangular_filter <- function(x, dif = 4, k_h = 20, k_v = 4, ...){

  # vertical filtering
  x_smooth <- RcppRoll::roll_mean(x,
                                  k_v,
                                  na.rm = TRUE,
                                  fill = NA)

  # horizontal filtering with wrap around
  x_smooth <- t(x_smooth)
  x_smooth <- RcppRoll::roll_mean(as.numeric(x_smooth),
                                  k_h,
                                  na.rm = TRUE,
                                  fill = NA)
  x_smooth <- matrix(x_smooth, ncol = ncol(x), byrow = TRUE)

  x[abs(x_smooth - x) >= dif] <- NA_real_

  return(x)

}

# faster parallel version - requires roll package
# rectangular_filter2 <- function(x, dif = 4, k_h = 20, k_v = 4, ...){
#
#   # vertical filtering
#   x_smooth <- roll::roll_mean(x, width = k_v, min_obs = 1)
#   x_smooth <- rbind(x_smooth[-(1:(k_v %/% 2)),],
#                     matrix(NA_real_, ncol = ncol(x_smooth), nrow = k_v %/% 2))
#
#   # horizontal filtering with wrap around
#   x_smooth <- t(x_smooth)
#   dim(x_smooth) <- c(length(x), 1)
#   x_smooth <- roll::roll_mean(x_smooth, width = k_h, min_obs = 1)
#
#   # center the mean
#   x_smooth <- c(x_smooth[-(1:(k_h %/% 2))], rep(NA_real_, (k_h %/% 2)))
#
#   x_smooth <- matrix(x_smooth, byrow = TRUE, ncol = ncol(x))
#
#   x[abs(x_smooth - x) >= dif] <- NA_real_
#
#   return(x)
#
# }
#
# system.time(
#   aa2 <- rectangular_filter2(tt_mat)
# )
# system.time(
#   aa <- rectangular_filter(tt_mat)
# )
