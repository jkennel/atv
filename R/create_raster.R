#' create_raster
#'
#' @param x the matrix from a .las file
#' @param elev are values in elevation
#'
#' @return raster
#'
#' @export
create_raster <- function(x, elev = FALSE){
  if (elev) {
    ymx <- max(x[,1], na.rm = TRUE)
    ymn <- min(x[,1], na.rm = TRUE)
  } else {
    ymn <- -max(x[,1], na.rm = TRUE)
    ymx  <- -min(x[,1], na.rm = TRUE)
  }
  nc <- NCOL(x[, -1])
  raster::raster(x[, -1],
                 ymn = ymn,
                 ymx = ymx,
                 xmn = 0,
                 xmx = 360 / nc * (nc - 1))
}
