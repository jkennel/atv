#' subset_depth
#'
#' @param x raster
#' @param top top value
#' @param bot bottom value
#' @param elev whether values are in elevation or depth
#'
#' @return subsetted raster
#'
#' @export
subset_depth <- function(x, top, bot, elev = FALSE) {

  if (elev) {
    ext <- raster::extent(raster::extent(x)@xmin,
                          raster::extent(x)@xmax, top, bot)
  } else {
    ext <- raster::extent(raster::extent(x)@xmin,
                          raster::extent(x)@xmax, -bot, -top)
  }

  x <- raster::crop(x, ext)

  return(x)
}
