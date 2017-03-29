#' subset_depth
#'
#' @param x
#' @param top
#' @param bot
#' @param elev
#'
#' @return
#' @export
#'
#' @examples
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
