#' read_las
#'
#' @param file_name name of file to read
#' @param na_value value for NAs
#'
#' @return
#' @export
#'
#' @examples
read_las <- function(file_name, na_value = -99999){

  # ASCII FILE TO READ
  to.read = file(file_name, 'rb')

  # READ FILES
  las = readLines(to.read, n = 200000)

  # FIND THE START AND END OF THE HEADER FILE
  hs <- grep('~LOG_DEFINITION', las)
  he <- grep('~LOG_DATA', las)

  # GET THE HEADER VALUES
  rem <- regexec("[0-9]+\\.[0-9]", las[hs:he])
  ang <- as.numeric(regmatches(las[hs:he], rem))
  ang <- ang[!is.na(ang)]

  # CREATE THE MATRIX OF VALUES
  val <- matrix(as.numeric(unlist(strsplit(las[(he+1):NROW(las)], ','))),
                ncol=length(ang) + 1, byrow = TRUE)

  # SET THE NA VALUE
  val[which(val == na_value)] <- NA_real_

  # SET THE COLUMN NAMES
  colnames(val) <- c('Depth', paste0('Ang', ang))

  close(to.read)
  return(val)
}
