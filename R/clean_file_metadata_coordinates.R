#' Function to clean and convert non-decimal degrees, usually from photos' 
#' metadata strings to to decimal degrees.
#' 
#' @author Stuart K. Grange
#' 
#' @param x Vector containing coordinates in this format: 
#' \code{53 deg 37\' 56.49\" N}. 
#' 
#' @param round Number of decimal points to round the coordinates to. 
#' 
#' @return Numeric vector. 
#' 
#' @examples
#' 
#' # An example latitude
#' clean_file_metadata_coordinates('53 deg 37\' 56.49\" N')
#' 
#' @export
clean_file_metadata_coordinates <- function(x, round = 6) {
  
  # Split into pieces
  x <- stringr::str_split_fixed(x, " ", 5)
  x[, 3] <- stringr::str_replace(x[, 3], "'", "")
  x[, 4] <- stringr::str_replace(x[, 4], '"', "")
  
  # To decimal degrees
  coordinate <- sspatialr::dms_to_decimal(
    as.numeric(x[, 1]), 
    as.numeric(x[, 3]), 
    as.numeric(x[, 4])
  )
  
  # Negate if necessary
  coordinate <- if_else(x[, 5] %in% c("W", "S"), coordinate * -1, coordinate)
  
  # Round if desired
  if (!is.na(round)) {
    coordinate <- round(coordinate, round)
  }
  
  return(coordinate)
  
}
