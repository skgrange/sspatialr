#' Function to use a \strong{sf} object to erase pieces of another \strong{sf}
#' object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf_x  First \strong{sf} object. 
#' 
#' @param sf_y  Second \strong{sf} object. 
#' 
#' @return An \strong{sf}. 
#' 
#' @export
st_erase <- function(sf_x, sf_y) {
  # Unite the second polyon and use to erase pieces in the first object
  st_difference(sf_x, st_union(sf_y))
}
