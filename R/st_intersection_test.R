#' Function to test if \code{sf} objects intersect with one another. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x,y \code{sf} objects to test. \code{st_intersection_test} will test 
#' if the features in \code{x} intersect with any feature in \code{y}. 
#' 
#' @return Logical vector with the length of \code{x}.
#' 
#' @export
st_intersection_test <- function(x, y) {
  
  # Test for intersections, this will return a sparse index
  intersects <- sf::st_intersects(x, y, sparse = TRUE)
  
  # To a logical vector
  intersects <- purrr::map_lgl(intersects, ~length(.) != 0L)
  
  return(intersects)
  
}
