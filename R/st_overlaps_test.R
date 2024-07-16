#' Function to test if \code{sf} objects overlap with one another. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf An \code{sf} object to test for overlaps. 
#' 
#' @param any Should \code{\link{any}} be use to test if some values are 
#' \code{TRUE}? 
#' 
#' @return Logical vector with the length of \code{sf} or \code{1}.
#' 
#' @export
st_overlaps_test <- function(sf, any = FALSE) {
  
  # Test for overlaps
  overlaps <- sf::st_overlaps(sf, sparse = TRUE)
  
  # To a logical vector
  overlaps <- purrr::map_lgl(overlaps, ~length(.) != 0L)
  
  # To a vector with the length of one
  if (any) {
    overlaps <- any(overlaps)
  }
  
  return(overlaps)
  
}
