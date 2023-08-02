#' Function to get the class of a \strong{sf} spatial object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param sf A \strong{sf} spatial object. 
#' 
#' @param as_lower Should the return be lower-case? 
#' 
#' @return Character vector with length of \code{1}. 
#' 
#' @export
sf_class <- function(sf, as_lower = TRUE) {
  
  # Get unique geometry classes as a character vector
  x <- sf %>% 
    sf::st_geometry_type() %>% 
    unique() %>% 
    as.character()
  
  # Make lower case
  if (as_lower) {
    x <- stringr::str_to_lower(x)
  }
  
  return(x)
  
}
